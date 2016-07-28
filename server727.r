library(shiny)
library(knitr)
library(rmarkdown)
library(Quandl)
library(NMOF)
library(termstrc) #nlminb
#library(ycinterextra)
#library(base64)
#library(base)
Quandl("USTREASURY/YIELD", api_key="joNszWzKU-LCs9WWmWZp")
YC <- Quandl("USTREASURY/YIELD", api_key="joNszWzKU-LCs9WWmWZp")
YC100days <- first(YC, 100)
YC1day <- first(YC, 1)
YCday <- YC1day[2:12]
YC100days <- first(YC, 100)
YC100 <- YC100days[2:12]
YC100mean <- colMeans(YC100)
paramNames <- c("Current_Interest_Rate", "annual_liquidity_premium", "annual_ret_std_dev",
                "annual_inflation", "annual_inf_std_dev", "Time_to_Maturity", "n_obs",
                "n_sim")

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
shinyServer(function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  navA <- reactive(do.call(simulate_nav, getParams("a")))
  navB <- reactive(do.call(simulate_nav, getParams("b")))
  
  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$a_distPlot <- renderPlot({
    plot_nav(navA())
  })
  output$b_distPlot <- renderPlot({
    plot_nav(navB())
  })
  
})

simulate_nav <- function(Current_Interest_Rate = .3, annual_liquidity_premium = .10,
                         annual_ret_std_dev = 7.0, annual_inflation = 2.5,
                         annual_inf_std_dev = 1.5, Time_to_Maturity = 5,
                         n_obs = 20, n_sim = 200) {
  #-------------------------------------
  # Inputs
  #-------------------------------------
  
  # Current Interest Rate(Initial capital used to be start capital)
  Current.Interest.Rate = Current_Interest_Rate
  
  # Investment Rate Characteristics
  annual.liquidity.premium = annual_liquidity_premium / 100
  annual.ret.std.dev = annual_ret_std_dev / 100
  
  # Inflation
  annual.inflation = annual_inflation / 100
  annual.inf.std.dev = annual_inf_std_dev / 100
  
  # Time to Maturity
  Time.to.Maturity = Time_to_Maturity
  
  # Number of observations (in Years)
  n.obs = n_obs
  
  # Number of simulations
  n.sim = n_sim
  
  
  #-------------------------------------
  # Simulation
  #-------------------------------------
  
  # number of months to simulate
  n.obs = 12 * n.obs
  
  # monthly Rate and Inflation assumptions
  monthly.liquidity.premium = annual.liquidity.premium / 12
  #monthly.liquidity.premium = annual.liquidity.premium / 12
  monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)
  #monthly.Current.Interest.Rate = Annual.Current.Interest.Rate / 12
  
  monthly.inflation = annual.inflation / 12
  monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)
  
  # simulate Returns
  monthly.liquidity.premium = matrix(0, n.obs, n.sim)
  monthly.inflation.returns = matrix(0, n.obs, n.sim)
  
  monthly.liquidity.premium[] = rnorm(n.obs * n.sim, mean = monthly.liquidity.premium, sd = monthly.ret.std.dev)
  monthly.inflation.returns[] = rnorm(n.obs * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)
  
  # simulate Rates (old Withdrawals)
  nav = matrix(Current.Interest.Rate, n.obs + 1, n.sim)
  for (j in 1:n.obs) {
    nav[j + 1, ] = (nav[j, ] + monthly.liquidity.premium[j, ] + monthly.inflation.returns[j, ])
  }
  
  # once nav is below 0 => run out of money
  nav[ nav < 0 ] = NA
  
  # convert to millions
  nav = nav / 1
  
  return(nav)
}

plot_nav <- function(nav) {
  
  layout(matrix(c(1,2,1,3),2,2))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  # plot all scenarios
  matplot(nav,
          type = 'l', lwd = .25, lty = 1, col = 1:5,
          xlab = 'Months', ylab = 'Interest Rate',
          main = 'Interest Rates')
  
  # plot current yield curve (old plot % of scenarios that are still paying)
  p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)
  
  #plot(100 * p.alive, las = .5, xlab = 'Months', ylab = 'Percentage Paying',
       #main = 'Percentage of Paying Scenarios', ylim=c(0,100))
  grid()
  
  
  last.period = nrow(nav)
  
  #YC1day <- first(YC, 1)
  #YC1day
  #YCday <- YC1day[2:12]
  #YCday
  Mats <-c(.08333, .25, .5, 1, 2, 3, 5, 7, 10, 20, 30)
  plot(Mats, YCday, xlab = "Maturity", ylab = "Interest Rate", 
       main = 'Current Yield Curve vs. Avg. Past 100 trading day Yield Curve', 
       type='b')
    lines(Mats, y=YC100mean, type='b',col="dark blue")
  # plot average yield curve past 100 trading days (old plot distribution of final wealth)
  final.nav = nav[last.period, ]
  final.nav = final.nav[!is.na(final.nav)]
  
  if(length(final.nav) ==  0) return()
  
  #plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
       #main = paste0('Distribution of Final Capital\n', 100 * p.alive[last.period], '% are still paying'))
  plot(Mats, YC100mean-YCday, xlab = "Maturity", ylab = "Interest Rate", main = '100 Day Avg. Yield Curve - Current Yield Curve', type = 'b', col='dark green')

  grid()
  
  #plot(Mats, YCday-YC100mean, xlab = "Maturity", ylab = "Interest Rate", main = 'Average Yield Curve Past 100 Trading Days', type = 'b')
  #grid()
}

