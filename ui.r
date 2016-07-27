library(shiny)
library(base)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_", "n_obs"), "Number of observations (in Years):", min = 0, max = 40, value = 20),
             sliderInput(paste0(prefix, "_", "Current_Interest_Rate"), "Current Interest Rate :", min = -1.00, max = 10.00, value = .30, step = .05, pre = "", post = "%", sep = ","),
             sliderInput(paste0(prefix, "_", "annual_liquidity_premium"), "Liquidity Premium (in %):", min = 0.0, max = 6.0, value = .10, step = 0.01),
             sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual Rate volatility (in %):", min = 0.0, max = 25.0, value = 7.0, step = 0.1)
      ),
      column(6,
             sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 10, value = 2.5, step = 0.1),
             sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 1.5, step = 0.05),
             sliderInput(paste0(prefix, "_", "Time_to_Maturity"), "Time to Maturity:", min = 0, max = 100, value = 5, step = 1, pre = "", post = "yrs", sep = ","),
             sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 2000, value = 200)
      )
    ),
    p(actionButton(paste0(prefix, "_", "recalc"),
                   "Re-run simulation", icon("random")
    ))
  )
}

# Define UI for application that plots random distributions
shinyUI(fluidPage(theme="simplex.min.css",
                  tags$style(type="text/css",
                             "label {font-size: 12px;}",
                             ".recalculating {opacity: 1.0;}"
                  ),
                  
                  # Application title
                  tags$h2("Simulating Interest Rates"),
                  p("An adaptation of the",
                    tags$a(href="http://glimmer.rstudio.com/systematicin/retirement.withdrawal/", "retirement app"),
                    "from",
                    tags$a(href="http://systematicinvestor.wordpress.com/", "Systematic Investor"),
                    "to demonstrate the use of Shiny's new grid options."),
                  hr(),
                  
                  fluidRow(
                    column(6, tags$h3("Scenario A")),
                    column(6, tags$h3("Scenario B"))
                  ),
                  fluidRow(
                    column(6, renderInputs("a")),
                    column(6, renderInputs("b"))
                  ),
                  fluidRow(
                    column(6,
                           plotOutput("a_distPlot", height = "600px")
                    ),
                    column(6,
                           plotOutput("b_distPlot", height = "600px")
                    )
                  )
))