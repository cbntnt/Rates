# New Shiny Page
require(ycinterextra)
# Svensson interpolation
ycSV <- ycinter(yM = txZC, matsin = u, matsout = t, 
                method = "SV", typeres = "rates")
# Yield to maturities * Pull in yield curve data and make rnorm function
txZC <- c(0.01422, 0.01309, 0.0138, 0.01549, 0.01747, 
          0.0194, 0.02104, 0.02236, 0.02348, 0.02446, 0.02535, 
          0.02614, 0.02679, 0.02727, 0.0276, 0.02779, 0.02787, 
          0.02786, 0.02776, 0.02762, 0.02745, 0.02727, 0.02707,
          0.02686, 0.02663, 0.0264, 0.02618, 0.02597, 0.02578, 
          0.02563)
#tyZC <-rnorm(ZCB_US)
#tzZC <-c(tyZC)

# Observed maturities
u <- 1:30

# Output maturities
t <- seq(from = 1, to = 30, by = 0.5)

# Svensson interpolation
ycSV <- ycinter(yM = txZC, matsin = u, matsout = t, 
                method = "SV", typeres = "rates")

# Smith-Wilson interpolation
ycSW <- ycinter(yM = txZC, matsin = u, matsout = t, 
                method = "SW", typeres = "rates")
ycplot(ycSV)

Quandl("USTREASURY/YIELD", api_key="joNszWzKU-LCs9WWmWZp")
YC <- Quandl("USTREASURY/YIELD", api_key="joNszWzKU-LCs9WWmWZp")

YC100days <- first(YC, 100)
YC1day <- first(YC, 1)
YC1day
YCday <- YC1day[2:12]
YCday
Mats <-c(.08333, .25, .5, 1, 2, 3, 5, 7, 10, 20, 30)
plot(Mats, YCday, xlab = "Maturity", ylab = "Interest Rate")

#Average Yield Curve past 100 trading days
YC100days <- first(YC, 100)
YC100 <- YC100days[2:12]
YC100mean <- colMeans(YC100)
plot(Mats, YC100mean, xlab = "Maturity", ylab = "Interest Rate")

# param a vector. For NS: ??1, ??2, ??3, ??. For NSS: a vector: ??1, ??2, ??3, ??4, ??1, ??2.
# tm a vector of maturities

tm <- c(c(1, 3, 6, 9) / 12, 1:10) ## in years
param <- c(6, 3, 8, 1)
yM <- NS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")
#NS->1 hump and NSS->2 humps(B1 = lon-run level (30yr), B2 = Steepness weighted 
#by a function of time to maturity only felt at shortend of curve, B3 = Curvature adds hump to curve), 
#(Lamda affects the weight function for B2 and B3 and determines position of hump)
#Least Squares for B values, Constraints B1>0, B1+B2>0
#NS-> B1,B2,B3, Lamda
#NSS-> B1,B2,B3, B4 Lamda1, Lamda2
#We set box constraints as follows: 0 < ??1 < 15, ???15 < ??2 < 30, ???30 < ??3 < 30,
#???30 < ??4 < 30, 0 < ??1 < 30, 0 < ??2 < 30
#: param, which is a candidate solution (a numeric vector), and the list data,
#which holds all other variables. It returns the maximum absolute difference between a vector of
#observed ('market') yields yM, and the model's yields for parameters param.
#B1 - Level of yields could be the average yield, B2 - steepness can be measured by the yield 
#difference between long-dated and short-dated bonds. 
#Diebold and Li (2006) for instance suggest to define B1 the level as the 10yr
#B2 Steepness 10yr - 3mo
#B3 Curvature 2* 2yr - 3mo - 10yr

param <- c(6, 3, 5, -5, 1, 3)
yM <- NSS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")
#German Bund data 9/2009
param <- c(2.05, -1.82, -2.03, 8.25, .87, 14.38)
yM <- NSS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")

#US Yield Curve (10yrs)
tm <- c(c(1, 3, 6, 9) / 12, 1:10) ## in years
param <- c(1.58, -1.26, -.46,.134, 2.5,30)
yM <- NSS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")

#US Yield Curve (30yrs)
#10yr
tyr <-first(YC1day,1)
ctyr <- c(tyr[,10])
threemo <- c(tyr[,3])
twoyr<- c(tyr[,6])
threemo
ctyr
diffthreemoctyr<- -(ctyr-threemo)
diffthreemoctyr
twotimestwoyr <- ((2*twoyr)-threemo-ctyr)
twotimestwoyr
thirtyyr <- c(tyr[,12])

tm <- c(c(1, 3, 6, 9) / 12, 1:30) ## in years
param <- c(ctyr, diffthreemoctyr, twotimestwoyr,2, 2.5,5.5)
yM <- NSS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")
tm <- c(c(1, 3, 6, 9) / 12, 1:30) ## in years
param <- c(thirtyyr, diffthreemoctyr, -1.17,2, 2.5,5.5)
yM <- NSS(param, tm)
plot(tm, yM, xlab = "maturity in years",
     ylab = "yield in percent")