# R code for GARCH(1,1)/Arch(0,1) estimation

#install.packages("tseries")
library(tseries)

data<-read.csv("~/Documents/GitHub/codeproject/oilprice.csv")
# Get the variables
X <- rev(data$ClosingPrice)

n <- length(X)

r <- ((X[2:n] - X[1:(n-1)])/X[1:(n-1)])
# Arch and Garch Meathods

summary(r)
plot(r)
arch.r= garch(r,order=c(0,1))
summary(arch.r)
Garch.r = garch(r,order=c(1,1))
summary(Garch.r)
qqnorm(r, pch = 1, frame = FALSE)
qqline(r, col = "steelblue", lwd = 2)
#LOG-normal distribution.

logr = log(sqrt(r^2))
summary(logr)
plot(logr)
arch.logr= garch(logr,order=c(0,1))
summary(arch.logr)
Garch.logr = garch(logr,order=c(1,1))
summary(Garch.logr)

qqnorm(logr, pch = 1, frame = FALSE)
qqline(logr, col = "steelblue", lwd = 2)