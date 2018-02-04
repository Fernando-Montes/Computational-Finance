library(zoo)
library(PerformanceAnalytics)
library(tseries)
library(xts)

# NOTE
# useful function to obtain returns after 1 year on a monthly basis 
# apply.rolling(Return.calculate(temp), width = 12, FUN = "mean.geometric")

# Returns a data.frame with the performance of the SP500
sp500 <- as.xts(get.hist.quote(instrument="^GSPC", 
                          quote="AdjClose",provider="yahoo", start = as.Date("1985-12-13"),
                          compression="d", quiet=TRUE))
par(mfrow = c(1, 1))
plot.zoo(sp500)
# Convert the daily frequency of sp500 to monthly frequency
sp500_monthly <- to.monthly(sp500,indexAt = 'endof')
# Print the first six rows of sp500_monthly
head(sp500_monthly)
# Create sp500_returns using Return.calculate using the closing prices
sp500_returns <- Return.calculate(sp500_monthly$sp500.Close)
# Time series plot
plot.zoo(sp500_returns)
# Produce the year x month table
table.CalendarReturns(sp500_returns)

# -----------------------------------------------
# Monthly mean and volatility
sp500_returns <- sp500_returns[-1,]
# Compute the mean monthly returns
mean(sp500_returns)
# Compute the geometric mean of monthly returns
mean.geometric(sp500_returns)
# Compute the standard deviation
(sum((sp500_returns - mean(sp500_returns))*(sp500_returns - mean(sp500_returns)))/(length(sp500_returns)-1))^(1/2)
sd(sp500_returns)

# -----------------------------------------------
# (Annualized) Sharpe Ratio
rf <- read.csv("~/Dropbox/Courses/PortfolioAnalysisInR/MonthlyT-Bill.csv", header = FALSE)
rf <- as.xts(rf[,-1], order.by=as.Date.character(rf[,1],"%m/%d/%y"))
# Compute the annualized risk free rate
annualized_rf <- (1+rf)^12-1  # Annualized rate on a monthly basis!!!
# Plot the annualized risk free rate
plot.zoo(annualized_rf)
# Compute the series of excess portfolio returns 
sp500_excess <- sp500_returns - rf
# Compare the mean
mean(sp500_returns)
mean(sp500_excess)
# Compute the Sharpe ratio
sp500_sharpe <- mean(sp500_excess)/sd(sp500_returns)

# -----------------------------------------------
# Annualized mean and volability
# Compute the annualized mean
Return.annualized(sp500_returns)
# Compute the annualized standard deviation
StdDev.annualized(sp500_returns)
# Compute the annualized Sharpe ratio
ann_sharpe <- Return.annualized(sp500_returns)/StdDev.annualized(sp500_returns)
# Compute of the above at once
table.AnnualizedReturns(sp500_returns)

# -----------------------------------------------
# Time-variation in portfolio performance
# Rolling annualized mean and volatility
# Calculate the mean, volatility, and sharpe ratio of sp500_returns
returns_ann <- Return.annualized(sp500_returns)
sd_ann <- StdDev.annualized(sp500_returns)
sharpe_ann <- SharpeRatio.annualized(sp500_returns, Rf = rf)
# Plotting the 12-month rolling annualized mean
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "Return.annualized")
abline(h = returns_ann)
# Plotting the 12-month rolling annualized standard deviation
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "StdDev.annualized")
abline(h = sd_ann)
# Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "SharpeRatio.annualized",Rf = rf)
abline(h = sharpe_ann)
# All three plots together
charts.RollingPerformance(R = sp500_returns, width = 12, Rf = rf)

# -----------------------------------------------
# Subperiod performance analysis and the function window
# Fill in window for 2008
sp500_2008 <- window(sp500_returns, start = "2008-01-01", end = "2008-12-31")
# Create window for 2014
sp500_2014 <- window(sp500_returns, start = "2014-01-01", end = "2014-12-31")
# Plotting settings
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(sp500_2008) <- "sp500_2008"
names(sp500_2014) <- "sp500_2014"
# Plot histogram of 2008
chart.Histogram(sp500_2008, methods = c("add.density", "add.normal"))
# Plot histogram of 2014
chart.Histogram(sp500_2014, methods = c("add.density", "add.normal"))

# -----------------------------------------------
# Non-normality of the return distribution
# Detecting non-normality using skewness and kurtosis
sp500_daily <- Return.calculate(sp500)
sp500_monthly <- sp500_returns
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
chart.Histogram(sp500_daily, methods = c("add.density", "add.normal"), xlim= c(-0.05, 0.05))
chart.Histogram(sp500_monthly, methods = c("add.density", "add.normal"))
#  Compute the skewness 
skewness(sp500_daily)
skewness(sp500_monthly)
# Compute the excess kurtosis
kurtosis(sp500_daily)
kurtosis(sp500_monthly)

# -----------------------------------------------
# Downside risk measures
# Calculate the SemiDeviation
SemiDeviation(sp500_monthly)
# Calculate the value at risk
VaR(sp500_monthly, p=0.025)
VaR(sp500_monthly, p=0.05)
# Calculate the expected shortfall
ES(sp500_monthly, p=0.025)
ES(sp500_monthly, p=0.05)

# -----------------------------------------------
# Drawdowns due to buying high, selling low
plot.zoo(sp500)
plot.zoo(sp500_monthly)
# Table of drawdowns
table.Drawdowns(sp500_monthly)
# Plot of drawdowns
chart.Drawdown(sp500_monthly)