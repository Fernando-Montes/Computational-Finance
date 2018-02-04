# Getting financial data
# Load relevant packages
library(PerformanceAnalytics)
library(zoo)
library(tseries)

# Get the monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo! using the 
# tseries function get.hist.quote(). Set the sample to Jan 1998 through Dec 2009.

# Get the adjusted closing prices from Yahoo!
VBLTX_prices <- get.hist.quote(instrument="vbltx", start="1998-01-01",
                               quote="AdjClose",provider="yahoo", origin="1970-01-01",
                               compression="m", retclass="zoo", quiet=TRUE)
FMAGX_prices <- get.hist.quote(instrument="fmagx", start="1998-01-01",
                               quote="AdjClose",provider="yahoo", origin="1970-01-01",
                               compression="m", retclass="zoo", quiet=TRUE)

SBUX_prices <- get.hist.quote(instrument="sbux", start="1998-01-01",
                              quote="AdjClose",provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo", quiet=TRUE)
  
# Change the class of the time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package  
index(VBLTX_prices) <- as.yearmon(index(VBLTX_prices))
index(FMAGX_prices) <- as.yearmon(index(FMAGX_prices))
index(SBUX_prices)  <- as.yearmon(index(SBUX_prices))

# Inspect your data
start(SBUX_prices)
end(SBUX_prices)

# Calculating the returns
# Create merged price data
all_prices = merge(VBLTX_prices, FMAGX_prices, SBUX_prices)
# Rename columns
colnames(all_prices) <- c("VBLTX", "FMAGX", "SBUX")

# Calculate cc returns as difference in log prices
all_returns = diff(log(all_prices), lag = 1)
  
# Look at the return data
start(all_returns)
end(all_returns)
colnames(all_returns) 
head(all_returns)

# Plotting financial data with PerformanceAnalytics
# Plot returns after using the PerformanceAnalytics function chart.TimeSeries().
# This function creates a slightly nicer looking plot than plot.zoo()
chart.TimeSeries(all_returns, legend.loc="bottom", main=" ") 

# The previous charts are a bit hard to read. The PerformanceAnalytics function
# chart.Bar makes it easier to compare the returns of different assets on the 
# same plot
chart.Bar(all_returns, legend.loc="bottom", main=" ")

# Cumulative return plot - must use simple returns (!) and not cc returns for this
# Use PerformanceAnalytics function chart.CumReturns()
simple_returns <- diff(all_prices, lag = 1)/lag.xts(all_prices, k = 1)
chart.CumReturns(simple_returns, wealth.index = TRUE, main = "Future Value of $1 invested",
                 colorset = c("blue","red", "orange"), legend.loc = "topleft")

# Create graphical summary for a return series
# Create matrix with returns:
# strip off the index/time attributes and return only the observations
return_matrix <- coredata(all_returns);

# Generate four panel plots
par(mfrow=c(2,2))
# Generating histogram of the cc returns
hist(return_matrix[,"VBLTX"], main="VBLTX monthly returns",
     xlab="VBLTX", probability=T, col="slateblue1")
# Generating boxplot of the cc returns
boxplot(return_matrix[,"VBLTX"], outchar=T, main="Boxplot", col="slateblue1")
# Generating density plot of the cc returns
plot(density(return_matrix[,"VBLTX"]), type="l", main="Smoothed density",
     xlab="monthly return", ylab="density estimate", col="slateblue1")
# Generating quantile plot of the cc returns
qqnorm(return_matrix[,"VBLTX"], col="slateblue1")
qqline(return_matrix[,"VBLTX"])

par(mfrow=c(1,1))

# Return distribution comparison
# Show boxplot of three series on one plot
boxplot(return_matrix[,"VBLTX"], return_matrix[,"FMAGX"], return_matrix[,"SBUX"],
        names=colnames(return_matrix), col="slateblue1")

# Do the same thing using the PerformanceAnalytics function chart.Boxplot

# -------------------------------------------------------
# -------------------------------------------------------
# Trying things -----------------------------------------
par(mfrow=c(2,2))
findDrawdowns(all_returns[,3], geometric = TRUE)
chart.Drawdown(all_returns[,3],
               main="Drawdown from Peak Equity Attained")
chart.TimeSeries(all_returns[,3], main=" ") 
chart.TimeSeries(SBUX_prices, main=" ") 
par(mfrow=c(1,1))
AverageDrawdown(all_returns[,3])

SBUX.ts <- ts(SBUX_prices, start = c(1998,1), end = c(2013, 12), freq=12)
plot(decompose(SBUX.ts))
SBUX.decom <- decompose(SBUX.ts, type = "mult")

# -------------------------------------------------------
# -------------------------------------------------------
library(FinancialInstrument)
require("FinancialInstrument")
currency(c("USD", "EUR")) # define some currencies
stock(c("SPY", "LQD", "IBM", "GS"), currency="USD") # define some stocks
ls_stocks() #get the names of all the stocks

update_instruments.yahoo(ls_stocks())
update_instruments.TTR(ls_stocks()) # doesn't update ETFs
update_instruments.masterDATA(ls_stocks()) # only updates ETFs

## Find only the ETFs; update_instruments.masterDATA added a "Fund.Type" field
## to the ETFs, but not to the stocks
ls_instruments_by("Fund.Type") # all instruments that have a "Fund.Type" field

# -------------------------------------------------------
# Prediction --------------------------------------------
# Getting financial data
# Load relevant packages
library(PerformanceAnalytics)
library(zoo)
library(tseries)

stock <-("kilburn.bo")

# Obtaining historical data
SYMB_prices <- get.hist.quote(instrument=stock, 
                              quote="AdjClose",provider="yahoo", 
                              compression="m", retclass="zoo", quiet=TRUE)

# Find the start of the quoted prices
start <- c(as.numeric(format(index(SYMB_prices)[1], format = "%Y", tz = "", usetz = FALSE)),
           as.numeric(format(index(SYMB_prices)[1], format = "%m", tz = "", usetz = FALSE)))

SYMB.ts <- ts(SYMB_prices, start = start, end = c(2014, 12), freq=12)

#plot(SYMB.ts, xlab= "Time (months)", ylab = "Price")
SYMB.hw <- HoltWinters (SYMB.ts, seasonal = "mult")
#SYMB.hw 
#SYMB.hw$coef 
#SYMB.hw$SSE
#plot (SYMB.hw$fitted)
#plot (SYMB.hw)

# Prediction into the future !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SYMB.predict <- predict(SYMB.hw, n.ahead = 4 * 12, level = 0.9, prediction.interval = TRUE)
SYMB.ts <- ts(SYMB_prices, start = start, freq=12)
ts.plot(SYMB.ts, as.ts(SYMB.hw$fitted[,1]), SYMB.predict, lty = c(1,2,3,3,3)
        , xlab= "Time (months)", ylab = paste("Price", stock))
