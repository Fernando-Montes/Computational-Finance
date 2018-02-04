## First look at the data

library(zoo)
library(PerformanceAnalytics)
library(tseries)

date1 <- as.Date("2004-01-01")
date2 <- as.Date("2016-12-01")

# Returns a data.frame with the stock price of coca-cola as a function of time
ko <- get.hist.quote(instrument="ko", start = date1, end = date2,
                              quote="AdjClose",provider="yahoo", 
                              compression="m", retclass="zoo", quiet=TRUE)
# Returns a data.frame with the stock price of pepsico as a function of time
pep <- get.hist.quote(instrument="pep", start = date1, end = date2,
                     quote="AdjClose",provider="yahoo", 
                     compression="m", retclass="zoo", quiet=TRUE)

# Normalizing with respect to initial money invested
ko <- ko/ko[1,1][[1]]
pep <- pep/pep[1,1][[1]]

plot.zoo(cbind(ko,pep), type = "s") 

# Define ko_pep 
ko_pep <- ko/pep

# Make a time series plot of ko_pep
plot.zoo(ko_pep)  

# Add as a reference, a horizontal line at 1
abline(h=1)  

# -----------------------------------------------
## Calculation portfolio weights

# Define the vector values
values <- c(4000,4000,2000)
# Define the vector weights
weights <- values/sum(values)
# Print the resulting weights
print(weights)

# -----------------------------------------------
## Weights of a market capitalization weighted portfolio
# Define marketcaps
marketcaps <- c(5, 8, 9, 20, 25, 100, 100, 500, 700, 2000 ) 
# Compute the weights
weights <- marketcaps/sum(marketcaps)
# Inspect summary statistics
summary(weights)
# Create a barplot of weights
barplot(weights)

# -----------------------------------------------
## Calculation portfolio returns

# Vector of initial value of the assets
in_values <- c(1000, 5000, 2000)
# Vector of final values of the assets
fin_values <- c(1100, 4500, 3000)
# Weights as the proportion of total value invested in each assets
weights <- in_values/sum(in_values)
# Vector of simple returns of the assets 
returns <- (fin_values - in_values)/in_values
# Compute portfolio return using the portfolio return formula
preturns <- sum(weights*returns)

# -----------------------------------------------
## Time series of asset returns
## Performance Analytics
# Load package PerformanceAnalytics 
library(PerformanceAnalytics)
prices1 <- get.hist.quote(instrument="aapl", 
                     quote="AdjClose",provider="yahoo", start = as.Date("2006-01-03"),
                     compression="d", retclass="zoo", quiet=TRUE)
prices2 <- get.hist.quote(instrument="msft", 
                     quote="AdjClose",provider="yahoo", start = as.Date("2006-01-03"),
                     compression="d", retclass="zoo", quiet=TRUE)
prices <- merge(as.xts(prices1), as.xts(prices2))
colnames(prices)[which(colnames(prices) == "Adjusted")] = "AAPL"
colnames(prices)[which(colnames(prices) == "Adjusted.1")] = "MSFT"
# Print the first and last six rows of prices
head(prices)
tail(prices)
# Create the variable returns using Return.calculate()  
returns <- Return.calculate(prices)
# Print the first six rows of returns. Note that the first observation is NA, because there is no prior price.
head(returns)
# Remove the first row of returns
returns <- returns[-1, ]

# -----------------------------------------------
## Return
# Create the weights
eq_weights <- c(0.5, 0.5)
# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(R = returns, weights = eq_weights)
# Create a portfolio rebalancing monthly 
pf_rebal <- Return.portfolio(R = returns, weights = eq_weights, rebalance_on = 'months')
# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(pf_bh)
plot.zoo(pf_rebal)

# -----------------------------------------------
## Return with verbose option
# Create the weights
eq_weights <- c(0.5, 0.5)
# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE )
# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE )
# Create end of eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight
# Create eop_weight_rebal
eop_weight_rebal <- pf_rebal$EOP.Weight
# Plot end of period weights
par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$AAPL)
plot.zoo(eop_weight_rebal$AAPL)
