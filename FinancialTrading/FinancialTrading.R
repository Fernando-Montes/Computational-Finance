# Packages needed
library(quantmod)
library(TTR)  # To use SMA() simple-moving-average function
library(quantstrat)
library(blotter)


# "Fix to make it work"
.blotter <- new.env()
.strategy <- new.env()

# -------------------------------
# TRADING BASICS
# -------------------------------
# Plotting financial data
# Get SPY from Yahoo Finance ("yahoo")
getSymbols("SPY", from = "2000-01-01", to = "2016-06-30", src = "yahoo", adjust = TRUE)
# Plot the closing price of SPY
plot(Cl(SPY))
# Add a 200-day moving average using the lines command
# Accepted wisdom is that whenever the price is above the 200-day moving average, 
# a whole assortment of good things will happen, such as the asset appreciating in price, low volatility, and so on. 
lines(SMA(Cl(SPY), n = 200), col = "red")

# -------------------------------
# BOILER PLATE STRATEGY
# -------------------------------
# Understanding initialization settings - I
# Create initdate, from, and to charater strings
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"
# Set the timezone to UTC
Sys.setenv(TZ = "UTC")
# Set the currency to USD 
currency("USD")
# -------------------------------
# Understanding initialization settings - II
# Retrieve SPY from yahoo
getSymbols("SPY", from = "2003-01-01", to = "2015-12-31", src = "yahoo", adjust = TRUE)
# Use the stock command to initialize SPY and set currency to USD
stock("SPY", currency = "USD")
# -------------------------------
# Understanding initialization settings - III
# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000
# Define the names of your strategy, portfolio and account
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"
# Remove the existing strategy if it exists
rm.strat(strategy.st)
# -------------------------------
# Understanding initialization settings - IV
# initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
# initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
# initialize the orders
initOrders(portfolio.st, initDate = initdate)
# store the strategy
strategy(strategy.st, store = TRUE)

# -------------------------------
# INDICATORS
# -------------------------------
# The SMA and RSI functions
# Create a 200-day moving average
spy_sma <- SMA(x = Cl(SPY), n = 200)
# Create an RSI with a 3 day lookback period
# The RSI is a formula that expresses the fraction of gains and losses over the past lookback periods, 
# and is the difference of 100 - (100/(1 + RS)), where RS is the average gain over the average loss 
# over the lookback window decided.
spy_rsi <- RSI(price = Cl(SPY), n = 3)
# -------------------------------
# Visualize an indicator and guess its purpose - I
# Plot the closing prices of SPY
plot(Cl(SPY))
# Overlay a 200-day SMA
lines(SMA(Cl(SPY), n = 200), col = "red")
# Is this a trend or reversion indicator?
"trend"
# -------------------------------
# Visualize an indicator and guess its purpose - II
# plot the closing price of SPY
plot(Cl(SPY))
# plot the RSI 2
plot(RSI(Cl(SPY), n = 2)) # Remember: 0 - only gains, 100 - only losses
# trend or reversion?
"reversion"
# -------------------------------
# Implementing an indicator - I
# Add a 200-day simple moving average indicator to your strategy
add.indicator(strategy = strategy.st, 
              # Add the SMA function
              name = "SMA", 
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 200), 
              # Label your indicator SMA200
              label = "SMA200")
# -------------------------------
# Implementing an indicator - II
# Add a 50-day simple moving average indicator to your strategy
add.indicator(strategy = strategy.st, 
              # Add the SMA function
              name = "SMA", 
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              # Label your indicator SMA50
              label = "SMA50")
# -------------------------------
# Implementing an indicator - III
# add an RSI 3 indicator to your strategy
add.indicator(strategy = strategy.st, 
              # add an RSI function to your strategy
              name = "RSI", 
              # use a lookback period of 3 days
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              # label it RSI_3
              label = "RSI_3")
# -------------------------------
# Code your own indicator - I
# Write the RSI_avg function
RSI_avg <- function(price, n1, n2) {
  # RSI 1 takes an input of the price and n1
  rsi_1 <- RSI(price = price, n = n1)
  # RSI 2 takes an input of the price and n2
  rsi_2 <- RSI(price = price, n = n2)
  # RSI_avg is the average of rsi_1 and rsi_2
  RSI_avg <- (rsi_1 + rsi_2)/2
  # Your output of RSI_avg needs a column name of "RSI_avg"
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}
# Add the RSI_avg function to your strategy using an n1 of 3 and an n2 of 4, and label it "RSI_3_4"
add.indicator(strategy.st, name = "RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")
# -------------------------------
# Code your own indicator - II
# Declare the DVO function. The first argument is the high, low, and close of market data.
# It computes a ratio that is between the closing price and average of high and low prices.
# Next, it applies a simple moving average to that quantity to smooth out noise.
# Finally, it uses the runPercentRank function to take a running percentage rank of this average ratio, 
# and multiplies it by 100 to convert it to a 0-100 quantity.
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  # Convert ratio into a 0-100 value using runPercentRank function
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}
# -------------------------------
# Apply your own indicator
# add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")
# use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
# subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]

# -------------------------------
# SIGNALS
# -------------------------------
# Using sigComparison
# add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", 
           # we are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            # particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"),
           # label this signal longfilter
           label = "longfilter")
# -------------------------------
# Using sigCrossover
# add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           # we're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            # the relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           # label it filterexit
           label = "filterexit")
# -------------------------------
# Using sigThreshold - I
# implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           # use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            # the threshold is 20
                            threshold = 20, 
                            # we want the oscillator to be under this value
                            relationship = "lt", 
                            # we're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           # label it longthreshold
           label = "longthreshold")
# -------------------------------
# Using sigThreshold - II 
# add a sigThreshold signal to your strategy that specifies that DVO_2_126 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           # reference the column of DVO_2_126
           arguments = list(column = "DVO_2_126", 
                            # set a threshold of 80
                            threshold = 80, 
                            # the oscillator must be greater than 80
                            relationship = "gt", 
                            # we are interested only in the cross
                            cross = TRUE), 
           # label it thresholdexit
           label = "thresholdexit")
# -------------------------------
# Using sigFormula
# Create your dataset: test
test_init <- applyIndicators(strategy.st, mktdata=OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)
# -------------------------------
# Combining signals 
# add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.st, name = "sigFormula",
           # specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            # specify that cross must be TRUE
                            cross = TRUE),
           # label it longentry
           label = "longentry")

# -------------------------------
# RULES
# -------------------------------
# Using add.rule() to implement an exit rule: SMA50 is less than the SMA200
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
# -------------------------------
# Another exit rule: DVO_2_126 crosses above 80 - stock too expensive
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
# -------------------------------
# Using add.rule() to implement an entry rule
# create an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
         # use the longentry column as the sigcol: SMA50 is greater than the SMA200 & DVO less than 20
         arguments=list(sigcol = "longentry", 
                        # set sigval to TRUE
                        sigval = TRUE, 
                        # set orderqty to 1
                        orderqty = 1,
                        # use a market type of order
                        ordertype = "market",
                        # take the long orderside
                        orderside = "long",
                        # do not replace other signals
                        replace = FALSE, 
                        # buy at the next day's opening price
                        prefer = "Open"),
         # this is an enter type rule, not an exit
         type = "enter")
# -------------------------------
# Implementing a rule with an order sizing function
# add a rule that uses an osFUN to size an entry position
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          # use the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          # the tradeSize argument should be equal to tradesize (defined earlier)
                          tradeSize = tradesize,
                          # the maxSize argument should be equal to tradesize as well
                          maxSize = tradesize),
         type = "enter")

# -------------------------------
# Analyzing strategy
# -------------------------------
# Running your strategy
# use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
# update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
# update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)
# what is the date of the last trade?
"2015-12-13"
# -------------------------------
# Profit factor
# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)
# Print the profit factor
tstats$Profit.Factor
# -------------------------------
# Using chart.Posn()
# use chart.Posn to view your system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")
# -------------------------------
# Adding an indicator to a chart.Posn() chart
# compute the SMA50
sma50 <- SMA(x = Cl(SPY), n = 50)
# compute the SMA200
sma200 <- SMA(x = Cl(SPY), n = 200)
# compute the DVO_2_126 with an navg of 2 and a percentlookback of 126
dvo <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)
# recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")
# overlay the SMA50 on your plot as a blue line
add_TA(sma50, on = 1, col = "blue")
# overlay the SMA200 on your plot as a red line
add_TA(sma200, on = 1, col = "red")
# add the DVO_2_126 to the plot in a new window
add_TA(dvo)
# -------------------------------
# Cash Sharpe ratio
portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)
# -------------------------------
# Returns Sharpe ratio
# get instrument returns
instrets <- PortfReturns(portfolio.st)
# compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)

