library(tseries)
library(PerformanceAnalytics)

# -----------------------------------------------
# Making a risk-reward scatter diagram
# Obtaining information
rm(returns)
djia <- c("AA","AAPL","AXP","BA","BAC","CAT","CVX","DD","DIS","GE","HD","HPQ","INTC","IBM",
          "JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UTX","VZ","WMT",
          "XOM", "T")
temp <- get.hist.quote(instrument=djia[1],start=as.Date("1990-12-31"),quote="AdjClose",quiet=T,compression="m")
# Calculating returns
returns_djia <- Return.calculate(temp)
returns_djia <- returns_djia[-1,]
returns <- returns_djia
for(i in 2:length(djia)) {
  temp <- get.hist.quote(instrument=djia[i],start=as.Date("1990-12-31"),quote="AdjClose",quiet=T,compression="m")
  colnames(temp)[which(colnames(temp) == "AdjClose")] = djia[i]
  # Calculating returns
  returns_djia <- Return.calculate(temp)
  returns_djia <- returns_djia[-1,]
  # Merging with all data
  returns <- merge(returns,returns_djia)
  colnames(returns)[i] = djia[i]
}
colnames(returns)[1] = djia[1]

# Create a vector of returns 
means <- apply(returns, 2, "mean")
# Create a vector of standard deviation
sds <- apply(returns, 2, "sd")
# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)
abline(h = 0, lty = 3)

# -----------------------------------------------
# Exploring monthly returns of the 30 DJIA stocks
# Verify the class of returns 
class(returns)
# Investigate the dimensions returns
dim(returns)
# Create a vector of row means
# portfolio of equally weighted assets
ew_preturns <- rowMeans(returns)
# Cast the numeric vector back to an xts object
ew_preturns <- xts(ew_preturns, order.by = time(returns))
# Plot ew_preturns
plot.zoo(ew_preturns, xlab = "Time", ylab = "Returns equally weighted portfolio")

# -----------------------------------------------
# Finding the mean-variance efficient portfolio
# Create an optimized portfolio of returns
# Compute portfolio that has the desired expected returnand no other portfolio exists, 
# which has the same mean return, but a smaller variance.
opt <- portfolio.optim(returns)
# Create pf_weights
pf_weights <- opt$pw
# Assign asset names
names(pf_weights) <- colnames(returns)
# Select optimum weights opt_weights
opt_weights <- pf_weights[pf_weights>0.01]
# Barplot of opt_weights
barplot(opt_weights)
# Print expected portfolio return and volatility
opt$pm
opt$ps

# -----------------------------------------------
# Effect of the return target
# Create portfolio with target return of average returns 
pf_mean <- portfolio.optim(returns)
# Create portfolio with target return 10% greater than average returns
pf_10plus <- portfolio.optim(returns, pm = 1.1*mean(returns)) 
# Print the standard deviations of both portfolios
pf_mean$ps
pf_10plus$ps
# Calculate the proportion increase in standard deviation
(pf_10plus$ps-pf_mean$ps)/pf_mean$ps*100.0

# The efficient frontier
# Calculating return vs sds for an optimized mean-variance portfolio
pf_return <- seq(mean(returns), 0.023, 0.0005)
# Initialize empty vectors
pf_sds <- rep(NA, times = length(pf_return) )
# Create a for loop to calculate Sharpe ratios
for(i in 1:length(pf_return)) {
  pf_sds[i] <- portfolio.optim(returns, pm = pf_return[i])$ps 
}
# Plot efficiency frontier
plot(pf_sds, pf_return, xlab = "Variance", ylab= "Return")

# -----------------------------------------------
# Imposing weight constraints
# Create vectors of maximum weights
max_weights1 <- rep(1, ncol(returns))
max_weights2 <- rep(0.1, ncol(returns))
max_weights3 <- rep(0.05, ncol(returns))
# Create an optimum portfolio with max weights of 100%
opt1 <- portfolio.optim(returns, reshigh = max_weights1)
# Create an optimum portfolio with max weights of 10%
opt2 <- portfolio.optim(returns, reshigh = max_weights2)
# Create an optimum portfolio with max weights of 5%
opt3 <- portfolio.optim(returns, reshigh = max_weights3)
# Calculate how many assets have a weight that is greater than 1% for each portfolio
sum(opt1$pw > .01)
sum(opt2$pw > .01)
sum(opt3$pw > .01)
# Print portfolio volatilites 
opt1$ps
opt2$ps
opt3$ps

# -----------------------------------------------
# Computing the efficient frontier using a grid of target returns
# Calculate each stocks mean returns
stockmu <- colMeans(returns)
# Create a grid of target values
grid <- seq(from = 0.01, to = max(stockmu), length.out = 50)
# Create empty vectors to store means and deviations
vpm <- rep(NA,length(grid)) 
vpsd <- rep(NA,length(grid)) 
# Create an empty matrix to store weights
mweights <- matrix(NA, 50, 30)
# Create your for loop
for(i in 1:length(grid)) {
  opt <- portfolio.optim(x = returns, pm = grid[i])
  vpm[i] <- opt$pm
  vpsd[i] <- opt$ps
  mweights[i, ] <- opt$pw 
}
# Plot efficiency frontier
plot(sds, means, xlab = "Variance", ylab= "Return", xlim = c(0.03, 0.14))
text(sds, means, labels = colnames(returns), cex = 0.7)
points(vpsd, vpm, type = "l")

# -----------------------------------------------
# The minimum variance and maximum Sharpe ratio portfolio
# Create weights_minvar as the portfolio with the least risk
weights_minvar <- mweights[vpsd == min(vpsd), ]
# Calculate the Sharpe ratio
vsr <- (vpm - 0.0075)/vpsd
# Create weights_max_sr as the portfolio with the maximum Sharpe ratio
weights_max_sr <- mweights[vsr == max(vsr),]
# Create barplot of weights_minvar and weights_max_sr
par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
barplot(weights_minvar[weights_minvar > 0.01])
barplot(weights_max_sr[weights_max_sr > 0.01])

# -----------------------------------------------
# Split-sample evaluation
# Create returns_estim 
returns_estim <- window(returns, start = "1991-01-01", end = "2003-12-31")
# Create returns_eval
returns_eval <- window(returns, start = "2004-01-01", end = "2015-12-31")
# Create vector of max weights
max_weights <- rep(0.1, dim(returns)[2])
# Create portfolio with estimation sample 
pf_estim <- portfolio.optim(returns_estim, reshigh = max_weights)
# Create portfolio with evaluation sample
pf_eval <- portfolio.optim(returns_eval, reshigh = max_weights)
# Create a scatter plot
plot(pf_estim$pw, pf_eval$pw)
abline(h = 0, b = 1, lty = 3)

# -----------------------------------------------
# Out of sample performance evaluation
# Create returns_pf_estim
returns_pf_estim <- Return.portfolio(returns_estim, pf_estim$pw, rebalance_on = "months")
# Create returns_pf_eval
returns_pf_eval <- Return.portfolio(returns_eval, pf_estim$pw, rebalance_on = "months")
# Print a table for your estimation portfolio
table.AnnualizedReturns(returns_pf_estim)
# Print a table for your evaluation portfolio
table.AnnualizedReturns(returns_pf_eval)

# Print portfolio volatilites 