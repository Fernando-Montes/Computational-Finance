# Data on US Equities and bonds 
library(tseries)
eq_prices <- get.hist.quote(instrument="SPY",start=as.Date("2003-12-31"),end=as.Date("2016-06-30"),quote="AdjClose",quiet=T,compression="m")
bond_prices <- get.hist.quote(instrument="AGG",start=as.Date("2003-12-31"),end=as.Date("2016-06-30"),quote="AdjClose",quiet=T,compression="m")
eq_prices <- eq_prices/as.numeric(eq_prices[1])
bond_prices <-  bond_prices/as.numeric(bond_prices[1])

assets <- merge(eq_prices, bond_prices)
eq_weights <- c(0.6, 0.4)
# Create a portfolio rebalancing monthly 
returns_6040 <- Return.portfolio(R = assets, weights = eq_weights, rebalance_on = 'months')
assets <- merge(assets, returns_6040$portfolio.returns)
par(mfrow = c(1, 1) , mar=c(3, 2, 2, 2))
plot.zoo(assets, screens=c(1,1), col = 1:3, lwd = 2)
legend("topleft", c("Equities (SPY)","Bonds (AGG)", "60/40 Equities - Bonds"), lty = 1, col = 1:3, lwd = 2)

# -----------------------------------------------
# Choice of portfolio weights
returns_equities <- Return.calculate(eq_prices)
returns_equities <- returns_equities[-1,]
returns_bonds <- Return.calculate(bond_prices)
returns_bonds <- returns_bonds[-1,]
# Create a grid
grid <- seq(0, 1, 0.01)
# Initialize an empty vector for sharpe ratios
vsharpe <- rep(NA, times = length(grid) )
# Create a for loop to calculate Sharpe ratios
for(i in 1:length(grid)) {
  weight <- grid[i]
  preturns <- weight*returns_equities + (1-weight)*returns_bonds
  vsharpe[i] <- SharpeRatio.annualized(preturns)
}
# Plot weights and Sharpe ratio
plot(grid, vsharpe, xlab = "Weights", ylab= "Ann. Sharpe ratio")
abline(v = grid[vsharpe == max(vsharpe)], lty = 3)

# -----------------------------------------------
# Interpreting correlation
# Create a scatter plot
chart.Scatter(returns_bonds, returns_equities, main = "Eq. vs bonds")
# Find the correlation
cor(returns_equities, returns_bonds)
# Merge returns_equities and returns_bonds 
returns <- merge(returns_equities, returns_bonds)
# Find and visualize the correlation using chart.Correlation
chart.Correlation(returns)
# Visualize the rolling estimates using chart.RollingCorrelation
chart.RollingCorrelation(returns_equities, returns_bonds, width = 24)

# -----------------------------------------------
# Making a risk-reward scatter diagram
# Obtaining information
eq_prices <- get.hist.quote(instrument="SPY",start=as.Date("2006-08-29"),quote="AdjClose",quiet=T,compression="m")
bond_prices <- get.hist.quote(instrument="AGG",start=as.Date("2006-08-29"),quote="AdjClose",quiet=T,compression="m")
estate_prices <- get.hist.quote(instrument="VNQ",start=as.Date("2006-08-29"),quote="AdjClose",quiet=T,compression="m")
commod_prices <- get.hist.quote(instrument="GSG",start=as.Date("2006-08-29"),quote="AdjClose",quiet=T,compression="m")
# Gain/loss investing
eq_prices <- eq_prices/as.numeric(eq_prices[1])
bond_prices <-  bond_prices/as.numeric(bond_prices[1])
estate_prices <- estate_prices/as.numeric(estate_prices[1])
commod_prices <-  commod_prices/as.numeric(commod_prices[1])
gainloss <- merge(eq_prices,bond_prices,estate_prices,commod_prices)
# Calculating returns
returns_equities <- Return.calculate(eq_prices)
returns_equities <- returns_equities[-1,]
returns_bonds <- Return.calculate(bond_prices)
returns_bonds <- returns_bonds[-1,]
returns_estate <- Return.calculate(estate_prices)
returns_estate <- returns_estate[-1,]
returns_commod <- Return.calculate(commod_prices)
returns_commod <- returns_commod[-1,]
# Merging
returns <- merge(returns_equities,returns_bonds,returns_estate,returns_commod)
# Plotting
plot.zoo(gainloss, screens=c(1,1,1,1), col = 1:4, lwd = 2)
legend("topleft", c("Equities (SPY)","Bonds (AGG)", "Real estate (VNQ)", "Commodities (GSG)"), 
       lty = 1, col = 1:4, lwd = 2)
# Create a vector of returns 
means <- apply(returns, 2, "mean")
# Create a vector of standard deviation
sds <- apply(returns, 2, "sd")
# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)
abline(h = 0, lty = 3)

# -----------------------------------------------
# Covariance matrix
# Create a matrix with variances on the diagonal
diag_cov <- diag(sds^2)
# Create a covariance matrix of returns
cov_matrix <- cov(returns)
# Create a correlation matrix of returns
cor_matrix <- cor(returns)
# Verify covariances equal the product of standard deviations and correlation
all.equal(cov_matrix[1,2], cor_matrix[1,2] * sds[1] * sds[2])

# -----------------------------------------------
# Matrix-based calculation of portfolio mean and variance
weights <- c(0.4,0.4,0.1,0.1)
vmeans <- means
sigma <- cov_matrix
# Create a weight matrix w
w <- as.matrix(weights)
# Create a matrix of returns
mu <- as.matrix(vmeans)
# Calculate portfolio mean monthly returns
t(w)%*%mu
# Calculate portfolio volatility
t(w)%*%sigma%*%w

# -----------------------------------------------
# Risk budget
# Create portfolio weights
weights <- c(0.4,0.4,0.1,0.1)
# Create volatility budget
vol_budget <- StdDev(returns, portfolio_method = "component", weights = weights)
# Make a table of weights and risk contribution
weights_percrisk <- cbind(weights, vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")
# Print the table
weights_percrisk