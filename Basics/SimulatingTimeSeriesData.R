# Simulate data from a MA(1) model
# Y_t_ave = 0.05 + e_t + theta*e_(t-1) where e_t iid N(0,(0.1)^2)

set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1_sim <- 0.05 + arima.sim(model = list(ma=0.5), n = 250, mean = 0, sd = 0.1)

# A line plot of the simulated observations
plot(ma1_sim, xlab = "time", ylab = "y(t)", type = "l", main = "MA(1) Process: mu=0.05, theta=0.5")
abline(h=0)

# Plotting the theoretical and the sample Autocorrelation Function ACF
# The theoretical autocorrelation function gives you for each lag 
# the autocorrelation implied by the model.
# Generate the theoretical ACF with upto lag 10
acf_ma1_model <- ARMAacf(ma = 0.5, lag.max = 10)
  
# Split plotting window in three rows
par(mfrow=c(3,1))

# First plot: The simulated observations
plot(ma1_sim, type="l",main="MA(1) Process: mu=0.05, theta=0.5",xlab="time",ylab="y(t)")
abline(h=0)

# Second plot: Theoretical ACF
plot(1:10, acf_ma1_model[2:11], type="h", col="blue",  ylab="ACF", main="theoretical ACF")

# Third plot: Sample ACF
# Assign to tmp the Sample ACF
tmp <- acf(ma1_sim, lag.max = 10, main="Sample ACF")

# Reset graphical window to only one graph
par(mfrow=c(1,1))

# Generate the same three graphs as in the previous exercise 
par(mfrow=c(3,1))

# An AR(1) model
# Y_t_ave = 0.05 + phi*(Y_(t-1) - 0.05) where e_t iid N(0,(0.1)^2)
set.seed(123);
# Simulate 250 observations from the described AR(1) model
ar1_sim <- 0.05 + arima.sim(model = list(ar=0.5), n = 250, mean = 0, sd = 0.1)

# First plot: The simulated observations
plot(ar1_sim, type="l", main="AR(1) Process: mu=0.05, phi=0.5", xlab="time", ylab="y(t)")
abline(h=0)

# Generate the theoretical ACF with ten lags
acf_ar1_model <- ARMAacf(ar = 0.5, lag.max = 10)

# Second plot: Theoretical AFC
plot(1:10, acf_ar1_model[2:11], type="h", col="blue", main="theoretical ACF")

# Third plot: Sample AFC
tmp <- acf(ar1_sim, lag.max = 10, main="Sample ACF")
  
# Reset plotting window to default
par(mfrow=c(1,1))