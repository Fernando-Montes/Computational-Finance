# Compute probabilites
# X ~ N(0.05, (0.10)^2)
mu_x = 0.05
sigma_x = 0.10

# Calculating normal probability distributions 
# Pr(X > 0.10)
1-pnorm(0.1, mean = mu_x, sd = sigma_x)  
  
# Pr(X < -0.10)
pnorm(-0.1, mean = mu_x, sd = sigma_x)  

# Pr(-0.05 < X < 0.15)
pnorm(0.15, mean = mu_x, sd = sigma_x)-pnorm(-0.05, mean = mu_x, sd = sigma_x)

# Compute quantiles
# 1%, 5%, 95% and 99% quantile
c(qnorm(0.01, mean = mu_x, sd = sigma_x), qnorm(0.05, mean = mu_x, sd = sigma_x), 
  qnorm(0.95, mean = mu_x, sd = sigma_x), qnorm(0.99, mean = mu_x, sd = sigma_x))

# Compute probability densities
# Normally distributed monthly returns
x_vals <- seq(-0.25, 0.35, length.out = 100)
MSFT <- dnorm(x_vals, mean = 0.05, sd = 0.1)
SBUX <- dnorm(x_vals, mean = 0.025, sd = 0.05)

# Plot normal curve
# Normal curve for MSFT and SBUX
plot(x_vals, MSFT, type="l", col="blue", ylab="Normal curves", ylim = c(0,8))
lines(x_vals, SBUX, type = "l", col="red")

# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), 
       col = c("blue", "red"), lty = 1)

# Determine the value-at-risk of simple monthly returns
# R ~ N(0.04, (0.09)^2) 
mu_R <- 0.04
sigma_R <- 0.09
  
# Initial wealth W0 equals $100,000
W0 <- 100000
  
# The 1% value-at-risk
W0*qnorm(0.01, mean = mu_R, sd = sigma_R)
# The 5% value-at-risk
W0*qnorm(0.05, mean = mu_R, sd = sigma_R)

# Determine the value-at-risk of continuously compounded monthly returns
# r ~ N(0.04, (0.09)^2) 
mu_r <- 0.04 
sigma_r <- 0.09
  
# Initial wealth W0 equals $100,000
W0 <- 100000

# Transformation between compound and simple interest
# R + 1 = exp(r)
# The 1% value-at-risk
W0*(exp(qnorm(0.01, mean = mu_r, sd = sigma_r))-1)
  
# The 5% value-at-risk
W0*(exp(qnorm(0.05, mean = mu_r, sd = sigma_r))-1)

# Compute simple monthly returns
# Vectors of prices
PA <- c(38.23, 41.29)
PC <- c(41.11, 41.74)
  
# Simple monthly returns
RA <- (PA[2]-PA[1])/PA[1]
RC <- (PC[2]-PC[1])/PC[1]

# Compute continuously compounded monthly returns
# Continuously compounded returns
rA <- log(RA+1) 
rC <- log(RC+1) 

# Compute simple total returns and dividend yields
# Cash dividend per share
DA <- 0.1

# Simple total return
RA_total <- (DA+PA[2]-PA[1])/PA[1]

# Dividend yield
DY <- DA/PA[1]

# Compute annual returns
# Simple annual return
RA_annual <- (1+RA)^12-1
  
# Continuously compounded annual return
rA_annual <- log(1+RA_annual)

# Compute portfolio shares and portfolio returns
# Portfolio shares
xA <- 8000/10000
xC <- 2000/10000

# Simple monthly return
xA*RA + xC*RC