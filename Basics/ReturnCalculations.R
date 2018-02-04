# Load the monthly stock return data
# Assign the URL to the CSV file
data_url <- "http://assets.datacamp.com/course/compfin/sbuxPrices.csv"

# Load data frame 
sbux_df <- read.csv(data_url, header = TRUE, stringsAsFactors = FALSE)

# Checking structure
str(sbux_df)
head(sbux_df)
tail(sbux_df)

class(sbux_df$Date)

# Adjusted closing prices
closing_prices <- sbux_df[,"Adj.Close", drop=FALSE]

# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 <- which(sbux_df$Date == "3/1/1994")
index_2 <- which(sbux_df$Date == "3/1/1995")
  
# Extract prices between 3/1/1994 and 3/1/1995
some_prices <- sbux_df[index_1:index_2,"Adj.Close"]

# Subset directly on dates (alternative way)
# Create a new data frame that contains the price data with the dates as the row names
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) <- sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, subsetting directly on the dates
price_1 <- sbux_prices_df["3/1/1994",1]
price_2 <- sbux_prices_df["3/1/1995",1] 

# Plot closing prices 
plot(sbux_df$Adj.Close, type="l", col="blue", lwd=2, 
     ylab="Adjusted close", main="Monthly closing price of SBUX") 
legend(x='topleft',legend='SBUX', lty=1, lwd=2, col='blue')

# Simple returns
n <- nrow(sbux_prices_df)   # Number of time periods
sbux_ret <- (sbux_prices_df[2:n,1] - sbux_prices_df[1:n-1,1])/sbux_prices_df[1:n-1,1]
  
# Checking structure: not a data frame object
class(sbux_ret)

# Add dates to simple return vector
names(sbux_ret) <- sbux_df$Date[2:n]
head(sbux_ret)

# Compute continuously compounded 1-month returns
# r = ln(P_t/P_t-1)
sbux_ccret <- log(sbux_prices_df[2:n,1]) - log(sbux_prices_df[1:n-1,1])
  
# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) <- sbux_df$Date[2:n]
  
# Show sbux_ccret
head(sbux_ccret)

# Compare the simple and cc returns
head(cbind(sbux_ret, sbux_ccret))

# Plot the returns on the same graph
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# Add horizontal line at zero
abline(h=0)

# Add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# Add the continuously compounded returns
lines(sbux_ccret, type="l", col="red", lwd=2)

# Calculate growth of $1 invested in SBUX
# Compute gross monthly return of $1
sbux_gret <- 1 + sbux_ret
  
# Compound product of the monthly returns
sbux_fv <- cumprod(sbux_gret)
  
# Plot the evolution of the $1 invested in SBUX as a function of time
plot(sbux_fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")