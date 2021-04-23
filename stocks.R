library(ggplot2)
library(plotly)

# get intervaled returns (daily, weekly, monthly, etc.)
get_returns <- function(stocks) {
  (stocks[-1,1:ncol(stocks)]-stocks[-nrow(stocks),1:ncol(stocks)])/stocks[-nrow(stocks),1:ncol(stocks)]
}

# get average returns per stock
get_mean_returns <- function(stocks) {
  colMeans(get_returns(stocks))
}

# get covariance of stocks
get_covariance_matrix <- function(stocks) {
  cov(get_returns(stocks))
}

# get minimum risk portfolio
get_min_risk <- function(stocks) {
  
  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks)
  
  n <- ncol(stocks)
  
  numerator <- solve(covmat) %*% rep(1, n)
  denom <- drop(t(rep(1, n)) %*% solve(covmat) %*% rep(1, n))
  
  weights <- numerator/denom
  weights
  
}

get_portfolio_return <- function(portfolio, stocks) {
  
  means <- get_mean_returns(stocks)
  drop(t(portfolio) %*% means)
  
}

get_portfolio_variance <- function(portfolio, stocks) {
  
  covmat <- get_covariance_matrix(stocks)
  drop(t(portfolio) %*% covmat %*% portfolio)
  
}

portfolio_from_return <- function(stocks, E) {

  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks)
  n <- ncol(stocks)
  
  A <- t(rep(1,n)) %*% solve(covmat) %*% means
  B <- t(means) %*% solve(covmat) %*% means
  C <- t(rep(1,n)) %*% solve(covmat) %*% rep(1,n)
  D <- B * C - A^2
  
  lambda_1 <- drop((C * E - A)/D)
  lambda_2 <- drop((B - (A * E))/D)
  
  weights <- lambda_1 * (solve(covmat) %*% means) + lambda_2 * (solve(covmat) %*% rep(1,n))
  weights
  
}

plot_frontier <- function(stocks) {
  
  E <- seq(-5, 5, .1)
  
  points_df <- data.frame(expected_return = numeric(), standard_deviations = numeric())
  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks)
  n <- ncol(stocks)
  
  A <- t(rep(1,n)) %*% solve(covmat) %*% means
  B <- t(means) %*% solve(covmat) %*% means
  C <- t(rep(1,n)) %*% solve(covmat) %*% rep(1,n)
  D <- B * C - A^2
  
  #Efficient frontier:
  minvar <- 1/c(C)
  minE <- c(A)/c(C)
  sdeff <- seq((minvar)^0.5, 1, by = 0.00001)
  options(warn = -1)
  y1 <- (c(A) + sqrt(c(D)*(c(C)*sdeff^2 - 1)))*(1/c(C)) 
  y2 <- (c(A) - sqrt(c(D)*(c(C)*sdeff^2 - 1)))*(1/c(C)) 
  options(warn = 0)
  
  df <- data.frame(sdeff, y1, y2)
    
  g <- plot_ly(df,x=~sdeff, y=~y1, type = 'scatter', mode = 'lines', name="Positive Returns")
  g <- g %>% add_trace(x = ~sdeff, y= ~y2,  type='scatter', mode = 'lines', name="Negative Returns")
  
  x <- list(
    title = "Standard Deviation (Risk)"
  )
  y <- list(
    title = "Expected Return"
  )
  
  g <- g %>% layout(xaxis = x, yaxis = y)
  
  #df2 <- data.frame(x=0, y=.4)
  #g <- g %>% add_trace(data=df2, x = ~x, y = ~y, mode="markers")
  #print(g)
  
  g %>% layout(xaxis = list(range = c(0, .2)), yaxis = list(range=c(-.2, .2)))
  
}

plot_portfolio <- function(portfolio) {
  
  df <- data.frame(Stock = rownames(portfolio), Weight = portfolio[,1])
  
  g <- plot_ly(df, x=~Stock, y=~Weight, type="bar")
  
  g 
  
}

#plot_portfolio((portfolio_from_return(data, .01)))
# 
# data.frame(rownames(f), f[,1])
# 
# f <- portfolio_from_return(data, .01)
# rownames(f)
# 
# data <- read.csv("stonks.csv")
# data <- data[,-(1:3)]
# write.csv(data, "stonks_cut.csv" ,row.names = FALSE)
# 
# View(data.frame(portfolio_from_return(data, .01)))
# 
# 
# get_returns(data)
# get_mean_returns(data)
# get_covariance_matrix(data[,1:5])
# 
# min_risk <- get_min_risk(data)
# 
# get_portfolio_return(min_risk, data)
# get_portfolio_variance(min_risk, data)
# 
# plot_frontier(data)
