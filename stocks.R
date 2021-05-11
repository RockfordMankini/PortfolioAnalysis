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
get_covariance_matrix <- function(stocks, method) {
  
  if(method == "historical") {
    return(cov(get_returns(stocks)))
  }
  
  # if(method == "mgm") {
  #   
  #   corrmat <- cor(get_returns(stocks))
  #   variances <- diag(cov(get_returns(stocks)))
  #   
  # }
  
}

# get minimum risk portfolio
get_min_risk <- function(stocks, method) {
  
  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks, method)
  
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

get_portfolio_variance <- function(portfolio, stocks, method) {
  
  covmat <- get_covariance_matrix(stocks, method)
  drop(t(portfolio) %*% covmat %*% portfolio)
  
}

portfolio_from_return <- function(stocks, E, method) {

  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks, method)
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

plot_frontier <- function(stocks, method) {
  
  E <- seq(-5, 5, .1)
  
  points_df <- data.frame(expected_return = numeric(), standard_deviations = numeric())
  means <- get_mean_returns(stocks)
  covmat <- get_covariance_matrix(stocks, method)
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

### MULTI GROUP MODEL
multi_group_rho <- function(corrmat, breaks) {
  
  industries <- length(breaks)
  
  start_inds <- c(1, breaks[1:industries-1] + 1)
  end_inds <- breaks
  
  rho_bar <- matrix(nrow = industries, ncol = industries)
  
  for(row in 1:industries) {
    
    rows <- start_inds[row]:end_inds[row]
    
    for(column in 1:industries) {
      
      if(row > column) {
        next
      }
      
      columns <- start_inds[column]:end_inds[column]
      
      rho <- corrmat[rows, columns]
      
      if(row == column) {
        rho <- rho[upper.tri(rho)]
      }
      
      rho_bar[row, column] <- mean(rho)
      rho_bar[column, row] <- mean(rho)
      
    }
    
  }
  
  rho_bar
  
}

make_phi_matrix <- function(rho_bar, breaks, means, variances, rf) {
  
  industries <- length(breaks)
  
  A <- matrix(nrow = industries, ncol = industries)
  
  for(row in 1:industries) {
    
    if(row == 1) {
      N <- breaks[row]
    }
    
    else {
      N <- breaks[row] - breaks[row - 1]
    }
    
    rho_within <- rho_bar[row, row]
    
    for(column in 1:industries) {
      
      rho_between <- rho_bar[row, column]
      
      if(row == column) {
        A[row, column] <- 1 + ((N * rho_within) / (1 - rho_within))
      }
      
      else {
        A[row, column] <- (N * rho_between) / (1 - rho_within)
      }
      
    }
    
  }
  
  C <- numeric(industries)
  
  for(industry in 1:industries) {
    
    if(industry == 1) {
      start_ind <- 1
    }
    
    else {
      start_ind <- breaks[industry - 1] + 1
    }
    
    end_ind <- breaks[industry]
    
    rho <- rho_bar[industry, industry]
    
    C[industry] <- sum((means[start_ind:end_ind] - rf) / (variances[start_ind:end_ind]^.5 * (1 - rho)))
    
  }
  
  solve(A) %*% C
  
}

make_multigroup_portfolio <- function(rho_bar, phi, breaks, means, variances, rf, short_sales_allowed = TRUE) {
  
  z <- numeric(length(means))
  z_index <- 1
  
  sds <- variances^.5
  
  for(industry in 1:length(breaks)) {
    
    # get C_star_i
    c_star <- rho_bar[industry,] %*% phi
    
    if(industry == 1) {
      start_ind <- 1
    }
    
    else {
      start_ind <- breaks[industry - 1] + 1
    }
    
    end_ind <- breaks[industry]
    
    for(stock in start_ind:end_ind) {
      
      component_1 <- (1 / (sds[z_index] * (1 - rho_bar[industry, industry])))
      
      component_1 <- drop(component_1)
      
      component_2 <- (((means[z_index] - rf)/sds[z_index]) - c_star)
      
      z[z_index] <- component_1 * component_2
      z_index <- z_index + 1
      
    }
    
  }
  
  return(z)
  
}

### END MULTIGROUP MODEL

get_optimum_portfolio <- function(stocks, rf, method) {
  
  if(method == "historical") {
    
    returns <- get_mean_returns(stocks)
    sigma <- get_covariance_matrix(stocks, "historical")
    
    z <- solve(sigma) %*% (returns - rf)
    return(z/sum(z))
  }
  
  if(method == "mgm") {
    
    returns <- get_mean_returns(stocks)
    sigma <- get_covariance_matrix(stocks, "mgm")
    
  }
  
}
