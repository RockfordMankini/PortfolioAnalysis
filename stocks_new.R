# get intervaled returns (daily, weekly, monthly, etc.)
get_returns <- function(stocks) {
  (stocks[-1,1:ncol(stocks)]-stocks[-nrow(stocks),1:ncol(stocks)])/stocks[-nrow(stocks),1:ncol(stocks)]
}

# get average returns per stock
get_mean_returns <- function(stocks) {
  colMeans(get_returns(stocks))
}

# get covariance of stocks
get_covariance_matrix <- function(portfolio) {
  
  stocks <- portfolio$stocks
  method <- portfolio$method
  
  if(method == "historical") {
    return(cov(get_returns(stocks)))
  }
  
  if(method == "SIM") {
    index <- portfolio$index
    rf <- portfolio$rf
    
    #print(index)
    
    # print(index)
    # print(stocks)
    
    r <- get_returns(cbind(index, stocks))
    
    print("returns")
    
    #Compute the betas:
    covmat <- var(r)
    
    # print(r)
    # print(covmat)
    beta <- covmat[1,-1] / covmat[1,1]
    
    #Keep only the stocks with positive betas:
    rrr <- r[,-c(1,which(beta<0)+1)]  
    #Note: which(beta<0) gives the element in the beta vector with negative beta and add 1 because 
    #the first column in the iitial data set is the index.  We also remove column 1 (index) from the initial data #set.
    
    #Initialize
    beta <- rep(0,ncol(rrr))
    alpha <- rep(0,ncol(rrr))
    mse <- rep(0,ncol(rrr))
    Ribar <- rep(0,ncol(rrr))
    Ratio <- rep(0,ncol(rrr))
    stock <- rep(0,ncol(rrr))
    
    n <- ncol(rrr)
    
    print(n)
    
    #print(rrr)
    
    #This for loop computes the required inputs:
    for(i in 1:ncol(rrr)){
      q <- lm(data=rrr, formula=rrr[,i] ~ r[,1])
      beta[i] <- q$coefficients[2] 
      alpha[i] <- q$coefficients[1] 
      mse[i] <- summary(q)$sigma^2
      Ribar[i] <- q$coefficients[1]+q$coefficients[2]*mean(r[,1])
      Ratio[i] <- (Ribar[i]-rf)/beta[i]
      stock[i] <- i
    }
    
    covmat_SIM <- matrix(nrow=n, ncol=n)
    
    for(i in 1:n) {
      b_i <- beta[i]
      for(j in 1:n) {
        
        b_j <- beta[j]
        
        if(i == j) {
          entry <- covmat[1,1] * b_i^2 + mse[i]^2
        }
        
        else {
          entry <- covmat[1,1] * b_i * b_j
        }
        
        covmat_SIM[i,j] <- entry
        
      }
    }
    
    print("Names cause the issues maybe?")
    #print(covmat_SIM)
    
    print(names(stocks))
    print(length(names(stocks)))
    print(dim(covmat_SIM))
    
    rownames(covmat_SIM) <- names(rrr)
    colnames(covmat_SIM) <- names(rrr)
    
    return(covmat_SIM)
    
  }
  
  # if(method == "mgm") {
  #   
  #   corrmat <- cor(get_returns(stocks))
  #   variances <- diag(cov(get_returns(stocks)))
  #   
  # }
  
}

portfolio_from_return <- function(portfolio, E) {
  
  means <- portfolio$returns
  covmat <- portfolio$cov
  n <- ncol(portfolio$stocks)
  
  A <- t(rep(1,n)) %*% solve(covmat) %*% means
  B <- t(means) %*% solve(covmat) %*% means
  C <- t(rep(1,n)) %*% solve(covmat) %*% rep(1,n)
  D <- B * C - A^2
  
  lambda_1 <- drop((C * E - A)/D)
  lambda_2 <- drop((B - (A * E))/D)
  
  weights <- lambda_1 * (solve(covmat) %*% means) + lambda_2 * (solve(covmat) %*% rep(1,n))
  weights
  
}

get_optimum_portfolio <- function(portfolio) {
  
  returns <- portfolio$returns
  sigma <- portfolio$cov
  rf <- portfolio$rf
  
  z <- solve(sigma) %*% (returns - rf)
  return(z/sum(z))
  
}

get_min_risk <- function(portfolio) {
  
  means <- portfolio$returns
  covmat <- portfolio$cov
  
  n <- ncol(portfolio$stocks)
  
  numerator <- solve(covmat) %*% rep(1, n)
  denom <- drop(t(rep(1, n)) %*% solve(covmat) %*% rep(1, n))
  
  weights <- numerator/denom
  weights
  
}

get_portfolio_return <- function(portfolio) {
  
  drop(t(portfolio$weights) %*% portfolio$returns)
  
}

get_portfolio_variance <- function(portfolio) {
  
  drop(t(portfolio$weights) %*% portfolio$cov %*% portfolio$weights)
  
}

build_portfolio <- function(stocks, method, rf=NA, E=NA, name=NA, rf_name=NA, index=NA, beta_adj_method=NA, shorts_allowed=NA) {
  
 # print("test")
  
  portfolio <- list(method = method, stocks=stocks)
  
  if(method=="SIM") {
    portfolio$index <- index
    portfolio$beta_adj_method <- beta_adj_method
    portfolio$shorts_allowed <- shorts_allowed
  }
  
  portfolio$returns <- get_mean_returns(stocks)
  portfolio$rf <- rf
  print("test")
  portfolio$cov <- get_covariance_matrix(portfolio)
  
  portfolio$stocks <- portfolio$stocks[colnames(portfolio$cov)]
  
  
  # done in case of negative betas
  portfolio$returns <- get_mean_returns(portfolio$stocks)
  
  # find portfolio given E on curve
  if(!is.na(E)) {
    portfolio$weights <- portfolio_from_return(portfolio, E)
  }

  # optimum with RF
  else if(!is.na(rf)) {
    portfolio$weights <- get_optimum_portfolio(portfolio)
    print(portfolio$weights)
  }
  
  # min risk
  else {
    portfolio$weights <- get_min_risk(portfolio)
  }
  
  portfolio$port_return <- get_portfolio_return(portfolio)
  portfolio$port_var <- get_portfolio_variance(portfolio)
  
  if(!is.na(name)) {
    portfolio$name <- name
  }
  
  if(!is.na(rf_name)) {
    portfolio$rf_name <- rf_name
  }
  
  portfolio
}

plot_frontier <- function(portfolio) {
  
  E <- seq(-5, 5, .1)
  
  points_df <- data.frame(expected_return = numeric(), standard_deviations = numeric())
  means <- portfolio$returns
  covmat <- portfolio$cov
  n <- ncol(portfolio$stocks)
  print(n)
  
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
  # return(df)
  # 
  # g <- plot_ly(df,x=~sdeff, y=~y1, type = 'scatter', mode = 'lines', name="Positive Returns")
  # g <- g %>% add_trace(x = ~sdeff, y= ~y2,  type='scatter', mode = 'lines', name="Negative Returns")
  # 
  # x <- list(
  #   title = "Standard Deviation (Risk)"
  # )
  # y <- list(
  #   title = "Expected Return"
  # )
  # 
  # g <- g %>% layout(xaxis = x, yaxis = y)
  # 
  # #df2 <- data.frame(x=0, y=.4)
  # #g <- g %>% add_trace(data=df2, x = ~x, y = ~y, mode="markers")
  # #print(g)
  # 
  # g %>% layout(xaxis = list(range = c(0, .2)), yaxis = list(range=c(-.2, .2)))
  
}



plot_portfolio <- function(portfolio) {
  
  weights <- portfolio$weights
  
  df <- data.frame(Stock = rownames(weights), Weight = weights[,1])
  
  g <- plot_ly(df, x=~Stock, y=~Weight, type="bar")
  
  g 
  
}

#data <- read.csv("stonks_cut.csv")
# f <- build_portfolio(data, "historical")
# 
# f
