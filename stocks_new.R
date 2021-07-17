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
  
  # historical covariance. very simple
  if(method == "historical") {
    return(cov(get_returns(stocks)))
  }
  
  # single index model covariance.
  # something is wrong with the math here. I'm not sure what.
  if(method == "SIM") {
    index <- portfolio$index
    rf <- portfolio$rf
    
    r <- get_returns(cbind(index, stocks))
    
    print("returns")
    
    #Compute the betas:
    covmat <- var(r)

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
    
    xx <- (cbind(stock,alpha, beta, Ribar, mse, Ratio))
    rownames(xx) <- names(rrr)
    
    A <- xx[order(-xx[,6]),]
    
    A
    
    col1 <- rep(0,nrow(A))
    col2 <- rep(0,nrow(A))
    col3 <- rep(0,nrow(A))
    col4 <- rep(0,nrow(A))
    col5 <- rep(0,nrow(A))
    
    
    #Create the last 5 columns of the table:
    col1 <- (A[,4]-rf)*A[,3]/A[,5]
    col3 <- A[,3]^2/A[,5]
    for(i in(1:nrow(A))) {
      col2[i] <- sum(col1[1:i])
      col4[i] <- sum(col3[1:i])
    }
    
    #So far we have:
    cbind(A, col1, col2, col3, col4)
    
    
    #Compute the Ci (col5):
    for(i in (1:nrow(A))) {
      col5[i] <- var(r[,1])*col2[i]/(1+var(r[,1])*col4[i])
    }
    
    #SHORT SALES ALLOWED:
    #Compute the Zi:
    z_short <- (A[,3]/A[,5])*(A[,6]-col5[nrow(A)])
    #Compute the xi:
    x_short <- z_short/sum(z_short)
    
    #The final table when short sales allowed:
    Weights_with_short <- cbind(A, col1, col2, col3, col4, col5, z_short, x_short)
    print(Weights_with_short)
    
    mat <- matrix(nrow=n, ncol=n)
    msm <- var(r[,1])
    
    beta <- beta[Weights_with_short[,1]]
    mse <- mse[Weights_with_short[,1]]
    
    
    #Var-covar matrix based on the SIM model:
    for(i in 1:n){

      for(j in 1:n){

        if(i==j){
          mat[i,j] <- (msm * beta[i]^2) + mse[i]
        } else
        {
          mat[i,j] <- msm * beta[i] * beta[j]
        }
      }
    }
    
    print(Weights_with_short)
    print(mat)
    
    table1 <- Weights_with_short[,-c(12,13)]
    table2 <- table1[1:which(col5==max(col5)), ]
    
    print(table1)
    print(table2)
    
    if(portfolio$shorts_allowed == FALSE) {

      mat <- mat[1:which(col5==max(col5)),1:which(col5==max(col5))]
      rownames(mat) <- rownames(table2)
      colnames(mat) <- rownames(table2)


      #Compute the Zi:
      z_no_short <- (table2[,3]/table2[,5])*(table2[,6]-max(col5))
      
      #Compute the xi:
      x_no_short <- z_no_short/sum(z_no_short)
      print(x_no_short)
      
    }

    else {
      rownames(mat) <- rownames(Weights_with_short)
      colnames(mat) <- rownames(Weights_with_short)
    }

    print(mat)
    
    return(mat)
    
  }
  
  # Constant Correlation Model
  if(method == "CC") {
    
    n <- ncol(stocks)
    rf <- portfolio$rf
    
    r <- get_returns(stocks)
    rho <- (sum(cor(r[1:n]))-n)/(n^2 - n)
    
    #Initialize the vectors:
    col1 <- rep(0,n)
    col2 <- rep(0,n)
    col3 <- rep(0,n)
    
    #Initialize the var-covar matrix:
    y <- rep(0,n^2)
    mat <- matrix(y, ncol=n, nrow=n)
    
    Rbar <- colMeans(r)
    Rbar_f <- Rbar-rf
    sigma <- ( diag(var(r[1:n])) )^0.5
    Ratio <- Rbar_f/sigma
    
    #Initial table:
    xx <- (cbind(Rbar, Rbar_f, sigma, Ratio))
    
    print(Rbar)
    print(Rbar_f)
    print(sigma)
    print(Ratio)
    
    #Order the table based on the excess return to sigma ratio:
    aaa <- xx[order(-Ratio),]
    
    
    #Create the last 3 columns of the table:
    for(i in(1:n)) {
      
      col1[i] <- rho/(1-rho+i*rho)
      col2[i] <- sum(aaa[,4][1:i])
    }
    
    #Compute the Ci:
    for(i in (1:n)) {
      
      col3[i] <- col1[i]*col2[i]
      
    }
    
    #Create the entire table until now:
    xxx <- cbind(aaa, col1, col2, col3)
    
    #SHORT SALES ALLOWED:
    #Compute the Zi:
    z <- (1/((1-rho)*xxx[,3]))*(xxx[,4]-xxx[,7][nrow(xxx)])
    
    #Compute the xi:
    x <- z/sum(z)
    
    #The final table:
    aaaa <- cbind(xxx, z, x)
    print(aaaa)

    #Var-covar matrix based on the constant correlation model:
    for(i in 1:n){
      
      for(j in 1:n){
        
        if(i==j){
          mat[i,j]=aaaa[i,3]^2
        } else
        {
          mat[i,j]=rho*aaaa[i,3]*aaaa[j,3]
        }
      }
    }
    
    if(portfolio$shorts_allowed == FALSE) {
      print("Test")
      mat <- mat[1:which(aaaa[,7]==max(aaaa[,7])),1:which(aaaa[,7]==max(aaaa[,7]))]
      aaaaa <- aaaa[1:which(aaaa[,7]==max(aaaa[,7])), ]
      z_no <- (1/((1-rho)*aaaaa[,3]))*(aaaaa[,4]-aaaaa[,7][nrow(aaaaa)])
      x_no <- z_no/sum(z_no)
      print(x_no)
      
      rownames(mat) <- rownames(aaaaa)
      colnames(mat) <- rownames(aaaaa)

      
    }
    
    else {
      rownames(mat) <- rownames(aaa)
      colnames(mat) <- rownames(aaa)
    }
    
    return(mat)
    
  }
  
  ### MULTI GROUP MODEL
  
  if(method == "MGM") {
    
    corrmat <- cor(get_returns(stocks))
    breaks <- portfolio$breaks
    
    rho_bar <- multi_group_rho(corrmat, breaks)
    
    n <- ncol(stocks)
    
    rho_mat <- matrix(nrow = n, ncol = n)

    industries_key <- numeric(n)
    
    industries_index <- 1
    
    for(i in 1:n) {
      
      industries_key[i] <- industries_index
      
      if(i %in% breaks) {
        industries_index <- 1 + industries_index
      }
      
      
    }
    
    for(i in 1:n) {
      ind_i <- industries_key[i]
      for(j in 1:n) {
        ind_j <- industries_key[j]
        
        rho_mat[i,j] <- rho_bar[ind_i, ind_j]
        
        if(i == j) {
          rho_mat[i,j] <- 1
        }
        
      }
    }
    
    print(rho_bar)
    print(rho_mat)
    
    mat <- matrix(nrow=n, ncol=n)
    
    covmat <- cov(get_returns(stocks))
    variances <- diag(covmat)
    
    for(i in 1:n) {
      for(j in 1:n) {
        
        mat[i, j] <- variances[i]^.5 * variances[j]^.5 * rho_mat[i,j]
        
      }
    }
    
    rownames(mat) <- colnames(stocks)
    colnames(mat) <- colnames(stocks)
    
    return(mat)

  }
  
}

# using the covariance, the frontier can be drawn, and a portfolio can be found for a given E.
# once the portfolio is found, the portfolio's risk can be found as well.
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

# given a covariance matrix, the returns, and the risk free asset
# the "best" portfolio that can be given with a combination of the risk free asset can be found
get_optimum_portfolio <- function(portfolio) {
  
  returns <- portfolio$returns
  sigma <- portfolio$cov
  rf <- portfolio$rf
  
  z <- solve(sigma) %*% (returns - rf)
  return(z/sum(z))
  
}

# given the returns and covariance matrix, the minimum risk portfolio can be found.
get_min_risk <- function(portfolio) {
  
  means <- portfolio$returns
  covmat <- portfolio$cov
  
  n <- ncol(portfolio$stocks)
  
  numerator <- solve(covmat) %*% rep(1, n)
  denom <- drop(t(rep(1, n)) %*% solve(covmat) %*% rep(1, n))
  
  weights <- numerator/denom
  weights
  
}

# multiply expected returns by weights.
get_portfolio_return <- function(portfolio) {
  
  drop(t(portfolio$weights) %*% portfolio$returns)
  
}

# simply the covariance of a linear transformation.
get_portfolio_variance <- function(portfolio) {
  
  drop(t(portfolio$weights) %*% portfolio$cov %*% portfolio$weights)
  
}

# this is possibly the most important function here.
# it builds the portfolio object, which contains many attributes about the portfolio, including:
# the stock data, index data, the risk free rate, the weights, whether or not shorts are allowed,
# the returns/covariance matrices of the stocks given the model, the portfolio returns and variance, and other misc. attributes
# these are all used in order to create the frontier and other visual/numerical aids/data.
build_portfolio <- function(stocks, method, rf=NA, E=NA, name=NA, rf_name=NA, index=NA, beta_adj_method=NA, shorts_allowed=NA, breaks=NA) {
  
  # the most fundamental parts of any portfolio object.
  portfolio <- list(method = method, stocks=stocks)
  portfolio$shorts_allowed <- shorts_allowed
  
  # SIM requires a beta adj. method, an index, and whether or not shorts are allowed.
  if(method=="SIM") {
    portfolio$index <- index
    portfolio$beta_adj_method <- beta_adj_method
  }
  
  if(method=="MGM") {
    portfolio$breaks <- breaks
  }
  
  portfolio$returns <- get_mean_returns(stocks) # the same for any portfolio
  portfolio$rf <- rf # could potentially be NA for historical covariance.
  portfolio$cov <- get_covariance_matrix(portfolio) # varies heavily based on the model used.
  
  # if there are negative betas, or if no short sales are allowed, the stocks and returns need to be reassigned  
  portfolio$stocks <- portfolio$stocks[colnames(portfolio$cov)] 
  portfolio$returns <- get_mean_returns(portfolio$stocks)
  
  # find portfolio given E on curve if E is supplied.
  if(!is.na(E)) {
    portfolio$weights <- portfolio_from_return(portfolio, E)
  }

  # optimum with RF, i.e. the point of tangency for the curve and the RF.
  else if(!is.na(rf)) {
    portfolio$weights <- get_optimum_portfolio(portfolio)
    print(portfolio$weights)
  }
  
  # min risk portfolio
  else {
    portfolio$weights <- get_min_risk(portfolio)
  }
  
  # self explanatory
  portfolio$port_return <- get_portfolio_return(portfolio)
  portfolio$port_var <- get_portfolio_variance(portfolio)
  
  # name set for the object if supplied. used in the app
  if(!is.na(name)) {
    portfolio$name <- name
  }
  
  # ditto for the rf
  if(!is.na(rf_name)) {
    portfolio$rf_name <- rf_name
  }
  
  # return the portfolio object
  portfolio
}

# use the hyperbola method as discussed here: http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/merton_hyperbola_example.R
# returns a dataframe that's used in the plotly in the app
plot_frontier <- function(portfolio) {
  
  E <- seq(-5, 5, .1)
  
  points_df <- data.frame(expected_return = numeric(), standard_deviations = numeric())
  means <- portfolio$returns
  covmat <- portfolio$cov
  n <- ncol(portfolio$stocks)
  
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
  
}

# just a bar graph of the weights
plot_portfolio <- function(portfolio) {
  
  weights <- portfolio$weights
  
  df <- data.frame(Stock = rownames(weights), Weight = weights[,1])
  
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