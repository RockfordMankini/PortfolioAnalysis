#Stocks used:
#^GSPC,AAPL,AMZN,BA,COST,CVX,DIS,ELY,FL,GILD,GS,HD,JCP,LMT,M,MMM,PG,S,SBUX,SHOO,SMRT,TM,YUM,K,C,XOM,IBM,F,CAT,MCD,COKE.

#Data
#2015-01-31 to 2019-12-31

a <- read.csv("stockData50.csv", sep=",", header=TRUE)

#Convert adjusted close prices into returns:
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)]

#Compute the betas:
covmat <- var(r)
beta <- covmat[1,-1] / covmat[1,1]

#Keep only the stocks with positive betas:
rrr <- r[,-c(1,which(beta<0)+1)]  
#Note: which(beta<0) gives the element in the beta vector with negative beta and add 1 because 
#the first column in the initial data set is the index.  We also remove column 1 (index) from the initial data #set.


Rfr <- seq(-0.05,.01,0.0005)

#Initialize the two vectors:
rbar_opt <- rep(0,length(Rfr))
risk_opt <- rep(0,length(Rfr))


for(l in 1:length(Rfr)){
  #Risk free asset:
  rf <- Rfr[l]
  #rf <- .002
  #Initialize
  beta <- rep(0,ncol(rrr))
  alpha <- rep(0,ncol(rrr))
  mse <- rep(0,ncol(rrr))
  Ribar <- rep(0,ncol(rrr))
  Ratio <- rep(0,ncol(rrr))
  stocknum <- rep(0,ncol(rrr))
  #stock <- names(rrr)
  
  #This for loop computes the required inputs:
  for(i in 1:ncol(rrr)){
    q <- lm(data=rrr, formula=rrr[,i] ~ r[,1])
    beta[i] <- q$coefficients[2] 
    alpha[i] <- q$coefficients[1] 
    mse[i] <- summary(q)$sigma^2
    Ribar[i] <- q$coefficients[1]+q$coefficients[2]*mean(r[,1])
    Ratio[i] <- (Ribar[i]-rf)/beta[i]
    stocknum[i] <- i
  }
  
  #So far we have this table:
  #xx <- (cbind(stock,alpha, beta, Ribar, mse, Ratio))
  xx <- (data.frame(stocknum,alpha, beta, Ribar, mse, Ratio))
  
  
  #Order the table based on the excess return to beta ratio:
  A <- xx[order(-xx[,6]),]
  
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
  
  
  #The final table when short sales allowed:
  B <- cbind(A, col1, col2, col3, col4, col5)
  rownames(B) <- NULL
  
  #SHORT SALES NOT ALLOWED:
  #First create a matrix up to the maximum of col5:
  #table1 <- cbind(A, col1, col2, col3, col4, col5)
  #table2 <- (B[1:which(col5==max(col5)), ], nrow=which(col5==max(col5)), ncol=ncol(B))
  table2 <- B[1:which(col5==max(col5)), ]
  
  #Compute the Zi:
  z_no_short <- (table2[,3]/table2[,5])*(table2[,6]-max(col5))
  
  #Compute the xi:
  x_no_short <- z_no_short/sum(z_no_short)
  
  #Compute the mean and variance for each portfolio when short sales not allowed:
  #First match the columns of the data with the composition of the portfolio:
  r1 <- data.frame(rrr[,table2[,1]])
  
  beta1 <- rep(0,ncol(r1))
  sigma_e1 <- rep(0,ncol(r1))
  alpha1 <- rep(0,ncol(r1))
  
  for(i in 1:ncol(r1)){
    q1<- lm(r1[,i] ~ r[,1])
    beta1[i] <- q1$coefficients[2] 
    sigma_e1[i] <- summary(q1)$sigma^2
    alpha1[i] <- q1$coefficients[1] 
  } 
  
  means1 <- colMeans(r1)
  #means1 <- alpha1 + beta1*mean(r[,1])
  
  
  #Construct the variance covariance matrix using SIM:
  xx <- rep(0,ncol(r1)*(ncol(r1)))             #Initialize
  varcovar <- matrix(xx,nrow=ncol(r1),ncol=ncol(r1))  #the variance covariance matrix 
  
  
  for (i in 1:ncol(r1)){
    for (j in 1:ncol(r1)){
      varcovar[i,j]=beta1[i]*beta1[j]*var(r[,1])
      if(i==j){varcovar[i,j]=beta1[i]^2*var(r[,1])+ sigma_e1[i]}
    }
  }
  
  
  rbar_opt[l] <- t(x_no_short) %*% means1
  risk_opt[l] <- ( t(x_no_short) %*% varcovar %*% x_no_short )^.5
  
}

plot(risk_opt, rbar_opt, type="l", main="Efficient frontier when short sales not allowed", ylab="Portfolio expected return", xlab="Portfolio standard deviation")

#=====================================================
#=====================================================
#Note:  When we compute the expected return and the variance of these portfolios we use t(x) %*% means and t(x) %*% Sigma %*% x , but we have to rearrange the order of the stocks so they match the initial columns of the original data set.
