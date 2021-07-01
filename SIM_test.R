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
#the first column in the iitial data set is the index.  We also remove column 1 (index) from the initial data #set.

#Initialize
beta <- rep(0,ncol(rrr))
alpha <- rep(0,ncol(rrr))
mse <- rep(0,ncol(rrr))
Ribar <- rep(0,ncol(rrr))
Ratio <- rep(0,ncol(rrr))
stock <- rep(0,ncol(rrr))

#Risk free asset:
rf <- 0.001


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


#So far we have this table:
xx <- (cbind(stock,alpha, beta, Ribar, mse, Ratio))


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
for(i in(1:nrow(xx))) {
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


#SHORT SALES NOT ALLOWED:
#First create a matrix up to the maximum of col5:
table1 <- cbind(A, col1, col2, col3, col4, col5)
table2 <- table1[1:which(col5==max(col5)), ]

#Compute the Zi:
z_no_short <- (table2[,3]/table2[,5])*(table2[,6]-max(col5))

#Compute the xi:
x_no_short <- z_no_short/sum(z_no_short)

#The final table when short sales are not allowed:
Weights_no_short <- cbind(table2, z_no_short, x_no_short)

#=====================================================
#=====================================================
#Note:  If we want to compute the expected return and variance of these portfolios we can use t(x) %*% means and t(x) %*% Sigma %*% x , but we have to rearrange the order of the stocks so they match the initial columns of the original data set.

Weights_no_short
