library(readxl)
data1 <- read_excel("Nerlove1963.xlsx")
data<-log(data1)
y<-as.matrix(data[,1])
x<-as.matrix(cbind(matrix(1,nrow(y),1),data[,2:5]))
beta<-solve(t(x)%*%x,t(x)%*%y)
#standard error
n <- nrow(y)
k <- ncol(x)
invx <- solve(t(x)%*%x)
a <- n/(n-k)
e <- y-x%*%beta
leverage <- rowSums(x*(x%*%invx))
sig2 <- (t(e) %*% e)/(n-k)
u1 <- x*(e%*%matrix(1,1,k))
u2 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
u3 <- x*((e/(1-leverage))%*%matrix(1,1,k))
v0 <- as.numeric(sig2)*invx
v1 <- invx %*% (t(u1)%*%u1) %*% invx
v1a <- a * invx %*% (t(u1)%*%u1) %*% invx
v2 <- invx %*% (t(u2)%*%u2) %*% invx
v3 <- invx %*% (t(u3)%*%u3) %*% invx
s0 <- sqrt(diag(v0)) # Homoskedastic formula
s1 <- sqrt(diag(v1)) # HC0
s1a <- sqrt(diag(v1a)) # HC1
s2 <- sqrt(diag(v2)) # HC2
s3 <- sqrt(diag(v3)) # HC3

#wald test
R<-matrix(c(0,0,1,1,1),5,1)

waldstats<-(sum(beta[3:5])-1)^2/(t(R)%*%v1a%*%R)

qchisq(0.95, 1, ncp = 0, log = FALSE)

