library(readxl)
data <- read_excel("ddk2011test.xlsx")
#data clean
data<-data.frame(data$schoolid,data$totalscore,data$tracking,data$etpteacher,data$girl,data$agetest,data$percentile)
data<-data[data$data.girl!='.',]
data<-data[data$data.agetest!='.',]
data$data.girl<-sapply(data$data.girl,as.numeric)-2
data$data.agetest<-sapply(data$data.agetest,as.numeric)
data$data.percentile<-sapply(data$data.percentile,as.numeric)
#cluster based
y <- scale(as.matrix(data$data.totalscore))
n <- nrow(y)
x<-cbind(data$data.tracking,data$data.agetest,data$data.girl,data$data.etpteacher,data$data.percentile,matrix(1,n,1))
schoolid <- as.matrix(data$data.schoolid)
k <- ncol(x)
xx <- t(x)%*%x
invx <- solve(xx)
beta <- solve(xx,t(x)%*%y)
xe <- x*rep(y-x%*%beta,times=k)
# Clustered robust standard error
xe_sum <- rowsum(xe,schoolid)
G <- nrow(xe_sum)
omega <- t(xe_sum)%*%xe_sum
scalee <- G/(G-1)*(n-1)/(n-k)
V_clustered <- scalee*invx%*%omega%*%invx
se_clustered <- sqrt(diag(V_clustered))
print(beta,digits = 5)
print(se_clustered,digits = 5)
#conventional robust
e <- y-x%*%beta
leverage <- rowSums(x*(x%*%invx))
a <- n/(n-k)
sig2 <- (t(e) %*% e)/(n-k)
u1 <- x*(e%*%matrix(1,1,k))
u2 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
u3 <- x*((e/(1-leverage))%*%matrix(1,1,k))

v0 <- invx*as.numeric(sig2)
v1 <- invx %*% (t(u1)%*%u1) %*% invx
v1a <- a * invx %*% (t(u1)%*%u1) %*% invx
v2 <- invx %*% (t(u2)%*%u2) %*% invx
v3 <- invx %*% (t(u3)%*%u3) %*% invx
s0 <- sqrt(diag(v0)) # Homoskedastic formula
s1 <- sqrt(diag(v1)) # HC0
s1a <- sqrt(diag(v1a)) # HC1
s2 <- sqrt(diag(v2)) # HC2
s3 <- sqrt(diag(v3)) # HC3
print(s0)
print(s1)
print(s1a)
print(s2)
print(s3)
