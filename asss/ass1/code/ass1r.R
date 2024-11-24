library(readxl)
dat <- read_excel("cps09mar.xlsx")
#3.24 
#single Asian man 
sam <- (dat[,11]==4)&(dat[,12]==7)&(dat[,2]==0)
datsam <- dat[sam,]
#experience and experience squared
experience<-datsam[,1]-datsam[,4]-6
exp2<-experience^2/100
#less than 45 year experience
sam<-experience<45
datsam <- datsam[sam,]
#logwage
Y<-as.matrix(log(datsam[,5]/(datsam[,6]*datsam[,7])))
#new experience and its squared
experience<-experience[sam]
exp2<-experience^2/100
#X 
X<-as.matrix(cbind(datsam[,4],experience,exp2,matrix(1,nrow(datsam),1)))


#estimation of 3.49
beta349<-solve(t(X)%*%X,t(X)%*%Y)

#R^2 and SSE
Y_hat<-as.matrix(0.144*datsam$education+0.043*experience-0.095/100*experience^2+0.531)
#y bar
Y_bar<-mean(Y)
#R^2
R_squared<-sum((Y_hat-Y_bar)^2)/sum((Y-Y_bar)^2)
#sum of squared errors
SSE<-sum((Y-Y_hat)^2)
#print result
cat('R^2 = ', R_squared)
cat('SSE = ', SSE)

#b
#regress logwage on experience and its sqaure
X1<-as.matrix(cbind(experience,exp2,matrix(1,nrow(datsam),1)))
beta1<-solve(t(X1)%*%X1,t(X1)%*%Y)
#regress education on experience and its sqaure
beta2<-solve(t(X1)%*%X1,t(X1)%*%as.matrix(datsam[,4]))
#residuals and regression
e_1tilda<-Y-X1%*%beta1
x_2tilda<-as.matrix(datsam[,4]-X1%*%beta2)
xres<-as.matrix(cbind(x_2tilda,matrix(1,nrow(datsam),1)))
xxres<-t(xres)%*%xres
xyres<-t(xres)%*%e_1tilda
beta2_hat<-solve(xxres,xyres)
rownames(beta2_hat)<-c('beta2_hat','intercept')
res_hat<-e_1tilda-xres%*%beta2_hat
print(beta2_hat)
#sse_NEW
SSE_NEW<-sum(res_hat^2)
#new r sqaured
rsquared_new<-1-SSE_NEW/sum((Y-Y_bar)^2)
#print result
cat('The Re-estimate  slope on education is', beta2_hat[1])
cat('The new R^2 is', rsquared_new )
cat('The new SSE is', SSE_NEW )







#3.26
#white male hispanic
wmh<-(dat[,3]==1)&(dat[,11]==1)&(dat[,2]==0)
datwmh<-dat[wmh,]
#logwage
Y<-as.matrix(log(datwmh[,5]/(datwmh[,6]*datwmh[,7])))
#education
edu<-as.matrix(datwmh[,4])
#experience
expwmh<-as.matrix(datwmh[,1]-datwmh[,4]-6)
#experience sqaure
expwmh2<-as.matrix(expwmh^2/100)
#dummy variables for region
#NE
x1<-as.numeric(datwmh[,10]==1)
#S
x2<-as.numeric(datwmh[,10]==3)
#W
x3<-as.numeric(datwmh[,10]==4)
#dummy variables for marital
#Married
xxx1<-as.numeric(datwmh[,12]==1|datwmh[,12]==2|datwmh[,12]==3)
#Widowed
xxx2<-as.numeric(datwmh[,12]==4)
#Divorced
xxx3<-as.numeric(datwmh[,12]==5)
#Separated
xxx4<-as.numeric(datwmh[,12]==6)
#regressors
X<-cbind(edu,expwmh,expwmh2,x1,x2,x3,xxx1,xxx2,xxx3,xxx4,matrix(1,nrow(datwmh),1))
beta326<-solve(t(X)%*%X,t(X)%*%Y)
