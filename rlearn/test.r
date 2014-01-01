n <-10               # 样本量
m <-2               # 离群点数量,m<n
r <- rnorm(n,0,1)    # 误差项
x <- runif(n,-10,50)   # 自变量x 取值在[a,b]
beta1 <- 3.5
beta0 <- 6
betax <- 12
y    <- beta1*x    + beta0 +r      #  y=b0+b1*x
y[(n-m+1):n] <- betax*x[(n-m+1):n] + betax +r[(n-m+1):n]
plot(x,y)

model_1 <- lm(y ~  x)
plot(y~x)
abline(model_1,col = "black",lty = "solid" )


par(mfrow=c(2,2))
plot(model_1)

model_1 <- lm(y ~  x)
library("MASS")
model_2 <- rlm(y ~x,psi=psi.huber)
model_3 <- rlm(y ~ x,psi=psi.hampel)
model_4 <- rlm(y ~ x,psi=psi.bisquare)
summary(model_1)


fit <- lm(y ~  x)
y.stu<-rstudent(model_2)  
y.fit<-predict(model_2)  
par(mfrow=c(2,1))  
plot(y.stu~y.fit)  
hist(y.stu,freq=FALSE)  
lines(density(y.stu))  


cooks.distance(model_1)
result<-cooks.distance(model_1)  
result[cooks.distance(model_1)>4/(10)]  

library("car")
influencePlot(model_1)
hatvalues(model)



xtable::xtable( summary(model_2)$coef)

plot(y~x)
abline(model_1,col = "black",lty = "solid" )
abline(model_2,col = "red",  lty = "dotted" )
abline(model_3,col = "blue", lty = "dashed" )
abline(model_4,col = "green",lty = "dotdash")
legend("topleft",,c("OLS","huber","hampel","bisquare"),lty=c("solid","dotted","dashed","dotdash"),col=c("black","red","blue","green"))








library(MASS)
F <- forbes$bp
h <- forbes$pres
F <- c(F,190)
h <- c(h,10)
F <- c(F,200)
h <- c(h,11)
myforbes <- data.frame(F =F ,h = h, logh=log(h) ,log100h =100*log(h))
model_1 <-  lm(formula = log100h ~ F,  data = myforbes)
model_2 <- rlm(formula = log100h ~ F, data = myforbes,psi=psi.huber)
model_3 <- rlm(formula = log100h ~ F, data = myforbes,psi=psi.hampel)
model_4 <- rlm(formula = log100h ~ F, data = myforbes,psi=psi.bisquare)
plot(log100h ~ F, data = myforbes)
abline(model_1,col = "black")
abline(model_2,col = "red",  lty = "dotted" )
abline(model_3,col = "blue", lty = "dashed" )
abline(model_4,col = "green",lty = "dotdash")
points(x[])
legend("right",,c("OLS","huber","hampel","bisquare"),lty=c("solid","dotted","dashed","dotdash"),col=c("black","red","blue","green"))

getwd()
data <-read.table("words.txt",header=TRUE)
x1 <-diff(data$ch)

set.seed(92937)
test-arima.sin(model)




