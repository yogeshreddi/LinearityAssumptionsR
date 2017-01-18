View(women)

attach(women)


plot(height,weight)

reg1 <- lm(formula = weight~height)
plot(weight,height*height)

abline(reg1)
metricsFun(women$weight,reg1$fitted.values)

summary(reg1)
ncv.test(reg1)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar) 


detach(women)


library(MASS)

View(Boston)

colnames(Boston)

reg2 <- lm(data = Boston, medv ~.)
summary(reg2)
metricsFun(Boston$medv,reg2$fitted.values)

summary(reg2)
ncv.test(reg2)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg2) 
par(opar) 



# diagnostics

# Simple linear regr line
x <- runif(500,1,100)
y <- 500 + x + rnorm(500,0,10)
reg3 <- lm(y~x)

library(car)
install.packages('cars')
ncvTest(reg3)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg3) 
par(opar) 

# hetro scadacity
x <- runif(500,1,100)
y <- 500 + x + x*rnorm(500)
reg4 <- lm(y~x)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg4) 
par(opar) 


# non linear 
x = runif(500,1,20) 
y = ifelse(x<15,100+2*x +rnorm(500),100+5*x+rnorm(500)) 
reg5 = lm(y~x) 

summary(reg5)

plot(x,y)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg5) 
par(opar) 




x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
reg6 = lm(y~x) 

plot(x,y)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg6) 
par(opar) 


# interaction

x1 = runif(500,1,20) 
x2 = runif(500,1,20) 
y = x1+4*x2+0.5*x1*x2 + rnorm(500) 
reg1 = lm(y~x1+x2)

plot(x2,y)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar) 



#Scenario 6: 
#Extreme Values (x) 
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 
reg1 = lm(y~x) 

plot(x,y)
abline(reg1)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)           




x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
y[499] = 2000 
reg1 = lm(y~x)

summary(reg1)

plot(x,y)
abline(reg1)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)  

# multi collenarity
x1 = runif(500,1,10) 
lambda = 0.9
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)


summary(reg1)

plot(x2,y)
abline(reg1)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)  
