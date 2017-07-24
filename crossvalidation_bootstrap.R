library(ISLR)  #Smarket
library(boot) #cv

set.seed(1)
train <- sample(392,196)

lm.fit<- lm(mpg~horsepower, data=Auto,subset=train)
auto.pred <- predict(lm.fit,Auto)[-train]
#MSE
mean((Auto$mpg[-train] - auto.pred)^2)

#polynominal
lm.fit2<- lm(mpg~poly(horsepower,2), data=Auto,subset=train)
auto.pred <- predict(lm.fit2,Auto)[-train]
#MSE
mean((Auto$mpg[-train] - auto.pred)^2)

lm.fit3<- lm(mpg~poly(horsepower,3), data=Auto,subset=train)
auto.pred <- predict(lm.fit3,Auto)[-train]
#MSE
mean((Auto$mpg[-train] - auto.pred)^2)

#---------------- leave one out CV
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err<- cv.glm(Auto,glm.fit)

cv.err<- rep(0,5)
for (i in 1:5){
  glm.fit<- glm(mpg~poly(horsepower,i), data=Auto)
  cv.err[i]<- cv.glm(Auto,glm.fit)$delta[1]
}
plot(cv.err,type="b")

#---------------- k-fold CV
cv.err.10<- rep(0,10)
for (i in 1:10){
  glm.fit<- glm(mpg~poly(horsepower,i), data=Auto)
  cv.err.10[i]<- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
plot(cv.err.10,type="b")

#---------------- bootstrap
alpha.fn<- function(data,index){
  X<- data$X[index]
  Y<- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
alpha.fn(Portfolio,sample(100,100,replace = T)) # can run this many times and get sigma

#estimate param sigma
boot(Portfolio,alpha.fn,R=1000)

#estimate coef sigma
boot.fn<- function(data,index){return(coef(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index)))}

boot.fn(Auto,1:392)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))


