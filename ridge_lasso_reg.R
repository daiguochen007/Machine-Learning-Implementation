library(ISLR)
library(glmnet)

Hitters<- na.omit(Hitters)

x<- model.matrix(Salary~.,Hitters)[,-1] #convert factor to dummy variable
y<- Hitters$Salary

#------------------------------------------------------------------- alpha = 0 ridge
grid<- 10^seq(10,-2,length=100)
ridge.mod<- glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod)

plot(log(grid),coef(ridge.mod)[2,],ylim=c(-5,10),type="l",main = "coef and lambda")
lines(log(grid),coef(ridge.mod)[3,],type="l",col="red")
lines(log(grid),coef(ridge.mod)[4,],type="l",col="blue")
lines(log(grid),coef(ridge.mod)[5,],type="l",col="green")

#---- training set and test set
train = sample(1:nrow(x),nrow(x)/2)
test = -train
y.test= y[test]

ridge.mod<- glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh = 10^-12)
ridge.pred<- predict(ridge.mod,s=4,newx=x[test,])

mean((ridge.pred-y.test)^2)

#cross validation
cv.out<- cv.glmnet(x[train,],y[train],alpha = 0)
plot(cv.out)

cv.out$lambda.min

#whole set after cv and get coef
out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=cv.out$lambda.min)[1:20,]

#------------------------------------------------------------------- alpha = 0 lasso
lasso.mod<- glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)


#cross validation
cv.out<- cv.glmnet(x[train,],y[train],alpha = 1)
plot(cv.out)

cv.out$lambda.min

#whole set after cv and get coef
out = glmnet(x,y,alpha=1,lambda=grid)
predict(out,type="coefficients",s=cv.out$lambda.min)[1:20,]

