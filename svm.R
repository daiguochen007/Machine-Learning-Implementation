library(e1071)

#----------------------------------------------------------  support vector classifier
#cannot seprate by line
x = matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,] = x[y==1,]+1

plot(x,col=(3-y))

dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data=dat,kernel="linear",cost=10,scale =F)

plot(svmfit,dat)

svmfit$index

summary(svmfit)

#adjust cost
svmfit = svm(y~.,data=dat,kernel="linear",cost=0.1,scale =F)
plot(svmfit,dat)
svmfit$index

#cross validation
tune.out = tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2),ncol=2)
ytest = sample(c(-1,1),20,rep=T) 
xtest[ytest==1,] = xtest[ytest==1,]+1
testdat= data.frame(x=xtest,y=as.factor(ytest))

ypred = predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

#can seprate by line
x = matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,] = x[y==1,]+4
plot(x,col=(y+5)/2,pch=19)

dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data=dat,kernel="linear",cost=10,scale =F)
plot(svmfit,dat)
svmfit = svm(y~.,data=dat,kernel="linear",cost=0.1,scale =F)
plot(svmfit,dat)

#---------------------------------------------------------- SVM 
#nonlinear margin
x = matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat = data.frame(x=x,y=as.factor(y))
plot(x,col=y)

train=sample(200,100)
svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])

svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5) #overfit
plot(svmfit,dat[train,])

#cross validation
tune.out = tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),
                gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newx=dat[-train,]))

#------------------- ROC
library(ROCR)
rocplot=function(pred,truth,...){
  predob=prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf,...)
}
svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=0.5,cost=1,decision.values=T)
fitted = attributes(predict(svmfit,dat[train,],decision.values=T))$decision.values

rocplot(fitted,dat[train,"y"],main="Training Data")

svmfit = svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted = attributes(predict(svmfit,dat[train,],decision.values=T))$decision.values

rocplot(fitted,dat[train,"y"],add=T,col="red")
#test
svmfit = svm(y~.,data=dat[-train,],kernel="radial",gamma=0.5,cost=1,decision.values=T)
fitted = attributes(predict(svmfit,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],main="Test Data")

svmfit = svm(y~.,data=dat[-train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted = attributes(predict(svmfit,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")

#----------------------------------------multi classification
x = rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat = data.frame(x=x,y=as.factor(y))
plot(x,col=(y+1))

svmfit = svm(y~.,data=dat,kernel="radial",gamma=1)
plot(svmfit,dat)

#----------------------------------------- real data
library(ISLR)
table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~. ,data=dat,kernel="linear",cost=10)
table(out$fitted,dat$y)

dat.te = data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te = predict(out,newdata=dat.te)
table(pred.te,dat.te$y)




