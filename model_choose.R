library(ISLR)
library(leaps)

#------------------------subset
Hitters<- na.omit(Hitters)

regfit.full<- regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.sum<- summary(regfit.full)

plot(reg.sum$rsq,type="b",main="R^2")

par(mfrow=c(2,2))
plot(reg.sum$rss,type="l",xlab="Num of Variables",ylab="RSS")

plot(reg.sum$adjr2,type="l",xlab="Num of Variables",ylab="Adjusted R2")
which.max(reg.sum$adjr2)
points(11,reg.sum$adjr2[11],col="red",cex=2,pch=20)

plot(reg.sum$cp,type="l",xlab="Num of Variables",ylab="Cp")
which.min(reg.sum$cp)
points(10,reg.sum$cp[10],col="red",cex=2,pch=20)

plot(reg.sum$bic,type="l",xlab="Num of Variables",ylab="BIC")
which.min(reg.sum$bic)
points(6,reg.sum$bic[6],col="red",cex=2,pch=20)

dev.off()

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# subset forward & backward
regfit.fwd<- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd<- regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

#-------------------train test set
train=sample(c(T,F),nrow(Hitters),rep=T)
test= !train

regfit.best<- regsubsets(Salary~. ,data=Hitters[train,],nvmax=19)
test.mat<- model.matrix(Salary~.,data=Hitters[test,])

val.errors<- rep(NA,19)
for (i in 1:19){
  coefi<- coef(regfit.best,id=i)
  pred<- test.mat[,names(coefi)]%*%coefi
  val.errors[i]= mean((Hitters$Salary[test]-pred)^2)
}
plot(val.errors,type="b")

#------------------cross validation
predict.regsubsets<- function(object,newdata,id,...){
  form<- as.formula(object$call[[2]])
  mat<- model.matrix(form,newdata)
  coefi<- coef(object,id=id)
  return(mat[,names(coefi)]%*%coefi)
}

k=10
folds = sample(1:k,nrow(Hitters),replace = T)
cv.errors<- matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for (j in 1:k){
  best.fit<- regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for (i in 1:19){
    pred<- predict.regsubsets(best.fit ,Hitters[folds==j,] ,id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors<- apply(cv.errors,2,mean)
plot(mean.cv.errors,type="b")

#whole set after cv
reg.best<- regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best, which.min(mean.cv.errors))


#-------------------------------PCR PLS
library(pls)
x<- model.matrix(Salary~.,Hitters)[,-1] #convert factor to dummy variable
y<- Hitters$Salary

pcr.fit<- pcr(Salary~.,data=Hitters,scale=T,validation="CV") #10 fold-cv

validationplot(pcr.fit,val.type = "MSEP")

#test and train set
pcr.fit<- pcr(Salary~.,data=Hitters,subset=train,scale=T,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred<- predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y[test])^2)

pcr.fit<- pcr(y~x,scale=T,ncomp=7)
summary(pcr.fit)

#pls
pls.fit<- plsr(Salary~.,data=Hitters,subset=train,scale=T,validation="CV") #10 fold-cv
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")

pls.pred<- predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y[test])^2)

pls.fit<- plsr(y~x,scale=T,ncomp=2)
summary(pls.fit)


