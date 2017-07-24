library(ISLR)

#-------------------------------------------------------polynominal regression

#raw=T makes direct coef of age, age^2, age^3, age^4
fit = lm(wage~poly(age,4,raw=T),data=Wage) 
summary(fit)

age.grid=seq(min(Wage$age),max(Wage$age))
preds = predict(fit,list(age=age.grid),se=T)
se.bands= cbind(preds$fit+ 2*preds$se.fit,preds$fit - 2*preds$se.fit)
  
#par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(Wage$age,Wage$wage,cex=.5,col="darkgrey")
title("Wage~age 4 poly")#,outer = T)
lines(age.grid,preds$fit,lwd=2,col="red")
matlines(age.grid,se.bands,lwd=1,lty=3,col="darkred")

fit.1 = lm(wage~age,data=Wage) 
fit.2 = lm(wage~poly(age,2),data=Wage) 
fit.3 = lm(wage~poly(age,3),data=Wage) 
fit.4 = lm(wage~poly(age,4),data=Wage) 
fit.5 = lm(wage~poly(age,5),data=Wage) 
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

coef(summary(fit.5))

#logistic poly
fit = glm(I(wage>250)~poly(age,4),data=Wage,family = binomial)
preds = predict(fit, newdata = list(age =age.grid),se=T)
#notice logistic format, transfer needed
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands = cbind(preds$fit + 2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands = exp(se.bands)/(1+exp(se.bands))

#plot
plot(Wage$age,I(Wage$wage>250),ylim=c(0,.2),type="n")
points(jitter(Wage$age),I((Wage$wage>250)/5),cex=0.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="red")
matlines(age.grid,se.bands,lwd=1,lty=3,col="darkred")

#------------------------------------------------ step function
table(cut(Wage$age,4))
fit = lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

age.grid=seq(min(Wage$age),max(Wage$age))
preds = predict(fit,list(age=age.grid),se=T)
se.bands= cbind(preds$fit+ 2*preds$se.fit,preds$fit - 2*preds$se.fit)

#plot
plot(Wage$age,Wage$wage,cex=.5,col="darkgrey")
title("Wage~age step function")#,outer = T)
lines(age.grid,preds$fit,lwd=2,col="red")
matlines(age.grid,se.bands,lwd=1,lty=3,col="darkred")

#------------------------------------------------ regression spline
library(splines)
# func bs generate spline
attr(bs(Wage$age,df=6),"knots")

fit = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
age.grid=seq(min(Wage$age),max(Wage$age))
pred = predict(fit,newdata = list(age=age.grid),se=T)

plot(Wage$age,Wage$wage,col="grey")
lines(age.grid,pred$fit,lwd=2,col="red")
lines(age.grid,pred$fit+2*pred$se,lty="dashed",col="red")
lines(age.grid,pred$fit-2*pred$se,lty="dashed",col="red")
title("Regression Spline knots=3")

#natural spline
fit = lm(wage~ns(age,df=4),data=Wage)
pred = predict(fit,newdata = list(age=age.grid),se=T)
lines(age.grid,pred$fit,col="blue",lwd=2)

#------------------------------------------------ smooth spline
fit =smooth.spline(Wage$age,Wage$wage,df=16)   #lambda for df=16
fit2 =smooth.spline(Wage$age,Wage$wage,cv=T)   #lambda from cv

fit2$df

plot(Wage$age,Wage$wage,col="grey")
title("Smoothing spline")
lines(fit,lwd=2,col="red")
lines(fit2,lwd=2,col="blue")
legend("topright",legend = c("16df",paste(round(fit2$df,2),"df",sep="")),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#------------------------------------------------ GAM
library(gam)

gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
gam.m3 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(2,2))
plot(gam1,se=T,col="darkgreen")
par(mfrow=c(1,3))
plot(gam.m3,se=T,col="blue")

gam.m1 = gam(wage~s(age,5)+education,data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education,data=Wage)

anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m3)

#----------  local linear reg
gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo,se=T,col="green")

gam.lo.i = gam(wage~  lo(year,age,span=0.5)+education, data=Wage)

library(akima)
#plot 3d surface
plot(gam.lo.i)

#logistic GAM
gam.lr = gam(I(wage>250)~ year + s(age,df=5)+education,family=binomial,data=Wage)
plot(gam.lr,se=T,col="green")

table(Wage$education,I(Wage$wage>250))
#no high income for <HS modify the model
gam.lr = gam(I(wage>250)~ year + s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr,se=T,col="green")


