library(MASS) #Boston data QDA LDA
library(ISLR)  #Smarket
library(car) #vif
library(class) #KNN

#--------------------data: Boston house price
fix(Boston)

#single param regression
lm.fit = lm(medv ~ lstat, data = Boston)
summary(lm.fit)

plot(Boston$lstat,Boston$medv)
abline(lm.fit,col="red")

#plot everything of regression
par(mfrow=c(2,2))
plot(lm.fit)
dev.off()

#residuals and student resi
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

#leverage statistic
plot(hatvalues(lm.fit))
abline(h = 2/nrow(Boston),col="red")

#multi param regression
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
#all variables
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)

#vif
vif(lm.fit)

#all less one variable
lm.fit = lm(medv ~ .-age, data = Boston)
summary(lm.fit)
lm.fit = update(lm.fit,~.-age)

#add interactive term lstat*age = lstat+age+lstat:age
lm.fit = lm(medv ~ lstat*age, data = Boston)
summary(lm.fit)

#add nonlieaner term
lm.fit2 = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)
anova(lm.fit,lm.fit2)
#5 order polynominal
lm.fit5 = lm(medv ~ poly(lstat,5), data = Boston)
summary(lm.fit5)


#--------------------data: Carseats (dummy included)
fix(Carseats)

lm.fit = lm(Sales ~ . + Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

contrasts(Carseats$ShelveLoc)

#-------------------- data: Stock market SP500 
fix(Smarket)

# basic
summary(Smarket)
cor(Smarket[,-9]) 

#------------------- logistic
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data = Smarket, family=binomial)
summary(glm.fit)  

glm.probs = predict(glm.fit,type= "response") # predict prob
contrasts(Smarket$Direction)

glm.pred<- rep("Down",1250)
glm.pred[glm.probs>0.5]<- "Up"

table(glm.pred,Smarket$Direction)
#correct rate
mean(glm.pred == Smarket$Direction)

#training set and test set
train<- Smarket$Year<2005
Smarket.2005 <- Smarket[!train,]

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data = Smarket, family=binomial,subset = train)
summary(glm.fit)  

glm.probs = predict(glm.fit,Smarket.2005,type= "response") # predict prob

glm.pred<- rep("Down",252)
glm.pred[glm.probs>0.5]<- "Up"

table(glm.pred,Smarket$Direction[!train])
#correct rate
mean(glm.pred == Smarket$Direction[!train])

glm.fit <- glm(Direction ~ Lag1+Lag2 ,data = Smarket, family=binomial,subset = train)
glm.probs = predict(glm.fit,Smarket.2005,type= "response") # predict prob
glm.pred<- rep("Down",252)
glm.pred[glm.probs>0.5]<- "Up"
mean(glm.pred == Smarket$Direction[!train])

# -------- LDA
lda.fit<- lda(Direction ~ Lag1+Lag2,data = Smarket,subset= train)
plot(lda.fit)

lda.pred<- predict(lda.fit,Smarket.2005)
table(lda.pred$class,Smarket.2005$Direction)

mean(lda.pred$class==Smarket.2005$Direction)

# -------- QDA
qda.fit <- qda(Direction~Lag1+Lag2,data = Smarket,subset=train)
qda.fit
qda.pred <- predict(qda.fit,Smarket.2005)
mean(qda.pred$class==Smarket.2005$Direction)

#------------ KNN
train.X <- cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X<- cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.direction<- Smarket$Direction[train]

knn.pred<- knn(train.X,test.X,train.direction,k=3)
table(knn.pred,Smarket.2005$Direction)
mean(knn.pred==Smarket.2005$Direction)

#----------------data:Caravan KNN & logistic
fix(Caravan)
summary(Caravan$Purchase)
#standardized
stand.X<- scale(Caravan[,-86])
var(stand.X[,1])

test = 1:1000
train.X <- stand.X[-test,]
test.X<- stand.X[test,]
train.Y<- Caravan$Purchase[-test]
test.Y<- Caravan$Purchase[test]

knn.pred<- knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)

glm.fit <- glm(Purchase ~ . ,data = Caravan, family=binomial,subset = -test)
glm.probs = predict(glm.fit, Caravan[test,], type= "response") # predict prob
glm.pred<- rep("No",length(glm.probs))
glm.pred[glm.probs>0.5]<- "Yes"
table(glm.pred,test.Y) #very bad

glm.pred[glm.probs>0.25]<- "Yes"
table(glm.pred,test.Y)
