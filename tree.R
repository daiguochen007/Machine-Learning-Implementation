library(tree)
library(ISLR)

#------------------------------------------------------------ classification tree
High = ifelse(Carseats$Sales<=8,"No","Yes")
Carseats = data.frame(Carseats,High)

tree.carseats = tree(High~.-Sales,Carseats) #similar to lm
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty = 0,col="darkgreen")

tree.carseats

# test error
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High~.-Sales,Carseats,subset=train)
tree.pred = predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

# prune of trees
cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)
cv.carseats  #dev means err rate

plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats = prune.misclass(tree.carseats,best =9)
plot(prune.carseats)
text(prune.carseats,pretty = 0,col="darkgreen")

tree.pred = predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

#------------------------------------------------------------ regression tree
library(MASS)
train = sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston = tree(medv~.,Boston,subset = train)

summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty = 0,col="darkgreen")

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")

prune.boston = prune.tree(tree.boston,best =5)
plot(prune.boston)
text(prune.boston,pretty = 0,col="darkgreen")

yhat = predict(tree.boston,newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1,col="red")
mean((yhat-boston.test)^2)

#----------------------------------------------------------- bagging & random forest
library(randomForest)
bag.boston = randomForest(medv~.,data=Boston,subset=train,mtry=13,importance = T)
bag.boston

yhat.bag = predict(bag.boston,newdata = Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1,col="red")
mean((yhat.bag-boston.test)^2)

#rand forest
rf.boston = randomForest(medv~.,data=Boston,subset=train,mtry=6,importance = T)
rf.boston
yhat.rf = predict(rf.boston,newdata = Boston[-train,])
plot(yhat.rf,boston.test)
abline(0,1,col="red")
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)

#----------------------------------------------------------- boosting
library(gbm)

boost.boston = gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,interaction.depth = 4)
summary(boost.boston)
#price medv to variables
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

yhat.boost = predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)

#change lambda
boost.boston = gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,
                   interaction.depth = 4,shrinkage = 0.2)
yhat.boost = predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)




