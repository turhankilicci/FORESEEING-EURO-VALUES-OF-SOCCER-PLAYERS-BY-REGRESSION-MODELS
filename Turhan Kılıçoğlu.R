mydata = read.csv2("mydata.csv",sep=",",dec=".")
mydata = mydata[,-1:-2]
mydata = mydata[is.na(mydata$gk),-ncol(mydata)]
View(mydata)
names(mydata)
options(scipen = 999)

library(PerformanceAnalytics)
library(glmnet)
library(tree)
library(randomForest)

train = mydata[1:1000,]
test = mydata[1001:nrow(mydata),]

multi.fit = lm(eur_value~.,data = train)
summary(multi.fit)
test.pre = predict(multi.fit, newdata = test)

MSE = (1/nrow(test))*sum((test$eur_value - test.pre)^2)
MSE

windows()
my_data = mydata[, c(1,8,9,31)]
chart.Correlation(my_data, histogram = TRUE, pch = 19)

#glmnet

set.seed(1)
x = model.matrix(eur_value~.,data = train)[,-1]
x_test = model.matrix(eur_value~.,data = test)[,-1]
grid = 10 ^ seq(10,-2,length = 100)
ridge.glm = glmnet(x, train$eur_value, alpha = 0, lambda = grid)

cv.glm = cv.glmnet(x, train$eur_value, alpha = 0, lambda = grid)
wowlam.ridge = cv.glm$lambda.min
wowlam.ridge
plot(cv.glm)

ridge.pre = predict(ridge.glm, s = wowlam.ridge, newx = x_test)
MSE2 = mean((ridge.pre - test$eur_value)^2)
MSE2

set.seed(1)
ridge.lasso = glmnet(x, train$eur_value, alpha = 1, lambda = grid)
cv.lasso = cv.glmnet(x, train$eur_value, alpha = 1, lambda = grid)
wowlam.lasso = cv.lasso$lambda.min
wowlam.lasso
plot(cv.lasso)

predict(ridge.lasso, s = wowlam.lasso, type = "coefficients")
plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)

lasso.pre = predict(ridge.lasso, s = wowlam.lasso, newx = x_test)
MSE3 = mean((lasso.pre - test$eur_value)^2)
MSE3

#tree

set.seed(1)
tree.mydata = tree(eur_value~., data = train)
summary(tree.mydata)

cv.mydata = cv.tree(tree.mydata)
plot(cv.mydata$size, cv.mydata$dev, type = 'b')

prune.mydata = prune.tree(tree.mydata, best = 11)
plot(prune.mydata)
text(prune.mydata, pretty = 0)

yhat = predict(prune.mydata, newdata = test)
MSE4 = mean((yhat - test$eur_value)^2)
MSE4

#randomForest

set.seed(1)
cvtrain = train[sample(nrow(train)),]
folds = cut(seq(1,nrow(cvtrain)), breaks = 10, labels = FALSE)

mse = rep(NA, 10)

for (l in 1:35){
  for(i in 1:10){
    set.seed(1)
    testIndexes = which(folds==i,arr.ind=TRUE)
    testData = cvtrain[testIndexes, ]
    trainData = cvtrain[-testIndexes, ]
    
    model = randomForest(eur_value~., data = trainData,
                         mtry = l, n.trees = 500)
    
    yhat = predict(model, newdata = testData, n.trees = 500)
    mse[i] = mean((yhat - testData$eur_value)^2)
    
   }
  print(mean(mse))
} 

set.seed(1)
rf.train = randomForest (eur_value~., data = train,
                         importance = TRUE, mtry = 15)

windows()
importance(rf.train)
varImpPlot(rf.train)

yhat.rf = predict(rf.train, newdata = test)
MSE5 = mean((yhat.rf - test$eur_value)^2)
MSE5