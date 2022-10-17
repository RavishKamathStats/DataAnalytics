#### Question 2 ####
X = c(-3, -6, -8, -7, -7, -9, 6, 6, 3.5, 6, 5, 6)
X = matrix(X, ncol = 2)
Xbar = t(colMeans(X))
Xstar = apply(X, 2, scale, scale=FALSE, center=TRUE)
covmat = cov(Xstar)
eigen_v = eigen(covmat)
w = eigen_v$vectors
w[,1]

y = w[,1]%*%Xstar[1,]
y

#### Question 5 ####
germandata$Default = factor(germandata$Default)
germandata$history = factor(credit$history, levels=c("A30","A31", + "A32","A33","A34"))
levels(germandata$history) = c("good","good","poor","poor", + " terrible")
germandata$foreign <- factor(germandata$foreign,levels=c("A201", + "A202"),labels=c("foreign","german"))
germandata$rent <- factor(germandata$housing=="A151")
germandata$purpose <- factor(credit$purpose,
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47", 
                                  "A48","A49","A410"))
levels(germandata$purpose) <-c("newcar","usedcar",rep("goods/repair",4),
                           "edu",NA,"edu","biz","biz")








library(e1071)
library(randomForest)
germandata = read.csv('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 3333/1. Lectures/6. False Discovery Rate and Logistic Regression/R/germancredit.csv')
set.seed(1)
n = nrow(germandata)
nt = 800
rep = 1000
error_SVM = dim(rep)
error_RF = dim(rep)
neval = n - nt
germandata$Default = factor(germandata$Default)

for (i in 1: rep) {
training = sample(1:n, nt)
trainingset = germandata[training,]
testingset = germandata[-training,]

# SVM Analysis
x = subset(trainingset, select = c('duration', 'amount', 'installment', 'age'))
y = trainingset$Default
xPrime = subset(testingset, select = c('duration', 'amount', 'installment', 'age'))
yPrime = testingset$Default

svm_model1 = svm(x,y)
pred_SVM = predict(svm_model1, xPrime)
tableSVM = table(yPrime, pred_SVM)
error_SVM[i] = (neval - sum(diag(tableSVM)))/neval
  
#Random Forest Analysis
rf_classifier = randomForest(Default ~., data = trainingset, type = classification, 
                             ntree = 100, mtry = 2, importance = TRUE)
prediction_RF = predict(rf_classifier, testingset)
table_RF = table(yPrime, prediction_RF)
error_RF[i] = (neval - sum(diag(table_RF)))/neval
}

mean(error_SVM)
mean(error_RF)










































# #WIINNNIIEEES
# set.seed(1)
# n      = nrow(credit)
# nt     = 800
# neval  = n - nt
# rep    = 1000
# errsvm = dim(rep)
# errrf  = dim(rep)
# for (k in 1: rep) {
#   train <- sample(1:n, nt)
#   datatrain <- credit[train, ]
#   x <- subset(datatrain, select = c('duration', 'amount', 'installment', 'age'))
#   y <- datatrain$Default
#   datatest <- credit[-train, ]
#   xtest <- subset(datatest, select = c('duration', 'amount', 'installment', 'age'))
#   ytest <- datatest$Default
#   
#   # svm
#   svmmodel  <- svm(x, y)
#   tabsvm    <- table(ytest, predict(svmmodel, xtest))
#   errsvm[k] <- (neval - sum(diag(tabsvm))) / neval
#   
#   # randomforest
#   rf  <- randomForest(Default ~., data = datatrain, ntree = 100, mtry = 2, importance=TRUE)
#   tabrf    <- table(ytest, predict(rf, datatest))
#   errrf[k] <- (neval - sum(diag(tabrf))) / neval
# }
# 
# merrsvm = mean(errsvm)
# merrsvm
# merrrf = mean(errrf)
# merrrf






















