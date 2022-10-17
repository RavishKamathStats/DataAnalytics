#### MATH 3333 #####
####Assignment 2 ####

#### Question 1 ####
x<-(1:1000)/100
y<-numeric(1000)
for (i in 1:1000){
  y[i] = (3*(x[i])^2 - 1) / (1 + (x[i])^3)
}
plot(x,y,type="l")

fp = function(x){
  p = (-3*(x^4) + 3*(x^2) + 6*(x)) / (1+x^3)^2
  return(p)
}


fpp<-function(x){
  p = 6*(x^6 - 2*(x^4) - 7*(x^3)+ x + 1) / (1 + x^3)^3
  return(p)
}


###Newton:the starting  is 1.5

diff<-4
iter<-0

x<-1
while ((diff>0.001) && (iter<30)) {
  
  oldx<-x
  x<-x-fp(x)/fpp(x)
  diff<-abs(x-oldx)
  iter<-iter+1
  print(c(iter,diff))
}


#### Question 2 ####
#Data Frame
year = seq(1,9,1)
sales = c(61, 73, 85, 106, 120, 129, 142, 144, 161)
advertisement = c(19, 26, 30, 34, 43, 48, 52, 57, 68)
df = data.frame(year, sales, advertisement)
ones = c(rep(1,9))
X = cbind(ones, advertisement)
XtY = t(X)%*%sales
XtX = t(X)%*%X

# Part A
#Ridge Regression: Lambda = 0.1
lambda = 0.1*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta

#lambda = 0.01
lambda = 0.01*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta

#lambda = 2
lambda = 2*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta

#lambda = 10
lambda = 10*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta

#lambda = 100
lambda = 100*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta


#lambda = 10000
lambda = 10000*diag(2)
XtXinv = solve(XtX + lambda)
ridge_theta = XtXinv%*%XtY
ridge_theta


# Part B 
#when we increase lambda to a large number, we can safely assume that the coefficients tend to 0.
# 

lasso <-lars(x=as.matrix(df[,3]),y=as.numeric(unlist(df[,2])),trace=TRUE)
coef(lasso,s=0.8,mode="fraction")





#### Question 3 ####

#Part A
discovery<-0
for (i in 1:1000){
  x<-rnorm(30,0,1)
  ztest<-mean(x)/sqrt(var(x)/30)
  discovery<-discovery+(ztest>=1.645)
} 
discovery


#Part B
discovery<-0
for (i in 1:1000){
  x<-rnorm(30,0,1)
  ztest<-mean(x)/sqrt(var(x)/30)
  discovery<-discovery+(ztest>= 1.645/1000)
} 
discovery


# Part C
#Let us say that we have a huge data set with alot of predictors. Then we test out all the predictors to see
#whether each predictor is significant to the model or not. When we test each of the coefficients at lets say
#a 5% level of significance, we increase our type 1 error each time we reject a predictor. This means that we can 
#end up with a huge false positive rate (also known as false discovery rate (FDR)). When we do a sequence of tests
#at certain level of significance, we are bound to run into problms as we may end up with too many false positives. 
#This is also known as a multiplicity problem. 


#### Question 4 ####
#Please refer to handwritten notes


#### Question 5 ####

#German Credit Data
credit = read.csv(
  '/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/Statistics /MATH 3333/1. Lectures/6. False Discovery Rate and Logistic Regression/R/germancredit.csv')

#Making Dummy Variables for non-numeric variables
index = c(1,2,4,5,7,8,10,11,13,15,16,18,20,21)
for( i in index){
  credit[,i] = factor(credit[,i])
}
#Full Logistic Model
logist_fit = glm(Default~., data=credit,family=binomial(link=logit))
summary(logist_fit)

#Predicting Default Status 
for (j in 1:1){
  training = sample(1:1000, 900)
  trainingset = credit[training,]
  testingset = credit[-training,]
  model = glm(Default ~ duration + amount + installment + age
              + history + purpose + foreign + housing
              , data = trainingset, family = binomial(link = logit))
  pred = predict.glm(model, new = testingset)
  predprob = exp(pred)/(1 + exp(pred))
}


summary(model)
#Regression Coefficient Interpretation
# When taking a look at the installment regression coefficient for our full logistic model, 
#we can see that our Beta_1 = 0.2573. This implies that a change from x_1 t- x_1 + 1 will 
#change the odds of occurrence by a factor of exp(0.2573) = 1.293433. 
# It increases the odds by 100(1.293433 - 1) = 29.34%




predstatus = predprob >= 0.1666666666666
# Here we choose our cut off rate to be 1/6 to be conservative with our testing.


tp = sum((predstatus == 1) * (testingset$Default == 1))
tn = sum((predstatus == 0) * (testingset$Default == 0))
fp = sum((predstatus == 1) * (testingset$Default == 0))
fn = sum((predstatus == 0) * (testingset$Default == 1))
misrate = (fp + fn)/100
# Here we get our false negative to be 2, and our false positive to be 41. 

#Adding more predictors to our model
for (j in 1:1){L
  training = sample(1:1000, 900)
  trainingset = credit[training,]
  testingset = credit[-training,]
  model = glm(Default ~ duration + amount + installment + age
              + history + purpose + foreign + housing + residence + job
              , data = trainingset, family = binomial(link = logit))
  pred = predict.glm(model, new = testingset)
  predprob = exp(pred)/(1 + exp(pred))
}
#summary(model)
predstatus = predprob >= 0.1666666666666
tp = sum((predstatus == 1) * (testingset$Default == 1))
tn = sum((predstatus == 0) * (testingset$Default == 0))
fp = sum((predstatus == 1) * (testingset$Default == 0))
fn = sum((predstatus == 0) * (testingset$Default == 1))
misrate = (fp + fn)/100







#### Question 6 ####
#Please refer to handwritten notes

#Part D
theta = matrix(c(0.1,0.2,0.3), ncol = 3)
x_new = matrix(c(3,2,4), nrow = 3)
p_i = theta%*%x_new



#### REDUNDANT CODE ####
# credit$Default = factor(credit$Default)
# credit$history = factor(credit$history, levels=c("A30","A31","A32",
#                                                  "A33","A34"))
# levels(credit$history) = c('good', 'good', 'poor', 'poor', 'terrible')
# credit$foreign = factor(credit$foreign, levels = c("A201","A202"), labels=c("foreign","german"))
# credit$rent <- factor(credit$housing=="A151")
# credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42",
#                                                   "A43","A44","A45","A46","A47","A48","A49","A410"))
# levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),
#                             "edu",NA, "edu","biz","biz")
# 
# credit1 <- credit[,c("Default","duration","amount","installment",
#                     "age","history","purpose","foreign","rent")]
# 
# Xcred = model.matrix(Default ~., data = credit1)[,-1]
# set.seed(1)
# train = sample(1:1000, 900)
# xtrain = Xcred[train,]
# xnew = Xcred[-train,]
# ytrain = credit$Default[train]
# ynew = credit$Default[-train]
# credglm = glm(Default ~., family = binomial, data.frame(Default = ytrain, xtrain))
# summary(credglm)
# ptest = predict(credglm, newdata = data.frame(xnew), type = 'response')
# gg1 = floor(ptest + (5/6))
# ttt = table(ynew, gg1)
# ttt
#  We get our false 
# error = (ttt[1,2]+ttt[2,1])/100
# error
