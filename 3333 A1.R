attach(day)
attach(fishToxicity)
library(lattice)
trellis.device()
densityplot()
View(day)
View(fishToxicity)

####Question 1 ####

#part A
registeredMeans = aggregate(day$registered, by = list(day$weekday), FUN = function(x) {round(mean(x), digits = 0)})
days = matrix(c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'), byrow = T)
means = cbind(days, registeredMeans)
names(means) = c('Days', 'Days ID', 'Registered Mean')
means

#Part B
library(lattice)
densityplot(~registered|mnth, data = day, plot.points = FALSE, col = "black")

#Part C
df_brakes = tapply(day$registered, 
                   INDEX = list(cut(day$temp, breaks = 10), 
                                cut(day$hum, breaks = 10)), FUN = mean, na.rm = TRUE)
levelplot(df_brakes, scales = list(x = list(rot = 90)), main = '2D Levelplot of Registered', 
          xlab = 'temperature', ylab = 'humidity')

####Question 2 ####
#Part A
mod = lm(registered ~ temp + hum, data = day)
summary(mod)

#Part B
# I would say that weather situation and workingday would be good 
#predictors for modelling the variable casual. 
mod1 = lm(registered ~ temp + hum + workingday + weathersit, data = day)
summary(mod1)


#Part C
### CV for part a)
totalCVerror<-0
for ( j in 1:1){
  training<-sample(1:731,584)
  trainingset<-day[training,]
  testingset<-day[-training,]
  ###fill in the codes to get the predictions errors
  ### update the totalCVerror
  model<-lm(registered~temp+hum, data=trainingset)
  prediction<-predict(model, new = testingset)  
  errors<-sum((testingset$registered - prediction)^2)
  totalCVerror<-totalCVerror+errors
}
averageCVerrors_modA<-totalCVerror/100/147
averageCVerrors_modA

### CV for part b)
totalCVerror<-0
for ( j in 1:1){
  training<-sample(1:731,584)
  trainingset<-day[training,]
  testingset<-day[-training,]
  ###fill in the codes to get the predictions errors
  ### update the totalCVerror
  model<-lm(registered~temp+hum + workingday + weathersit, data=trainingset)
  prediction<-predict(model, new = testingset)  
  errors<-sum((testingset$registered - prediction)^2)
  totalCVerror<-totalCVerror+errors
}
averageCVerrors_modB<-totalCVerror/100/147
averageCVerrors_modB

#In this case, the model used in part B, has a better predictive power,
#since it has less errors. 

####Question 3 ####
#Part A
Y = c(34, 56, 65, 86, 109, 109, 122, 124, 131)
matrix(Y, ncol = 1, byrow = T)
#Part B
advertisement = c(23, 36, 45, 52, 53, 58, 63, 68, 70)
ones = c(rep(1,9))
X = cbind(ones, advertisement)
#Part C
XtransX = t(X)%*%X
#Part D
XtransY = t(X)%*%Y
theta = (solve(XtransX))%*%XtransY
theta
#Part E
mod = summary(lm(Y~X))
mod

#As you can see, when using the lm function vs. manually solving the
#Least Square method, we end up having the same model and coefficients.

#Part F

#Please check the handwritten notes


####Question 4####
#SEE paper notes for the answers

#Please check the handwritten notes for the answers

####Question 5 ####
#Part A
lasso <-lars(x=as.matrix(fishToxicity[,1:6]),y=as.numeric(unlist(fishToxicity[,7])),trace=TRUE)

#Based on the Output the following models will be listed based on the LASSO Method
# 1st Model: X_6 (MLOGP)
# 2nd Model: X_6 (MLOGP), X_2 (SM1_Dx(Z))
# 3rd Model: X_6 (MLOGP), X_2 (SM1_Dx(Z)), X_3(GATS1i)
# 4th Model: X_6 (MLOGP), X_2 (SM1_Dx(Z)), X_3(GATS1i), X_4(NdsCH)
# 5th Model: X_6 (MLOGP), X_2 (SM1_Dx(Z)), X_3(GATS1i), X_4(NdsCH), X_5 (NdssC)
# 6th Model: X_6 (MLOGP), X_2 (SM1_Dx(Z)), X_3(GATS1i), X_4(NdsCH), X_5 (NdssC), X_1 (CIC0)

#Part B
cv.lars(x=as.matrix(fishToxicity[,1:6]),y=as.numeric(unlist(fishToxicity[,7])),K=10)

# Based on looking at the plot, I would choose 0.8 as the best fraction s value.

#Part C
lasso <-lars(x=as.matrix(fishToxicity[,1:6]),y=as.numeric(unlist(fishToxicity[,7])),trace=TRUE)
coef(lasso,s=0.8,mode="fraction")


####Question 6 ####
# Part A
q = seq(0, .99, by = 0.01)
length(q)
y = 30 + 5*q + 2*q^2 + 3*q^3
noise = rnorm(length(q), mean = 0, sd = 1)
noisy.y = y + noise
plot(q, noisy.y, col = 'deepskyblue4', xlab = 'x', main = 'Observed Data')
lines(q, y, col = 'firebrick1', lwd = 3)
model <- lm(noisy.y ~ q + I(q^2) + I(q^3))
summary(model)
# We get the model to be 29.9287 + 0.5647x + 16.7805x^2 - 7.5869x^3 
#which is quite different from the true model

#We get the R squared for the estimated model to be 0.8886

#Part B
matdata = c(1, q[1], (q[1]^2), (q[1]^3), 1, q[2], (q[2]^2), (q[2]^3))
xMat = matrix(matdata, nrow = 2, ncol = 4, byrow = TRUE)
colnames(xMat) = c('1', 'x', 'x^2', 'x^3')
rownames(xMat) = c('row 1', 'row 2')
xMat

#Part C
model2 = lm(noisy.y ~ q + I(q^2) + I(q^3) + I(q^4))
summary(model2)
# We get the model to be 30.324 - 7.806x + 55.279x^2 - 68.280x^3 + 30.653x^4
#Once again, this is quite different from the true model in-terms of the coefficients 

# We get the R squared for the estimated model to be 0.8887.
#It has increased by a little, due to the fact that we have added one more regressor variable. 
#The more regressors we had, the bigger the R squared tends to 1. 

