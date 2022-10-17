########3333 Assignment 3#########
##################################

data = c(1,2,3,4,5,2,3,3,5,5)
c1 = matrix(data, nrow = 5, ncol = 2, byrow = FALSE)

data = c(1,2,3,3,5,6,0,1,1,2,3,5)
c2 = matrix(data, nrow = 6, ncol = 2, byrow = FALSE)
mu_1 = c(mean(c1[,1]),mean(c1[,2]))
mu_2 = c(mean(c2[,1]),mean(c2[,2]))
S_1 = 4*cov(c1)
S_2 = 5*cov(c2)
S_w = S_1 + S_2

###Solution ####
################
#### Question 1 ####
data = c(2,3,4,5,6,3,7,8,12,10)
c1 = matrix(data,nrow = 5, ncol = 2, byrow = FALSE)


data = c(2,3,4,5,6,7,1,2,2,3,4,6)
c2 = matrix(data, nrow = 6, ncol = 2, byrow = FALSE)

#Part A
mu_1 = c(mean(c1[,1]),mean(c1[,2]))
mu_2 = c(mean(c2[,1]),mean(c2[,2]))
cbind(mu_1, mu_2)

#Part B
S_1 = 4*cov(c1)
S_2 = 5*cov(c2)
S_w = S_1 + S_2

#Part C
S_w_inv = solve(S_w)
v = t(S_w_inv%*%(mu_1 - mu_2))


#Part D
(1/2)*v%*%(mu_1 + mu_2)

#Part E
new = c(5,3)

v%*%new

### Question 2 ####
x <- c( 1, 0.2, 0.06, 0.11)
intercept = c(1.613703, 3.444128, 0.999448, 0.067163, 0.339579)
na = c(-2.483557, -2.031676, -1.409505, -2.382624, 0.151459)
mg = c( 3.842907,  1.697162, 3.29135, 0.051466,  0.699274)
al = c(-3.719793, -1.704689, -3.006102 ,0.26351, -1.394559)
theta = cbind(intercept, na, mg, al)

xtheta = theta%*%x
sum = 1/ (1 + exp(xtheta[1])
                  + exp(xtheta[2])
                        + exp(xtheta[3])
                              + exp(xtheta[4])
                                    + exp(xtheta[5]))
p_i = rep(0,5)


for (i in 1:5){
  p_i[i] = exp(xtheta[i]) * sum
}
round(p_i,4)
data.frame(p_i)


### Question 3 ####
mu_1 = c(2,-1)
mu_2 = c(4,3)
mu_3 = c(2,3)

p_C1 = 1/4
p_C2 = 1/4
p_C3 = 1/2

x = c(0.5, 0.4)

g1 = 1/2*(t(mu_1)%*%x) - 1/(2*2)*(t(mu_1)%*%mu_1) + log(1/4)
g2 = 1/2*(t(mu_2)%*%x) - 1/(2*2)*(t(mu_2)%*%mu_2) + log(1/4)
g3 = 1/2*(t(mu_3)%*%x) - 1/(2*2)*(t(mu_3)%*%mu_3) + log(1/2)
cbind(g1, g2, g3)




#### Question 4 ####
df = read.csv('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 3333/3. Assessments/Assignments/Assignment 3/student-mat.csv', sep = ';')
View(df)
library(MASS)
set.seed(10000)

#Linear discriminant model
model1 = lda(schoolsup ~ G1 + G2 + G3, data = df)
model1
#Quadratic discriminant model
model2 = qda(schoolsup ~ G1 + G2 + G3, data = df)
model2

rep = 5
errlin = dim(rep)
errqua = dim(rep)

for (i in 1: 5){
training = sample(1:395, 300)
trainingset = df[training,]
testingset = df[-training,]
# linear discriminant analysis
m1 = lda(schoolsup ~ G1 + G2 + G3, data = trainingset)
pred_lin = predict(m1, testingset)$class
tablin = table(testingset$schoolsup, pred_lin)
errlin[i] = (95 - sum(diag(tablin)))/95
# quadratic discriminant analysis
m2 = qda(schoolsup ~ G1 + G2 + G3, data = trainingset)
pred_quad = predict(m2, testingset)$class
tablquad = table(testingset$schoolsup, pred_quad)
errqua[i] = (95 - sum(diag(tablquad)))/95
}
merrlin = mean(errlin)
merrlin
merrqua = mean(errqua)
merrqua




