# Loading Dataset
df = read.csv("F:/cars.csv")
View(df)
dim(df)

# Divide the data into train & test
set.seed(0)
rand = sample(1:nrow(df),350)
train = df[rand, ]
test = df[-rand, ]




# Qn.1 :- Choose a sample size of 20,30,40,50,70,100,200,350 from train
#----------------------------------------------------------------------------------------
# ----  Fit a polynomial regression of order 7
set.seed(0)
rand1 = sample(1:nrow(train),20) #Here only Iam changing the sample size
train1 = train[rand1, ]
test1 = test

m <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7), train1)
m
#        Plotting the model over data
plot(train1$Weight,train1$MPG, pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m)[order(train1$Weight)], col='red', type='l')
#Train & Test Accuracy
sum(m$residuals^2)
pred = predict(m, newdata=test1)
sum((pred-test1$MPG)^2)

sample_size = c(20,30,50,70,100,200,350)
test_error = c(2717233,2352026,16167.11,2404.403,1261.163,1053.24,1051.547)

plot(sample_size,test_error, pch=19, cex=0.5)
lines(sample_size,test_error, col = "red")

# Qn.2 :- Take 4 sets of sample(n=20) and fit regression of order 1,2,7,8,9,10 for each
#Repeat the same for 100 samples
#:- (a) FITTING A POLYNOMIAL REGRESSION OF ORDER 1

rand2 = sample(1:nrow(train),100)
train2 = df[rand2, ]
test2 = test


m1 <- lm(MPG ~ Weight, train2)
m1

#Plotting the model
plot(train2$Weight,train2$MPG, pch=19, cex=0.5)
lines(sort(train2$Weight), fitted(m1)[order(train2$Weight)], col='red', type='l') 

#Train & Test Error
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$MPG)^2)

#:- (b) FITTING A POLYNOMIAL REGRESSION OF ORDER 2

m2 <- lm(MPG ~ Weight+I(Weight^2), train2)
m2

#Plotting the model
plot(train2$Weight,train2$MPG, pch=19, cex=0.5)
lines(sort(train2$Weight), fitted(m2)[order(train2$Weight)], col='red', type='l') 

#Train & Test Error
sum(m2$residuals^2)
pred = predict(m2, newdata=test)
sum((pred-test$MPG)^2)

#:- (c) FITTING A POLYNOMIAL REGRESSION OF ORDER 7

m7 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7), train2)
m7

#Plotting the model
plot(train2$Weight,train2$MPG, pch=19, cex=0.5)
lines(sort(train2$Weight), fitted(m7)[order(train2$Weight)], col='red', type='l') 

#Train & Test Error
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$MPG)^2)

#:- (d) FITTING A POLYNOMIAL REGRESSION OF ORDER 8

m8 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8), train2)
m8

#Plotting the model
plot(train2$Weight,train2$MPG, pch=19, cex=0.5)
lines(sort(train2$Weight), fitted(m8)[order(train2$Weight)], col='red', type='l') 

#Train & Test Error
sum(m8$residuals^2)
pred = predict(m8, newdata=test)
sum((pred-test$MPG)^2)

#:- (e) FITTING A POLYNOMIAL REGRESSION OF ORDER 10

m10 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9)+I(Weight^10), train2)
m10

#Plotting the model
plot(train2$Weight,train2$MPG, pch=19, cex=0.5)
lines(sort(train2$Weight), fitted(m10)[order(train2$Weight)], col='red', type='l') 

#Train & Test Error
sum(m10$residuals^2)
pred = predict(m10, newdata=test)
sum((pred-test$MPG)^2)

#Vector corresponds to test error of each 4 sample size of 20 (without replacement)
Polynomial_Degree = c(1,2,7,8,10)
TestError1 = c(1632.457,1518.142,5545.208,341264.1,3600609)
TestError2 = c(1355.09,1218.002,2110.028,77167.75,166669.4)
TestError3 = c(1479.646,1336.53,1802.60,5334.19,34364.69)
TestError4 = c(1259.83,1104.96,35946.22,323447.8,2734758)

#Vector corresponds to test error of each 4 sample size of 100 (without replacement)
Polynomial_Degree = c(1,2,7,8,10)
TestError1 = c(1150.404,1016.401,1010.366,1034.792,1020.32)
TestError2 = c(1283.322,1180.92,1158.131,1162.136,1526.818)
TestError3 = c(1220.971,1104.357,1085.82,1091.99,1069.001)
TestError4 = c(1199.682,1076.775,1803.47,2472.09,4475.28)

# Plotting degree VS Test Error
#1
plot(Polynomial_Degree,TestError1, pch=19, cex=0.5)
lines(Polynomial_Degree,TestError1, col = "red")

#2
plot(Polynomial_Degree,TestError2, pch=19, cex=0.5)
lines(Polynomial_Degree,TestError2, col = "green")

#3
plot(Polynomial_Degree,TestError3, pch=19, cex=0.5)
lines(Polynomial_Degree,TestError3, col = "magenta")

#4
plot(Polynomial_Degree,TestError4, pch=19, cex=0.5)
lines(Polynomial_Degree,TestError4, col = "blue")

#Four graphs in a single plot
lines(Polynomial_Degree,TestError1, col = "red")+
  lines(Polynomial_Degree,TestError2, col = "green")+
  lines(Polynomial_Degree,TestError3, col = "magenta")+
  lines(Polynomial_Degree,TestError4, col = "blue")

#Qn 3 :- Plot a graph between model complexity and Test Error

#(a) FITTING A POLYNOMIAL REGRESSION OF ORDER 1
ml1 <- lm(MPG ~ Weight, train)
ml1

#Train & Test Error
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MPG)^2)

#(b) FITTING A POLYNOMIAL REGRESSION OF ORDER 2
ml2 <- lm(MPG ~ Weight+I(Weight^2), train)
ml2

#Train & Test Error
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MPG)^2)

#(c) FITTING A POLYNOMIAL REGRESSION OF ORDER 3
ml3 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3), train)
ml3

#Train & Test Error
sum(ml3$residuals^2)
pred = predict(ml3, newdata=test)
sum((pred-test$MPG)^2)

#(d) FITTING A POLYNOMIAL REGRESSION OF ORDER 4
ml4 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4), train)
ml4

#Train & Test Error
sum(ml4$residuals^2)
pred = predict(ml4, newdata=test)
sum((pred-test$MPG)^2)

#(e) FITTING A POLYNOMIAL REGRESSION OF ORDER 6
ml6 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6), train)
ml6

#Train & Test Error
sum(ml6$residuals^2)
pred = predict(ml6, newdata=test)
sum((pred-test$MPG)^2)

#(f) FITTING A POLYNOMIAL REGRESSION OF ORDER 7
ml7 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7), train)
ml7

#Train & Test Error
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MPG)^2)

#(g) FITTING A POLYNOMIAL REGRESSION OF ORDER 9
ml9 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9), train)
ml9

#Train & Test Error
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MPG)^2)

#(h) FITTING A POLYNOMIAL REGRESSION OF ORDER 13
ml13 <- lm(MPG ~ Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9)+I(Weight^10)+I(Weight^11)+I(Weight^12)+I(Weight^13), train)
ml13

#Train & Test Error
sum(ml13$residuals^2)
pred = predict(ml13, newdata=test)
sum((pred-test$MPG)^2)

#Vector corresponding to train & test error
model_complexity = c(1,2,3,4,6,7,9,13)
trainerror = c(9695.163,9202.983,9192.232,9184.446,9176.092,9175.713,9162.686,9128.99)
testerror = c(1177.555,1021.882,1020.879,1022.954,1032.916,1076.765,1051.547,1216.37)

#Plot Test error VS Model complexity

library("ggplot2")
RMSE = sqrt(trainerror/406)
p = sqrt(testerror/406)
q = p*2.9     #Ease of interpreting
newdf<-data.frame(model_complexity,RSS,q) 

ggplot(newdf, aes(model_complexity)) + 
  geom_line(aes(y = RMSE, colour = "TrainError")) + 
  geom_line(aes(y = u, colour = "TestError"))




