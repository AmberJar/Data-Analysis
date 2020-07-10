# Title     : TODO
# Objective : TODO
# Created by: User
# Created on: 10/07/2020

if (sigma < 0) {
profit_i <- profit_j
}else if (runif(1,0,1) < exp(-sigma/T)){
profit_i <- profit_j
}
n <- n + 1
if(!(profit_i == profit_j)){
t = t + 1
}
if(t >= 20 | n >= 100){
break
}
}
T <- 0.97*T
if (T <= 10){
break
}
}
bestprofits5[k,3] <- profit_i
}
for (k in 1:20){
dataset <- MKP_data[c((20 * k-19) : (20 * k)),]
#intial sloution i
profit_i <- 0
c1 <- 0
c2 <- 0
c3 <- 0
i <- sample(1:20,20,replace = FALSE)
for (j in i) {
if (c1 + dataset[j,2] <= capacity[1,2] &
c2 + dataset[j,3] <= capacity[1,3] &
c3 + dataset[j,4] <= capacity[1,4] ) {
c1 <- c1+dataset[j,2]
c2 <- c2+dataset[j,3]
c3 <- c3+dataset[j,4]
profit_i <- profit_i +dataset[j,1]
}else{
break
}
}
profit_i
neb <- 0
#assign initial temperature
T <- 10000
#temperature change counter
#start loop for Simulated Annuealing
repeat {
n <- 0
t <- 0
repeat{
#for compute profit_j
c1 <- 0
c2 <- 0
c3 <- 0
profit_j <- 0
neb <- matrix(NA,20,1)
neb <- i - sample(-3:3,20,replace = TRUE)
neb1 <- matrix(20,1)
for (i in 1:20){
if (0 < neb[i] & neb[i] < 20 & !(neb[i] %in% neb1[1:i])){
neb1[i] <- neb[i]
}else{
a <- as.vector(na.omit(neb1))
b <- 1:20
c <- b[-a]
d <- abs(neb[i] - c)
u <- which(d == min(abs(neb[i]-c)),arr.ind = TRUE)
neb1[i] <- c[u]
}
}
i <- as.vector(neb1)
for (j in i) {
if (c1+dataset[j,2] <= capacity[1,2] &
c2+dataset[j,3] <= capacity[1,3] &
c3+dataset[j,4] <= capacity[1,4]) {
c1 <- c1 + dataset[j,2]
c2 <- c2 + dataset[j,3]
c3 <- c3 + dataset[j,4]
profit_j <- profit_j +dataset[j,1]
}else{
break
}
}
#main body of Simulated Annuealing
sigma <- profit_i - profit_j
if (sigma < 0) {
profit_i <- profit_j
}else if (runif(1,0,1) < exp(-sigma/T)){
profit_i <- profit_j
}
n <- n + 1
if(!(profit_i == profit_j)){
t = t + 1
}
if(t >= 20 | n >= 100){
break
}
}
T <- 0.97*T
if (T <= 10){
break
}
}
bestprofits5[k,4] <- profit_i
}
bestprofits5 <- as.data.frame(bestprofits5)
bestprofits5$Average <- mean(bestprofits5)
bestprofits5$Average <- mean(bestprofits5[,1:4])
bestprofits5$Average <- mean(bestprofits5[,1]+bestprofits5[,2])
bestprofits5$Average <- mean(bestprofits5[,1]+bestprofits5[,2]+bestprofits5[,3]+bestprofits5[,4])
bestprofits5$Average <- (bestprofits5[,1]+bestprofits5[,2]+bestprofits5[,3]+bestprofits5[,4])/4
library(readr)
scanner_data <- read_csv("C:/Users/User/Desktop/583/scanner_data.csv")
View(scanner_data)
dataset <- scanner_data
dataset$volume <- dataset[,2]/dataset[,3]
view(dataset)
View(dataset)
model1 <- lm(volume~., data = dataset)
dataset <- as.data.frame(dataset)
model1 <- lm(volume~., data = dataset)
model1 <- lm(volume.sales~., data = dataset)
View(dataset)
dataset <- scanner_data
dataset$volume <- dataset[,2]/dataset[,3]
View(dataset)
dataset <- scanner_data
dataset$Volume <- dataset[,2]/dataset[,3]
View(dataset)
model1 <- lm(Volume.sales~., data = dataset)
attach(dataset)
model1 <- lm(Volume., data = dataset)
model1 <- lm(Volume~., data = dataset)
dataset <- scanner_data
dataset$Volume <- dataset$sales/dataset$price
View(dataset)
dataset <- as.data.frame(dataset)
model1 <- lm(Volume~., data = dataset)
summary(model1)
model2 <- lm(volume ~ price + display_only + coupon_only + both_promotions)
model2 <- lm(Volume ~ price + display_only + coupon_only + both_promotions)
model2 <- lm(Volume ~ price + display_only + coupon_only + both_promotions)
model1 <- lm(Volume~., data = dataset)
model2 <- lm(Volume ~ price + display_only + coupon_only + both_promotions)
model2 <- lm(Volume ~ price + display_only + coupon_only + both_promotions, data =dataset)
summary(model2)
dataset$log_volume <- log(dataset$Volume)
dataset$log_price <- log(dataset$price)
View(dataset)
model3 <- lm(log(Volume) ~ log(price) + isplay_only + coupon_only + both_promotions, data =dataset)
model3 <- lm(log(Volume) ~ log(price) + display_only + coupon_only + both_promotions, data =dataset)
summary(model3)
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model3) + 2*sum(log(dataset$Volume))
scanner <- scanner_data
scanner$volume <- scanner$sales/scanner$price
View(scanner)
model1 <- lm(volume~., data = scanner)
summary(model1)
model2 <- lm(Volume ~ price + display_only + coupon_only + both_promotions, data =dataset)
summary(model2)
scanner$log_volume <- log(scanner$Volume)
scanner$log_price <- log(scanner$price)
model3 <- lm(log(Volume) ~ log(price) + display_only + coupon_only + both_promotions, data =dataset)
summary(model3)
AIC(model1)
AIC(model2)
AIC(model3) + 2*sum(log(scanner$volume))
######lag######
for (i in 2 : 124){
scanner$volume_lag[i] <- scanner$volume[i-1]
}
###AR####
ar.volume <- as.ts(scanner[,7],4)
tsdisplay(ar.volume)
######question3#######
install.packages("forecast")
ar.price <- as.ts(scanner[,3],4)
tsdisplay(ar.price)
library("forecast")
install.packages("smooth")
library(smooth)
install.packages("tsutils")
library("tsutils")
tsdisplay(ar.price)
###AR####
ar.volume <- as.ts(scanner[,7],4)
tsdisplay(ar.volume)
######lag######
for (i in 3 : 124){
scanner$volume_lag[i] <- scanner$volume[i - 2]
}
scanner$volume_lag[1:2] <- 0
scanner$volume_lag[1:2] <- NA
for (i in 3 : 124){
scanner$price_lag[i] <- scanner$price[i-2]
}
for (i in 2 : 124){
scanner$price_lag[i] <- scanner$price[i-2]
}
for (i in 2 : 124){
scanner$price_lag[i] <- scanner$price[i-1]
}
for (i in 3 : 124){
scanner$price_lag[i] <- scanner$price[i-2]
}
scanner$price_lag[1:2] <- NA
for (i in 2 : 124){
scanner$coupou_lag[i] <- scanner$coupon_only[i-1]
}
for (i in 3 : 124){
scanner$coupou_lag[i] <- scanner$coupon_only[i-2]
}
scanner$coupou_lag[1:2] <- NA
for (i in 2 : 124){
scanner$both_lag[i] <-  scanner$both_promotions[i-1]
}
for (i in 2 : 124){
scanner$display_lag[i] <- scanner$display_only[i-1]
}
for (i in 3 : 124){
scanner$both_lag[i] <-  scanner$both_promotions[i-2]
}
scanner$both_lag[1:2] <- NA
for (i in 3 : 124){
scanner$display_lag[i] <- scanner$display_only[i-2]
}
scanner$display_lag[1:2] <- NA
##################
scanner[,8] <- NULL
T1 <- scanner[,-c(1,2)]
model4 <- lm(volume., data = T1)
model4 <- lm(Volume., data = T1)
View(T1)
model4 <- lm(volume., data = T1)
model4 <- lm(volume~., data = T1)
summary(model4)
AIC(mode4)
AIC(model4)
####################
scanner$log_volume_lag2 <- log(scanner$volume_lag)
scanner$log_price_lag2 <- log(scanner$price_lag)
####################
scanner$log_volume <- log(scanner$volume)
scanner$log_price <- log(sacnner$price)
scanner$log_price <- log(scanner$price)
View(scanner)
T2 <- scanner[,-c(1,2,3,7,8,9)]
model5 <- lm(volume~., data = T2)
model5 <- lm(log_volume~., data = T2)
summary(model5)
AIC(model5)
AIC(model5) + 2*sum(log(scanner$volume))
#######analysisi_of_model5#######
install.packages("car")
library("car")
durbinWatsonTest(model5)
ncvTest(model5)
spreadLevelPlot(model5)
vif(model5)
sqrt(vif(model5)) > 2
View(T2)
T2$d_log_price <- diff(T2$log_price)
T2$d_log_price <- diff(T2$log_price)
diff(T2$log_price)
diff(scanner$log_price)
T2$d_log_price <- NA
T2$d_log_price[1:123] <- diff(T2$log_price)
T2$d_log_price_lag2 < NA
T2$d_log_price_lag2 < 0
T2$d_log_price_lag2 <- 0
for (i in 3 : 124){
T2$d_log_price_lag2[i] <- T2$d_log_price[i-2]
}
scanner$display_lag[1:2] <- NA
model5 <- lm(log_volume~., data = T2[,-c(8,10)])
summary(model5)
AIC(model5) + 2*sum(log(scanner$volume))
vif(model5)
sqrt(vif(model5)) > 2
AIC(model5) + 2*sum(log(scanner$volume))
#################################
model6_display <- lm(log_volume~., data = T2[,-c(1,6,8,10)])
summary(model6_display)
AIC(model5_display) + 2*sum(log(scanner$volume))
AIC(model6_display) + 2*sum(log(scanner$volume))
model7_coupoin <- lm(log_volume~., data = T2[,-c(2,4,8,10)])
summary(model7_coupoin)
AIC(model7_coupoin) + 2*sum(log(scanner$volume))
model8_both <- lm(log_volume~., data = T2[,-c(3,5,8,10)])
summary(model8_both)
AIC(model8_both) + 2*sum(log(scanner$volume))
#################################
summary(model5)
View(T1)
AIC(model4)
AIC(model3) + 2*sum(log(scanner$volume))
durbinWatsonTest(model5)
#######analysisi_of_model5#######
install.packages("car")
library("car")
durbinWatsonTest(model5)
spreadLevelPlot(model5)
exp(1.408)
10^1.408
View(scanner)
10^1.408
log(10^1.408)
log(10)
log(2.3)
log(2.7)
exp(4.604)
?pnorm
qnorm(44,56,13,lower.tail = FALSE)
A <- qnorm(44,56,13,lower.tail = FALSE)
A
A <- qnorm(44,mean = 56,sd = 13,lower.tail = FALSE)
A
A <- qnorm(44,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
A <- qnorm(10,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
A <- qnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
A <- pnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
A
A <- pnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
A
pnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
pnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
pnorm(55,mean = 56,sd = 12,lower.tail = FALSE,log.p = FALSE)
pnorm(55,mean = 56,sd = 13,lower.tail = FALSE,log.p = FALSE)
?sample
?pnorm
rnorm(1,56,13)
rnorm(1,56,13)
rnorm(1,56,13)
rnorm(1,56,13)
profit <- matrix(1000,1)
profit <- matrix(1000,1)
for (i in 1:1000){
b <- 44
c <- 100
pd <- 100
pf <- 150
d <- rnorm(1,88,22)
f <- rnorm(1,56,13)
d <- min(d,b)
f <- min(f,c-d)
profit[i] <- pd * d + pf * f
d <- 0
f <- 0
}
View(profit)
mean(profit)
for (i in 1:1000){
b <- 61
c <- 100
pd <- 100
pf <- 150
d <- rnorm(1,88,22)
f <- rnorm(1,56,13)
d <- min(d,b)
f <- min(f,c-d)
profit[i] <- pd * d + pf * f
d <- 0
f <- 0
}
View(profit)
mean(profit)
install.packages(c("forecast", "greybox", "tseries"))
####normalize#####
normalize <- function(x){
return((x-min(x))/(max(x)-min(x)))
}
library('forecast')
library('tseries')
library(tsutils)
library('fpp2')
library('trend')
library('strucchange')
library('quantmod')
setwd("C:/Users/User/Desktop/523")
###input_data###
NN5_data <- read.csv('NN5_data.csv',header = TRUE)
sales <- NN5_data$NN3.Data
sales <- sales[1:735]
average <- mean(sales,na.rm = TRUE)
sales[is.na(sales)] <- average
data1 <- ts(sales,frequency = 7)
sales_data <- NN5_data[1:735,1:2]
sales_data[,2] <- data1
sales_data$Monday <- NA
sales_data$Monda <- NA
sales_data$Mony <- NA
sales_data$day <- NA
sales_data$Mnday <- NA
sales_data$Mday <- NA
sales_data$May <- NA
colnames(sales_data) <- c('Date','Sales',"Mon","Tues","Wed","Thur","Fri","Sat","Sun")
for (i in 1:105){
for (j in 1:7){
sales_data[j+(i-1)*7,j+2] <- 1
}
}
sales_data[is.na(sales_data)] <- 0
####missing_data####
View(sales)
sales_data$org <- NN5_data$NN3.Data[1:735]
sales_data$missing <- NA
for (i in 1:735){
if (is.na(sales_data$org[i])){
sales_data$missing[i] <- 1
}else(sales_data$missing[i] <- 0)
}
sum(sales_data$missing)
sales_data$org <- NULL
####modelling####
train_lr <- sales_data[1:679,-1]
test_lr <- sales_data[680:735,-1]
####outlier########
a <- sd(train_lr$Sales)
b <- mean(train_lr$Sales)
sales_data$outliers <- NA
for (i in 1:735){
if ((sales_data[i,2] - b)/a > 1.96 | (sales_data[i,2] - b)/a < -1.96 ){
sales_data$outliers[i] <- 1
}else{sales_data$outliers[i] <- 0}
}
sum(sales_data$outliers)
sales_data$outliers <- factor(sales_data$outliers)
sales_data$lag1[2:735] <- data1[1:734]
sales_data$lag6[7:735] <- data1[1:729]
######diff#####
sales_data$dl <- NA
sales_data$dl[1:734] <- diff(data1)
####modelling####
train_lr <- sales_data[1:679,-1]
test_lr <- sales_data[680:735,-1]
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
missing + outliers + lag1 + lag6 + dl
, data = train_lr)
summary(lr_model)
lr_fit <- predict(lr_model)
lr_fit
write.table(lr_fit,"sample.csv",sep=",")
write.table(lr_fc,"sample.csv",sep=",")
lr_fc <- predict(lr_model,test_lr)
write.table(lr_fc,"sample.csv",sep=",")
write.table(lr_fc,"sample1.csv",sep=",")
test_lr$dl[56] <- rnorm(1,mean(dl2),sd = sd(dl2)) + test_lr$dl[55]
dl <- train_lr$dl
test_lr$dl[56] <- rnorm(1,mean(dl),sd = sd(dl)) + test_lr$dl[55]
lr_fc
lr_fc <- predict(lr_model,test_lr)
lr_fc
write.table(lr_fc,"sample2.csv",sep=",")
######FINAL-FORECAST#####
final_forecast <- matrix(NA,70,13)
colnames(final_forecast) <-  c('Sales',"Mon","Tues","Wed","Thur","Fri","Sat","Sun",
'missing','outliers','lag1','lag6','dl')
as.data.frame(final_forecast)
as.data.frame(test_lr[1:56,])
A <- rbind(test_lr,final_forecast)
View(A)
######FINAL-FORECAST#####
final_forecast <- matrix(NA,14,13)
colnames(final_forecast) <-  c('Sales',"Mon","Tues","Wed","Thur","Fri","Sat","Sun",
'missing','outliers','lag1','lag6','dl')
A <- rbind(test_lr,final_forecast)
View(A)
for (i in 1:2){
for (j in 1:7){
A[56 + j+(i-1)*7,j+1] <- 1
}
}
A[,2:8][is.na(A[,2:8])]<- 0
missing_prob <- mean(sales_data$missing)
outlier_prob <- mean(as.numeric(sales_data$outliers)-1)
dl <- train_lr$dl
for (i in 1:14){
if (runif(1,0,1) > missing_prob){
A[56 + i,9] <- 0
}else{A[56 + i,9] <- 1}
if (runif(1,0,1) > outlier_prob){
A[56 + i,10] <- 0
}else{A[56 + i,10] <- 1}
A[56 + i, 11] <- A[56 + i - 1,1]
A[56 + i, 12] <- A[56 + i - 6,1]
A[56 + i, 13] <- rnorm(1,mean(dl),sd = sd(dl))
A[56 + i, 1] <- predict(lr_model,A[56 + i,])
}
write.table(A[57:70,1],"sample3.csv",sep=",")
autoplot(train_lr) + autolayer(lr_fit)
autoplot(ts(train_lr$Sales)) + autolayer(ts(lr_fit))
autoplot(ts(test_lr$Sales)) + autolayer(ts(lr_fc))
autoplot(ts(A[57:70,]))
autoplot(ts(A[57:70,1]))
train <- ts(train_lr$Sales)
train <- ts(train_lr$Sales)
test <-ts(test_lr$Sales)
forecasts <- ts(A[57:70,1])
fitted <- lr_fit
predicted <- lr_fc
autoplot(train) + autolayer(fitted)
train <- ts(train_lr$Sales)
test <-ts(test_lr$Sales)
forecasts <- ts(A[57:70,1])
fitted <- ts(lr_fit)
predicted <- ts(lr_fc)
autoplot(train) + autolayer(fitted)
autoplot(test) + autolayer(predicted)
autoplot(forecasts)
