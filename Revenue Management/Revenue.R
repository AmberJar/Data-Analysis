# Title     : TODO
# Objective : TODO
# Created by: User
# Created on: 10/07/2020

sales_data$lag3[4:735] <- data1[1:732]
sales_data$lag4[5:735] <- data1[1:731]
sales_data$lag5[6:735] <- data1[1:730]
sales_data$lag6[7:735] <- data1[1:729]
sales_data$lag7[8:735] <- data1[1:728]
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1 + lag2 + lag3 + lag4 + lag5 + lag6 + lag7, data = sales_data)
summary(lr_model)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1 + lag6 + lag7, data = sales_data)
summary(lr_model)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1 + lag6, data = sales_data)
summary(lr_model)
cor(sales_data, use = 'c')
cor(sales_data[,-1], use = 'c')
vif(lr_model)
tsdisplay(residuals(lr_model))
checkresiduals(lr_model)
summary(lr_model)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1, data = sales_data)
summary(lr_model)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag6, data = sales_data)
summary(lr_model)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1, data = sales_data)
tsdisplay(residuals(lr_model))
checkresiduals(lr_model)
acf2(data1)
acf2(lr_model)
ets_model1 <- ets(train,model = 'ANA',alpha = 0.1,gamma = 0.1,biasadj = FALSE,ic = 'aic',use.initial.values = TRUE)
ets_model1$fitted
ets_model_lr <- ets(data1,model = 'ANA',alpha = 0.1,gamma = 0.1,biasadj = FALSE,ic = 'aic',use.initial.values = TRUE)
ets_model1$fitted
ets_model_lr$fitted
sales_data$ets <- ets_model_lr$fitted
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1 + ets, data = sales_data)
summary(lr_model)
ma_model_lr <- ma(data1,order = 7, centre = TRUE)
ma_model_lr
sales_data$ma <- NA
sales_data$ma[4:732] <- ma_model_lr
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1 + ma, data = sales_data)
summary(lr_model)
lr_model2 <- auto.arima(data1,xreg = sales_data[3:9])
lr_model2 <- auto.arima(data1,xreg = as.numeric(sales_data[3:9]))
lr_model2 <- auto.arima(data1,xreg = as.matrix(sales_data[3:9]))
summary(lr_model2)
accuracy(lr_model2,data1)
lr_fc2 <- forecast(lr_model2, h = 56)
lr_model2 <- auto.arima(train,xreg = as.matrix(sales_data[3:9]))
lr_model2 <- auto.arima(train,xreg = as.matrix(sales_data[1:588,3:9]))
summary(lr_model2)
lr_fc2 <- forecast(lr_model2, h = 56)
lr_fc2 <- forecast(lr_model2, xreg = 56)
lr_fc2 <- forecast(lr_model2, xreg = (7,8))
lr_fc2 <- forecast(lr_model2, xreg = rep(7,8))
lr_fc2 <- forecast(lr_model2, xreg = rep(8,7))
lr_fc2 <- forecast(lr_model2, xreg = as.matrix(sales_data[1:588,3:9]),h=56)
accuracy(lr_model2,data1)
accuracy(lr_fc2,data1)
accuracy(arima_fc_result$pred, data1)
#####auto_exponential#####
ets_zzz <- ets(train,model = 'ZZZ')
auto_sf <- forecast(ets_zzz,h = 56)
accuracy(auto_sf,data1)
ets_model1 <- ets(train,model = 'ANA',alpha = 0.1,gamma = 0.1,biasadj = FALSE,ic = 'aic',use.initial.values = TRUE)
model1_fc <- forecast(ets_model1, h = 56)
accuracy(model1_fc,data1)
auto_arima <- auto.arima(train,stepwise = FALSE)
auto_model1 <- forecast(auto_arima, h = 56)
accuracy(auto_model1,data1)
arima_1 <- sarima(train,6,0,2,2,0,0,7)
arima_fc2 <- forecast(arima_1, h =56)
arima_fc_result <- sarima.for(train, n.ahead = 56,6,0,2,2,0,0,7)
accuracy(arima_fc_result$pred, data1)
mean(arima_fc_result$se)
lr_fc1 <- forecast(lr_model, h = 56)
lr_fc1 <- predict(lr_model, sales_data, interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, test, interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, cbind(sales_data[,2:10],sales_data[,18]), interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[,2:18], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[,2:9], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[,2:10], interval = 'prediction', level = '0.95')
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat +
lag1, data = sales_data)
lr_fc1 <- predict(lr_model, sales_data[,2:10], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[,1:10], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[,2:10], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, sales_data[1,2:10], interval = 'prediction', level = '0.95')
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat + Sunday
lag1, data = sales_data)
lr_fc1 <- predict(lr_model, sales_data[1,2:10], interval = 'prediction', level = '0.95')
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat + Sunday +
lag1, data = sales_data)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat + Sunday +
lag1, data = sales_data)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat  +
lag1, data = sales_data)
lr_fc1 <- predict(lr_model, sales_data[1,2:10], interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model, cbind(sales_data[1,2:8],sales_data[1,10]), interval = 'prediction', level = '0.95')
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat
, data = sales_data)
lr_fc1 <- predict(lr_model, sales_data[1,2:8], interval = 'prediction', level = '0.95')
lr_test <- lm(Sales ~ lag1, data = sales_data)
predict(lr_test)
lr_fc1 <- predict(lr_model, interval = 'prediction', level = '0.95')
lr_fc1 <- predict(lr_model)
plot(lr_fc1)
accuracy(lr_fc1, data1)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat
+ lag1 + ma, data = sales_data)
lr_model <- lm(Sales ~ Mon + Tues + Wed + Thur + Fri + Sat
+ lag1 + ma, data = sales_data[1:588,])
lr_fc1 <- predict(lr_model)
plot(lr_fc1)
accuracy(lr_fc1, data1)
summary(lr_model)
accuracy(lr_fc1, data1)
?predict
setwd("C:/Users/User/Desktop/583")
retailer_A <- read.csv("A_implement.csv",header = TRUE)
retailer_B <- read.csv("B_implement.csv", header = TRUE)
original_data <- read.csv("O_implement.csv",header = TRUE)
original_data <- original_data[,1:8]
retailer_A <- retailer_A[,1:8]
retailer_B <- retailer_B[,1:8]
View(retailer_A)
retailer_A$Mon <- NA
retailer_A$Tues <- NA
retailer_A$Wed <- NA
retailer_A$Thur <- NA
retailer_A$Fri <- NA
retailer_A$Sat <- NA
retailer_A$Sun <- NA
for (i in 1:104){
for (j in 1:7){
retailer_A[j+(i-1)*7,j+8] <- 1
}
}
retailer_A[is.na(retailer_A)] <- 0
retailer_B$Mon <- NA
retailer_B$Tues <- NA
retailer_B$Wed <- NA
retailer_B$Thur <- NA
retailer_B$Fri <- NA
retailer_B$Sat <- NA
retailer_B$Sun <- NA
for (i in 1:104){
for (j in 1:7){
retailer_B[j+(i-1)*7,j+8] <- 1
}
}
retailer_B[is.na(retailer_B)] <- 0
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat + Sun, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
AICc(model5)
AIC(model5)
model6 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
#####weekday_dummy_variables######
data1 <- ts(retailer_A,frequency = 7)
seasplot(data1)
autoplot(data1)
#####weekday_dummy_variables######
data1 <- ts(retailer_A[,4],frequency = 7)
autoplot(data1)
retailer_A <- read.csv("A_implement.csv",header = TRUE)
retailer_B <- read.csv("B_implement.csv", header = TRUE)
retailer_A <- retailer_A[,1:8]
retailer_B <- retailer_B[,1:8]
retailer_A$Mon <- NA
retailer_A$Tues <- NA
retailer_A$Wed <- NA
retailer_A$Thur <- NA
retailer_A$Fri <- NA
retailer_A$Sat <- NA
retailer_A$Sun <- NA
for (i in 1:52){
for (j in 1:7){
retailer_A[j+(i-1)*7,j+8] <- 1
}
}
retailer_A[is.na(retailer_A)] <- 0
retailer_B$Mon <- NA
retailer_B$Tues <- NA
retailer_B$Wed <- NA
retailer_B$Thur <- NA
retailer_B$Fri <- NA
retailer_B$Sat <- NA
retailer_B$Sun <- NA
for (i in 1:52){
for (j in 1:7){
retailer_B[j+(i-1)*7,j+8] <- 1
}
}
retailer_B[is.na(retailer_B)] <- 0
#####weekday_dummy_variables######
data1 <- ts(retailer_A[,4],frequency = 7)
autoplot(data1)
seasplot(data1)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat + Sun, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model6 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model16 <-  lm(Vol~P0 + AvgCompPrice + Weekday, data = retailer_B)
summary(model16)
model7m <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat + month, data = retailer_A)
for (i in 353){
retailer_A$month <- month(as.POSIXct(retailer_A[,2],format = "%d/%m/%Y"))
}
library("lubridate")
for (i in 353){
retailer_A$month <- month(as.POSIXct(retailer_A[,2],format = "%d/%m/%Y"))
}
retailer_A$Weekdays <- NA
retailer_A$Weekends <- NA
for (i in 364){
if (retailer_A[1,3] == 'Saturday'|retailer_A[1,3] == 'Sunday'){
retailer_A$weekdays[1] <- 0
}
else{
retailer_A$weekends[1] <- 1
}
}
for (i in 353){
retailer_A$month <- month(as.POSIXct(retailer_A[,2],format = "%d/%m/%Y"))
}
retailer_A$season <- NA
retailer_A$k <- retailer_A$month
retailer_A$k <- as.numeric(retailer_A$k)
for (i in 1:353){
if (retailer_A$k[i] < 4){
retailer_A$season[i] <- 'spring'
}
if (retailer_A$k[i] < 7  & retailer_A$k[i] > 3){
retailer_A$season[i] <- 'summer'
}
if (retailer_A$k[i] < 10  & retailer_A$k[i] > 6){
retailer_A$season[i] <- 'autunm'
}
if ( retailer_A$k[i] > 9){
retailer_A$season[i] <- 'winter'
}
}
retailer_A$k <- NULL
retailer_A$season <- factor(retailer_A$season)
for (i in 355){
retailer_B$month <- month(as.POSIXct(retailer_B[,2],format = "%d/%m/%Y"))
}
View(retailer_B)
retailer_B$season <- NA
retailer_B$k <- retailer_B$month
retailer_B$k <- as.numeric(retailer_B$k)
for (i in 1:353){
if (retailer_B$k[i] < 4){
retailer_B$season[i] <- 'spring'
}
if (retailer_B$k[i] < 7  & retailer_B$k[i] > 3){
retailer_B$season[i] <- 'summer'
}
if (retailer_B$k[i] < 10  & retailer_B$k[i] > 6){
retailer_B$season[i] <- 'autunm'
}
if ( retailer_B$k[i] > 9){
retailer_B$season[i] <- 'winter'
}
}
retailer_B$k <- NULL
retailer_B$season <- factor(retailer_B$season)
retailer_A$month <- factor(retailer_A$month)
retailer_B$month <- factor(retailer_B$month)
model7m <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat + month, data = retailer_A)
summary(model7m)
#####weekday_dummy_variables######
data1 <- ts(retailer_A[,4],frequency = 30)
seasplot(data1)
#####weekday_dummy_variables######
data1 <- ts(retailer_A[,4],frequency = 90)
seasplot(data1)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat + season, data = retailer_A)
summary(model5)
#####weekday_dummy_variables######
data1 <- ts(retailer_A[,4],frequency = 7)
seasplot(data1)
model5 <- lm(Vol~P0 + MaxCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + MinCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model6 <- lm(Vol~P0 + MinCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model6 <- lm(Vol~P0 + MaxCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model6 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model6 <- lm(Vol~P0 + MaxCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model6 <- lm(Vol~P0 + MinCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
model5 <- lm(Vol~P0 + AvgCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model6 <- lm(Vol~P0 + MaxCompPrice + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
(-145*mean(retailer_A[,5])-3598)/(2667+389 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+389 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+583 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+549 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+652 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+148 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+2924 - 145*mean(retailer_A[,5])+3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+2924 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+389 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+583 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+549 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+603 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+652 - 145*mean(retailer_A[,5])-3598*0.025)
-(-145*mean(retailer_A[,5])-3598)/(2667+148 - 145*mean(retailer_A[,5])-3598*0.025)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 1827 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 1982 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 1981 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 2023 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 2184 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 478 -68*mean(retailer_B[,5])+4139 * 0)
-(-68*mean(retailer_B[,5])-4139)/(2957 + 10475 -68*mean(retailer_B[,5])+4139 * 0)
model5 <- lm(Vol~P0 + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
model6 <- lm(Vol~P0 + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
profits.s <- matrix(NA,200000,2)
for (j in 1:200){
q <- runif(1,0,0.1)
for (i in 1:1000){
obj <- function(p){
a <- -130
b <- 5337
c <- runif(1,1.3,1.8)*(1 + q)
return(b * p^2 + (a - b * c) * p - b * c)
}
profit_max <- optimize(obj,c(2.69,4.39),tol = 0.0001,maximum = TRUE)
profits.s[i + 1000*(j-1),1] <- profit_max$objective
profits.s[i + 1000*(j-1),2] <- profit_max$maximum
}
}
mean(profits.s[,2])
for (j in 1:200){
q <- runif(1,0,0.1)
for (i in 1:1000){
obj <- function(p){
a <- -130
b <- 2460
c <- runif(1,1.3,1.8)*(1 + q)
return(b * p^2 + (a - b * c) * p - b * c)
}
profit_max <- optimize(obj,c(2.69,4.39),tol = 0.0001,maximum = TRUE)
profits.s[i + 1000*(j-1),1] <- profit_max$objective
profits.s[i + 1000*(j-1),2] <- profit_max$maximum
}
}
mean(profits.s[,2])
for (j in 1:200){
q <- runif(1,0,0.1)
for (i in 1:1000){
obj <- function(p){
a <- 2460
b <- -130
c <- runif(1,1.3,1.8)*(1 + q)
return(b * p^2 + (a - b * c) * p - b * c)
}
profit_max <- optimize(obj,c(2.69,4.39),tol = 0.0001,maximum = TRUE)
profits.s[i + 1000*(j-1),1] <- profit_max$objective
profits.s[i + 1000*(j-1),2] <- profit_max$maximum
}
}
mean(profits.s[,2])
View(retailer_A)
mean(retailer_A[,5]*retailer_A[,9])
#####retailer_A#####
mean(retailer_A[,5]*retailer_A[,9])
mean(retailer_A[,5]*retailer_A[,10])
mean(retailer_A[,5]*retailer_A[,11])
mean(retailer_A[,5]*retailer_A[,12])
mean(retailer_A[,5]*retailer_A[,13])
mean(retailer_A[,5]*retailer_A[,14])
mean(retailer_B[,5])
#####retailer_A#####
mean(retailer_A[,5]*retailer_A[,9])*7
-(-145*A-3598)/(2667+389 - 145*A-3598*0.025)
#####retailer_A#####
A <- mean(retailer_A[,5]*retailer_A[,9])*7
B <- mean(retailer_A[,5]*retailer_A[,10])*7
C <- mean(retailer_A[,5]*retailer_A[,11])*7
D <- mean(retailer_A[,5]*retailer_A[,12])*7
E <- mean(retailer_A[,5]*retailer_A[,13])*7
G <- mean(retailer_A[,5]*retailer_A[,14])*7
-(-145*A-3598)/(2667+389 - 145*A-3598*0.025)
-(-145*A-3598)/(2667+583 - 145*A-3598*0.025)
-(-145*A-3598)/(2667+549 - 145*A-3598*0.025)
-(-145*A-3598)/(2667+603 - 145*A-3598*0.025)
-(-145*A-3598)/(2667+652 - 145*A-3598*0.025)
-(-145*A-3598)/(2667+148 - 145*A-3598*0.025)
-(-145*B-3598)/(2667+583 - 145*B-3598*0.025)
-(-145*C-3598)/(2667+549 - 145*C-3598*0.025)
-(-145*D-3598)/(2667+603 - 145*D-3598*0.025)
-(-145*E-3598)/(2667+652 - 145*E-3598*0.025)
-(-145*G-3598)/(2667+148 - 145*G-3598*0.025)
#####retaier_B######
A <- mean(retailer_B[,5]*retailer_B[,9])*7
B <- mean(retailer_B[,5]*retailer_B[,10])*7
C <- mean(retailer_B[,5]*retailer_B[,11])*7
D <- mean(retailer_B[,5]*retailer_B[,12])*7
E <- mean(retailer_B[,5]*retailer_B[,13])*7
G <- mean(retailer_B[,5]*retailer_B[,14])*7
-(-68*A-3598)/(2957+1827 - 68*A)
-(-68*B-3598)/(2957+1982 - 68*B)
-(-68*C-3598)/(2957+1981 - 68*C)
-(-68*D-3598)/(2957+2023 - 68*D)
-(-68*E-3598)/(2957+2184 - 68*E)
-(-68*G-3598)/(2957+478 - 68*G)
mean(retailer_B[,5])
mean(price)
price <- retailer_A[,5]
mean(price)
profits.t <- matrix(NA,200000,2)
model6 <- lm(Vol~P0 + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_B)
summary(model6)
for (j in 1:200){
q <- runif(1,0,0.1)
for (i in 1:1000){
obj <- function(p){
a <- -95
b <- 3113
c <- runif(1,1.3,1.8)*(1 + q)
return(a * p^2 + (b - a * c) * p + b * c)
}
profit_max <- optimize(obj,c(2.69,4.39),tol = 0.0001,maximum = TRUE)
profits.t[i + 1000*(j-1),1] <- profit_max$objective
profits.t[i + 1000*(j-1),2] <- profit_max$maximum
}
}
model5 <- lm(Vol~P0  + Mon + Tues + Wed
+ Thur + Fri + Sat, data = retailer_A)
summary(model5)
mean(profits.t[,2])
mean(profits.s[,2])
##########1,2,3_all_allowed##########
h <- read.csv("Table_new.csv", header = TRUE)
View(h)
for (i in 1:7){
f.obj <- c(12,15,18,20,28,23,26,31,21,36,15,11,20,16,12,8,18,14,28,24,23,19,6,4,9,7,14,12)
f.con <- h[,c(1:28)]
f.dir <- c(rep('<=',7),rep('>=',6))
f.rhs <- h[,28 + i]
profits <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)$objval
solution <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)$solution
D[i,1] <- profits
D[i,2:29] <- solution
}
View(D)
library("lpSolve")
D <- matrix(NA,7,29)
for (i in 1:7){
f.obj <- c(12,15,18,20,28,23,26,31,21,36,15,11,20,16,12,8,18,14,28,24,23,19,6,4,9,7,14,12)
f.con <- h[,c(1:28)]
f.dir <- c(rep('<=',7),rep('>=',6))
f.rhs <- h[,28 + i]
profits <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)$objval
solution <- lp("max", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)$solution
D[i,1] <- profits
D[i,2:29] <- solution
}
View(D)
E <- matrix(D,ncol = 7, byrow = TRUE)
colnames(E)<-c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")
View(E)
