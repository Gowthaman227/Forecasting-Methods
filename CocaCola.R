library(readxl)
CocaCola_raw <- read_xlsx(file.choose())
View(CocaCola_raw)
hist(CocaCola_raw$Sales)

## Preprocessing data
## Creating 4 dummy variables as 4 Quarters each year 
Q1 <- ifelse(grepl("Q1",CocaCola_raw$Quarter),'1','0')
Q2 <- ifelse(grepl("Q2",CocaCola_raw$Quarter),'1','0')
Q3 <- ifelse(grepl("Q3",CocaCola_raw$Quarter),'1','0')
Q4 <- ifelse(grepl("Q4",CocaCola_raw$Quarter),'1','0')
CocaCola <- cbind(CocaCola_raw,Q1,Q2,Q3,Q4)
View(CocaCola)
colnames(CocaCola)

## Input "t"
CocaCola["t"] <- c(1:42)
View(CocaCola)

CocaCola["log_Sales"] <- log(CocaCola$Sales)
CocaCola["t_square"] <- CocaCola$t*CocaCola$t
View(CocaCola)

## Creating training and testing dataset
CocaCola_train <- CocaCola[1:32,]
View(CocaCola_train)
CocaCola_test <- CocaCola[33:42,]
View(CocaCola_test)

## Linear Model ##
Coca_Linear <- lm(Sales~t,data=CocaCola_train)
summary(Coca_Linear)
Coca_pred <- data.frame(predict(Coca_Linear,interval="predict",newdata = CocaCola_test))
Coca_pred
rmse_Linear <- sqrt(mean((CocaCola_test$Sales-Coca_pred$fit)^2,na.rm=T))
rmse_Linear

## Exponential Model ##
Coca_Expo <- lm(log_Sales~t,data=CocaCola_train)
summary(Coca_Expo)
Coca_Expo_pred <- data.frame(predict(Coca_Expo,interval="predict",newdata = CocaCola_test))
Coca_Expo_pred <- exp(Coca_Expo_pred)
Coca_Expo_pred
rmse_Coca_Expo <- sqrt(mean((CocaCola_test$Sales-Coca_Expo_pred$fit)^2,na.rm=T))
rmse_Coca_Expo

## Quadratic Seasonality Model ##
Coca_Quad <- lm(Sales~t+t_square,data=CocaCola_train)
summary(Coca_Quad)
Coca_Quad_pred <- data.frame(predict(Coca_Quad,interval="predict",newdata=CocaCola_test))
Coca_Quad_pred
rmse_Coca_Quad <- sqrt(mean((CocaCola_test$Sales-Coca_Quad_pred$fit)^2,na.rm=T))
rmse_Coca_Quad

## Additive Seasonality Model ##
Coca_Add <- lm(Sales~Q1+Q2+Q3+Q4,data=CocaCola_train)
Coca_Add
Coca_Add_pred <- data.frame(predict(Coca_Add,interval="predict",newdata=CocaCola_test))
Coca_Add_pred
rmse_Coca_Add <- sqrt(mean((CocaCola_test$Sales-Coca_Add_pred$fit)^2,na.rm=T))
rmse_Coca_Add

## Additive Seasonality with Quadratic Model ##
Coca_Add_Quad <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocaCola_train)
Coca_Add_Quad
Coca_Add_Quad_pred <- data.frame(predict(Coca_Add_Quad,interval="predict",newdata=CocaCola_test))
Coca_Add_Quad_pred
rmse_Coca_Add_Quad <- sqrt(mean((CocaCola_test$Sales-Coca_Add_Quad_pred$fit)^2,na.rm=T))
rmse_Coca_Add_Quad

## Multiplicative Seasonality Model ##
Coca_Mul <- lm(log_Sales~Q1+Q2+Q3+Q4,data=CocaCola_train)
Coca_Mul
Coca_Mul_pred <- data.frame(predict(Coca_Mul,interval="predict",newdata = CocaCola_test))
Coca_Mul_pred
rmse_Coca_Mul <- sqrt(mean((CocaCola_test$Sales-Coca_Mul_pred$fit)^2,na.rm=T))
rmse_Coca_Mul

## Preparing table on model and it's RMSE values
Coca_rmse <- data.frame(c("rmse_Linear","rmse_Coca_Expo","rmse_Coca_Quad",
                          "rmse_Coca_Add","rmse_Coca_Add_Quad","rmse_Coca_Mul"),c(rmse_Linear,rmse_Coca_Expo,rmse_Coca_Quad,
                          rmse_Coca_Add,rmse_Coca_Add_Quad,rmse_Coca_Mul))
colnames(Coca_rmse) <- c("Model","RMSE")
View(Coca_rmse)

## Additive Seasonality with Quadratic Model has least RMSE value
write.csv(CocaCola,file="CocaCola.csv", row.names = F)
View(CocaCola)
getwd()

## Bulid a Additive Seasonality with Quadratic Model for both training and
## testing dataset
Cocacola_Model <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocaCola)
Cocacola_Model
Coca_pred_new <- data.frame(predict(Cocacola_Model,interval="predict",newdata=CocaCola_test))
Coca_pred_new

plot(Cocacola_Model,lwd=2)

## Take all residual values of the model build & plot ACF plot
acf(Cocacola_Model$residuals,lag.max = 10)

Coca_arima <- arima(Cocacola_Model$residuals,c(1,0,0))
Coca_arima$residuals
ARerrors <- Coca_arima$residuals
acf(ARerrors,lag.max = 10)

# Predicting next 4 Quarters errors using arima(order =c(1,0,0))

library(forecast)
Quarter_errors <- forecast(Coca_arima,h=4)

future_errors <- data.frame(Quarter_errors)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# Predicted values for new data + future error values
CocaCola_Sales <- Coca_pred_new+future_errors
View(CocaCola_Sales)
write.csv(CocaCola_Sales,"CocaCola_Sales.csv",row.names = F)
