library(readxl)
Airlines_raw <- read_xlsx(file.choose())
View(Airlines_raw)
hist(Airlines_raw$Passengers)

# Pre Processing
# Creating 12 dummy variables
z <- data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
View(z)
colnames(z) <- month.abb # Assigning month names 
View(z)
Airlines <- cbind(Airlines_raw,z)
View(Airlines)
colnames(Airlines)

## Input "t" 
Airlines["t"] <- c(1:96)
View(Airlines)

Airlines["log_Passengers"] <- log(Airlines["Passengers"])
Airlines["t_square"] <- Airlines["t"]*Airlines["t"]
View(Airlines)
attach(Airlines)

## Creating training and testing dataset
Airlines_train <- Airlines[1:72,]
View(Airlines_train)
Airlines_test <- Airlines[73:96,]
View(Airlines_test)

## Linear Model ##
linear_model <- lm(Passengers~t,data=Airlines_train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model,interval="predict",newdata=Airlines_test))
linear_pred
rmse_linear <- sqrt(mean((Airlines_test$Passengers-linear_pred$fit)^2,na.rm=T))
rmse_linear

## Exponential Model ##
Expo_Model <- lm(log_Passengers~t,data=Airlines_train)
summary(Expo_Model)
Expo_pred <- data.frame(predict(Expo_Model,interval="predict",newdata=Airlines_test))
Expo_pred <- exp(Expo_pred)
Expo_pred
rmse_Expo <- sqrt(mean((Airlines_test$Passengers-Expo_pred$fit)^2,na.rm=T))
rmse_Expo

## Quadratic Model ##
Quad_Model <- lm(Passengers~t+t_square,data=Airlines_train)
summary(Quad_Model)
Quad_pred <- data.frame(predict(Quad_Model,interval="predict",newdata=Airlines_test))
Quad_pred
rmse_Quad <- sqrt(mean((Airlines_test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

## Additive Seasonality Model##
Add_Model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
                data=Airlines_train)
summary(Add_Model)
Add_pred <- data.frame(predict(Add_Model,interval="predict",newdata=Airlines_test))
Add_pred
rmse_Add <- sqrt(mean((Airlines_test$Passengers-Add_pred$fit)^2,na.rm=T))
rmse_Add

## Additive Seasonality with Quadratic Model ##
Add_Quad_Model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep
                     +Oct+Nov+Dec,data=Airlines_train)
summary(Add_Quad_Model)
Add_Quad_pred <- data.frame(predict(Add_Quad_Model,interval="predict",newdata=Airlines_test))
Add_Quad_pred
rmse_Add_Quad <- sqrt(mean((Airlines_test$Passengers-Add_Quad_pred$fit)^2,na.rm=T))
rmse_Add_Quad

## Multiplicative Seasonality Model ##
Mul_Model <- lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep
                +Oct+Nov+Dec,data=Airlines_train)
summary(Mul_Model)
Mul_pred <- data.frame(predict(Mul_Model,interval="predict",newdata=Airlines_test))
Mul_pred
rmse_Mul <- sqrt(mean((Airlines_test$Passengers-Mul_pred$fit)^2,na.rm=T))
rmse_Mul

## Preparing table on model and it's RMSE values
table_rmse <- data.frame(c("rmse_linear","rmse_Expo","rmse_Quad","rmse_Add",
                           "rmse_Add_Quad","rmse_Mul"),c(rmse_linear,rmse_Expo,rmse_Quad,rmse_Add,rmse_Add_Quad,rmse_Mul))
colnames(table_rmse) <- c("Model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value
write.csv(Airlines,file="Airlines.csv", row.names = F)
View(Airlines)
getwd()
setwd("F:/Data Sci/R Programming/Assignment/Forecasting")

## Combining Training & test data to build Additive seasonality using Quadratic Trend
Add_Sea_Quad_Model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+
                      Sep+Oct+Nov+Dec,data=Airlines)
summary(Add_Sea_Quad_Model)
pred_new <- data.frame(predict(Add_Sea_Quad_Model,interval="predict",
                        newdata=Airlines_test))
pred_new

plot(Add_Sea_Quad_Model,lwd=2)
## Take all residual values of the model build & plot ACF plot
acf(Add_Sea_Quad_Model$residuals,lag.max = 10)

Arima_Model <- arima(Add_Sea_Quad_Model$residuals,c(1,0,0))
Arima_Model$residuals

ARerror <- Arima_Model$residuals
acf(ARerror,lag.max = 10)

# Predicting next 12 months errors using arima(order =c(1,0,0))

library(forecast)
errors_12 <- forecast(Arima_Model, h = 12)

future_errors <- data.frame(errors_12)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# Predicted values for new data + future error values
Passenger_new_values <- pred_new + future_errors
View(Passenger_new_values)
write.csv(Passenger_new_values, file = "Passenger_values.csv", row.names = F)
getwd()