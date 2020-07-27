Plastic_raw <- read.csv(file.choose())
View(Plastic_raw)
hist(Plastic_raw$Sales)
## Preprocessing of data
## Creating 12 dummy variables
z <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
View(z)
colnames(z) <- month.abb
Plastic <- cbind(Plastic_raw,z)
View(Plastic)

## Input "t"
Plastic["t"] <- c(1:60)
View(Plastic)
Plastic["log_Sales"] <- log(Plastic$Sales)
Plastic["t_square"] <- Plastic$t * Plastic$t
View(Plastic)

## Creating training and Testing dataset
Plastic_train <- Plastic[1:45,]
View(Plastic_train)
Plastic_test <- Plastic[46:60,]
View(Plastic_test)

## Linear Model ##
Pla_Linear <- lm(Sales~t,data=Plastic_train)
summary(Pla_Linear)
Pla_pred <- data.frame(predict(Pla_Linear,interval="predict",newdata=Plastic_test))
Pla_pred
rmse_Pla_Linear <- sqrt(mean((Plastic_test$Sales-Pla_pred$fit)^2,na.rm=T))
rmse_Pla_Linear

## Expoential Model ##
Pla_Expo <- lm(log_Sales~t,data=Plastic_train)
summary(Pla_Expo)
Pla_Expo_pred <- data.frame(predict(Pla_Expo,interval="predict",newdata=Plastic_test))
Pla_Expo_pred <- exp(Pla_Expo_pred)
Pla_Expo_pred
rmse_Pla_Expo <- sqrt(mean((Plastic_test$Sales-Pla_Expo_pred$fit)^2,na.rm=T))
rmse_Pla_Expo

## Quadratic Model ##
Pla_Quad <- lm(Sales~t+t_square,data=Plastic_train)
summary(Pla_Quad)
Pla_Quad_pred <- data.frame(predict(Pla_Quad,interval="predict",newdata=Plastic_test))
Pla_Quad_pred
rmse_Pla_Quad <- sqrt(mean((Plastic_test$Sales-Pla_Quad_pred$fit)^2,na.rm=T))
rmse_Pla_Quad

## Additive Seasonality Model ##
Pla_Add <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
              data=Plastic_test)
summary(Pla_Add)
Pla_Add_pred <- data.frame(predict(Pla_Add,interval="predict",newdata=Plastic_test))
Pla_Add_pred
rmse_Pla_Add <- sqrt(mean((Plastic_test$Sales-Pla_Add_pred$fit)^2,na.rm=T))
rmse_Pla_Add

## Additive Seasonality with Quadratic Model ##
Pla_Add_Quad <- lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
                   data=Plastic_test)
summary(Pla_Add_Quad)
Pla_Add_Quad_pred <- data.frame(predict(Pla_Add_Quad,interval="predict",newdata=Plastic_test))
Pla_Add_Quad_pred
rmse_Pla_Add_Quad <- sqrt(mean((Plastic_test$Sales-Pla_Add_Quad_pred$fit)^2,na.rm=T))
rmse_Pla_Add_Quad

## Multiplicative Seasonality Model ##
Pla_Mul <- lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
              data=Plastic_test)
summary(Pla_Mul)
Pla_Mul_pred <- data.frame(predict(Pla_Mul,interval="predict",newdata=Plastic_test))
Pla_Mul_pred <- exp(Pla_Mul_pred)
Pla_Mul_pred
rmse_Pla_Mul <- sqrt(mean((Plastic_test$Sales-Pla_Mul_pred$fit)^2,na.rm=T))
rmse_Pla_Mul

## Preparing a Table on Model and its RMSE Values
Plastic_rmse <- data.frame(c("rmse_Pla_Linear","rmse_Pla_Expo","rmse_Pla_Quad","rmse_Pla_Add",
                             "rmse_Pla_Add_Quad","rmse_Pla_Mul"),c(rmse_Pla_Linear,rmse_Pla_Expo,rmse_Pla_Quad,rmse_Pla_Add,
                              rmse_Pla_Add_Quad,rmse_Pla_Mul))
colnames(Plastic_rmse) <- c("Model","RMSE")
View(Plastic_rmse)

## Additive Seasonality with Quadratic Model has least RMSE value
write.csv(Plastic,file="Plastic.csv", row.names = F)
View(Plastic)

## Bulid a Additive Seasonality with Quadratic Model for both training and
## testing dataset
Plastic_Model <- lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
                       data=Plastic)
summary(Plastic_Model)
final_pred <- data.frame(predict(Plastic_Model,interval="predict",newdata=Plastic_test))
final_pred

plot(Plastic_Model,lwd=2)

## Take all residual values of the model build & plot ACF plot
acf(Plastic_Model$residuals,lag.max = 10)

ARIMA <- arima(Plastic_Model$residuals,c(1,0,0))
AR_error <- ARIMA$residuals
acf(ARIMA$residuals,lag.max = 10)

# Predicting next 12 months errors using arima(order =c(1,0,0))
library(forecast)
errors <- forecast(ARIMA,h=12)
future_error <- data.frame(errors)
View(future_error)
future_error <- future_error$Point.Forecast

## # Predicted values for new data + future error values
Plastic_Sales <- final_pred+future_error
View(Plastic_Sales)
write.csv(Plastic_Sales,"Plastic_Sales.csv",row.names = F)
