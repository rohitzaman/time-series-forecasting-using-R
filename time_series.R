file <- read.csv(file.choose(),header = TRUE)
str(file)
View(file)

a <- ts(file$IPG2211A2N, frequency = 12, start = c(1985, 1),end = c(2017,12))
a
attributes(a)
plot(a)
#log transform
a <- log(a)
plot(a)
#decompose
decom <- decompose(a)
decom$figure
plot(decom$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasionality Index',
     col = 'blue',
     las = 2)
plot(decom)
#ARIMA - Autoregressive Integrated Moving Average
install.packages("forecast")
library(forecast)
model <- auto.arima(a)
model
#p = AR order
#d = degree of differencing
#q = MA order(moving average)
attributes(model)
# ACF and pacf plots
acf(model$residuals,main = 'Correlogram')
pacf(model$residuals,main = 'Partial Correlogram')

#Ljung-Box test
Box.test(model$residuals,lag = 20,type = 'Ljung-Box')

#Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogrm of Residuals',
     freq = FALSE)
lines(density(model$residuals))
#forecase
f <- forecast(model,48)
library(ggplot2)
autoplot(f)
accuracy(f)

#prediction

pred <- predict(model,n.ahead = 10*12)
pred1 <- 2.718^pred$pred
pred1
ts.plot(a,pred$pred,log="y",lty=c(1,3))







