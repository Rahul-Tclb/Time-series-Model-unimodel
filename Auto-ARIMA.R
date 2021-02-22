# import necessary packages

library(tseries)
library(forecast)
library(graphics)
library(ggplot2)

# Importing Data 

df  = read.csv('./Reliance_Data.csv')
class(df)
names(df)

# Renaming the columns

colnames(df)  <- c('Date' , 'x2' , 'x3' , 'x4' , 'x5' , 'x6' , 'x7' , 'x8' , 'x9' , 'x10',
                   'x11', 'x12' , 'x13' , 'x14' , 'x15' , 'x16' , 'x17' , 'x18' , 'x19' , 'x20' ,
                   'x21', 'x22' , 'x23' , 'x24' , 'x25' , 'x26' , 'x27' , 'x28' , 'x29' , 'x30' ,
                   'x31',  'x32' , 'x33' , 'x34'  , 'x35')

# Dropping the null value columns

df <- subset(df , select  = -c( x9 , x17 , x20 , x22 , x29 , x30 , x31 , x32 ))
df


# converting the data into timeseries and create a time series datafrme

x1  <- ts(df$Date,    start = 2011 ,frequency = 1)
x2  <- ts(df$x2  ,    start = 2011 ,frequency = 1)
x3  <- ts(df$x3  ,    start = 2011 ,frequency = 1)
x4  <- ts(df$x4  ,    start = 2011 ,frequency = 1)
x5  <- ts(df$x5  ,    start = 2011 ,frequency = 1)
x6  <- ts(df$x6  ,    start = 2011 ,frequency = 1)
x7  <- ts(df$x7  ,    start = 2011 ,frequency = 1)
x8  <- ts(df$x8  ,    start = 2011 ,frequency = 1)
x10  <- ts(df$x10,    start = 2011 ,frequency = 1)
x11 <- ts(df$x11 ,    start = 2011 ,frequency = 1)
x12  <- ts(df$x12  ,  start = 2011 ,frequency = 1)
x13  <- ts(df$x13  ,  start = 2011 ,frequency = 1)
x14  <- ts(df$x14  ,  start = 2011 ,frequency = 1)
x15  <- ts(df$x15  ,  start = 2011 ,frequency = 1)
x16  <- ts(df$x16  ,  start = 2011 ,frequency = 1)
x18  <- ts(df$x18  ,  start = 2011 ,frequency = 1)
x19  <- ts(df$x19  ,  start = 2011 ,frequency = 1)
x21  <- ts(df$x21  ,  start = 2011 ,frequency = 1)
x23  <- ts(df$x23  ,  start = 2011 ,frequency = 1)
x24  <- ts(df$x24  ,  start = 2011 ,frequency = 1)
x25  <- ts(df$x25  ,  start = 2011 ,frequency = 1)
x26  <- ts(df$x26  ,  start = 2011 ,frequency = 1)
x27  <- ts(df$x27  ,  start = 2011 ,frequency = 1)
x28  <- ts(df$x28  ,  start = 2011 ,frequency = 1)
x33  <- ts(df$x33  ,  start = 2011 ,frequency = 1)
x34  <- ts(df$x34  ,  start = 2011 ,frequency = 1)
x35  <- ts(df$x35  ,  start = 2011 ,frequency = 1)



tf <- cbind(x1 ,x2 , x3, x4, x5 , x6,x7, x8,x10,
            x11,x12,x13,x14,x15,x16,x18,x19,x21,x23,
            x24,x25,x26,x27,x28,x33,x34,x35 )


#------------------------------------------------------------------------------#

# Visuilizing the data of x2

autoplot(x2) + ggtitle('Time series Graph')+ylab('REVENUE.FROM.OPERATIONS..GROSS.')

#Build ARIMA Model and check statistics

fit_Arima2 <- auto.arima(x2)
print(summary(fit_Arima2))
checkresiduals(fit_Arima2)


# forecast the ARIMA model and viewing the statistics

fcast2 <- forecast(fit_Arima2, h = 1)
print(summary(fcast2))
plot(fcast2)

# Reudial Plots sample vs Theoretical

qqnorm(fcast2$residuals)
acf(fcast2$residuals)
pacf(fcast2$residuals)


# checking the stationary

adf.test(fcast2$residuals)
kpss.test(fcast2$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima2))

# Method -II

accuracy(fit_Arima2)

# forecast plot

autoplot(fcast2)+ggtitle('Forecast of Revnue') + ylab('REVENUE.FROM.OPERATIONS..GROSS.')
print(summary(fcast2))

#------------------------------------------------------------------------------#

# Visuilizing the data of x3

autoplot(x3) + ggtitle('inflation rate')+ylab('Less..Excise.Sevice.Tax.Other.Levies')

#Build ARIMA Model and check statistics

fit_Arima3 <- auto.arima(x3)
print(summary(fit_Arima3))
checkresiduals(fit_Arima3)


# forecast the ARIMA model and viewing the statistics

fcast3 <- forecast(fit_Arima3, h = 1)
print(summary(fcast3))
plot(fcast3)

# Reudial Plots sample vs Theoretical

qqnorm(fcast3$residuals)
acf(fcast3$residuals)
pacf(fcast3$residuals)


# checking the stationary

adf.test(fcast3$residuals)
kpss.test(fcast3$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima3))

# Method -II

accuracy(fit_Arima3)

# forecast plot

autoplot(fcast3)+ggtitle('Time series Graph')+ylab('Less..Excise.Sevice.Tax.Other.Levies')

print(summary(fcast3))

#------------------------------------------------------------------------------#

# Visuilizing the data of x4

autoplot(x4) + ggtitle('Time series Graph')+ylab('REVENUE.FROM.OPERATIONS..NET.')

#Build ARIMA Model and check statistics

fit_Arima4 <- auto.arima(x4)
print(summary(fit_Arima4))
checkresiduals(fit_Arima4)


# forecast the ARIMA model and viewing the statistics

fcast4 <- forecast(fit_Arima4, h = 1)
print(summary(fcast4))
plot(fcast4)

# Reudial Plots sample vs Theoretical

qqnorm(fcast4$residuals)
acf(fcast4$residuals)
pacf(fcast4$residuals)


# checking the stationary

adf.test(fcast4$residuals)
kpss.test(fcast4$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima4))

# Method -II

accuracy(fit_Arima4)

# forecast plot

autoplot(fcast4)+ggtitle('Time series Graph')+ylab('REVENUE.FROM.OPERATIONS..NET.')

print(summary(fcast4))

#------------------------------------------------------------------------------#

# Visuilizing the data of x5

autoplot(x5) + ggtitle('Time series Graph')+ylab('TOTAL.OPERATING.REVENUES')

#Build ARIMA Model and check statistics

fit_Arima5 <- auto.arima(x5)
print(summary(fit_Arima5))
checkresiduals(fit_Arima5)


# forecast the ARIMA model and viewing the statistics

fcast5 <- forecast(fit_Arima5, h = 1)
print(summary(fcast5))
plot(fcast5)

# Reudial Plots sample vs Theoretical

qqnorm(fcast5$residuals)
acf(fcast5$residuals)
pacf(fcast5$residuals)


# checking the stationary

adf.test(fcast5$residuals)
kpss.test(fcast5$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima5))

# Method -II

accuracy(fit_Arima5)

# forecast plot

autoplot(fcast5)+ggtitle('Time series Graph')+ylab('TOTAL.OPERATING.REVENUES')

print(summary(fcast5))

#------------------------------------------------------------------------------#

# Visuilizing the data of x6

autoplot(x6) + ggtitle('Time series Graph')+ylab('Other.Income')

#Build ARIMA Model and check statistics

fit_Arima6 <- auto.arima(x6)
print(summary(fit_Arima6))
checkresiduals(fit_Arima6)


# forecast the ARIMA model and viewing the statistics

fcast6 <- forecast(fit_Arima6, h = 1)
print(summary(fcast6))
plot(fcast6)

# Reudial Plots sample vs Theoretical

qqnorm(fcast6$residuals)
acf(fcast6$residuals)
pacf(fcast6$residuals)


# checking the stationary

adf.test(fcast6$residuals)
kpss.test(fcast6$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima6))

# Method -II

accuracy(fit_Arima6)

# forecast plot

autoplot(fcast6)+ggtitle('Time series Graph')+ylab('Other.Income')

print(summary(fcast6))

#------------------------------------------------------------------------------#

# Visuilizing the data of x7

autoplot(x7) + ggtitle('Time series Graph')+ylab('TOTAL.REVENUE')

#Build ARIMA Model and check statistics

fit_Arima7 <- auto.arima(x7)
print(summary(fit_Arima7))
checkresiduals(fit_Arima7)


# forecast the ARIMA model and viewing the statistics

fcast7 <- forecast(fit_Arima7, h = 1)
print(summary(fcast7))
plot(fcast7)

# Reudial Plots sample vs Theoretical

qqnorm(fcast7$residuals)
acf(fcast7$residuals)
pacf(fcast7$residuals)


# checking the stationary

adf.test(fcast7$residuals)
kpss.test(fcast7$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima7))

# Method -II

accuracy(fit_Arima7)

# forecast plot

autoplot(fcast7)+ggtitle('Time series Graph')+ylab('TOTAL.REVENUE')

print(summary(fcast7))

#------------------------------------------------------------------------------#

# Visuilizing the data of x8

autoplot(x8) + ggtitle('Time series Graph')+ylab('Cost.Of.Materials.Consumed')

#Build ARIMA Model and check statistics

fit_Arima8 <- auto.arima(x8)
print(summary(fit_Arima8))
checkresiduals(fit_Arima8)


# forecast the ARIMA model and viewing the statistics

fcast8 <- forecast(fit_Arima8, h = 1)
print(summary(fcast8))
plot(fcast8)

# Reudial Plots sample vs Theoretical

qqnorm(fcast8$residuals)
acf(fcast8$residuals)
pacf(fcast8$residuals)


# checking the stationary

adf.test(fcast8$residuals)
kpss.test(fcast8$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima8))

# Method -II

accuracy(fit_Arima8)

# forecast plot

autoplot(fcast8)+ggtitle('Time series Graph')+ylab('Cost.Of.Materials.Consumed')

print(summary(fcast8))

#------------------------------------------------------------------------------#

# Visuilizing the data of x10

autoplot(x10) + ggtitle('Time series Graph')+ylab('Changes.In.Inventories.Of.FG.WIP.And.Stock.In.Trade')

#Build ARIMA Model and check statistics

fit_Arima10 <- auto.arima(x10)
print(summary(fit_Arima10))
checkresiduals(fit_Arima10)


# forecast the ARIMA model and viewing the statistics

fcast10 <- forecast(fit_Arima10, h = 1)
print(summary(fcast10))
plot(fcast10)

# Reudial Plots sample vs Theoretical

qqnorm(fcast10$residuals)
acf(fcast10$residuals)
pacf(fcast10$residuals)


# checking the stationary

adf.test(fcast10$residuals)
kpss.test(fcast10$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima10))

# Method -II

accuracy(fit_Arima10)

# forecast plot

autoplot(fcast10)+ggtitle('Time series Graph')+ylab('Changes.In.Inventories.Of.FG.WIP.And.Stock.In.Trade')

print(summary(fcast10))

#------------------------------------------------------------------------------#

# Visuilizing the data of x11

autoplot(x11) + ggtitle('Time series Graph')+ylab('Employee.Benefit.Expenses')

#Build ARIMA Model and check statistics

fit_Arima11 <- auto.arima(x11)
print(summary(fit_Arima11))
checkresiduals(fit_Arima11)


# forecast the ARIMA model and viewing the statistics

fcast11 <- forecast(fit_Arima11, h = 1)
print(summary(fcast11))
plot(fcast11)

# Reudial Plots sample vs Theoretical

qqnorm(fcast11$residuals)
acf(fcast11$residuals)
pacf(fcast11$residuals)


# checking the stationary

adf.test(fcast11$residuals)
kpss.test(fcast11$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima11))

# Method -II

accuracy(fit_Arima11)

# forecast plot

autoplot(fcast11)+ggtitle('Time series Graph')+ylab('Employee.Benefit.Expenses')

print(summary(fcast11))

#------------------------------------------------------------------------------#

# Visuilizing the data of x12

autoplot(x12) + ggtitle('Time series Graph')+ylab('Finance.Costs')

#Build ARIMA Model and check statistics

fit_Arima12 <- auto.arima(x12)
print(summary(fit_Arima12))
checkresiduals(fit_Arima12)


# forecast the ARIMA model and viewing the statistics

fcast12 <- forecast(fit_Arima12, h = 1)
print(summary(fcast12))
plot(fcast12)

# Reudial Plots sample vs Theoretical

qqnorm(fcast12$residuals)
acf(fcast12$residuals)
pacf(fcast12$residuals)


# checking the stationary

adf.test(fcast12$residuals)
kpss.test(fcast12$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima12))

# Method -II

accuracy(fit_Arima12)

# forecast plot

autoplot(fcast12)+ggtitle('Time series Graph')+ylab('Finance.Costs')

print(summary(fcast12))

#------------------------------------------------------------------------------#

# Visuilizing the data of x13

autoplot(x13) + ggtitle('Time series Graph')+ylab('Depreciation.And.Amortisation.Expenses')

#Build ARIMA Model and check statistics

fit_Arima13 <- auto.arima(x13)
print(summary(fit_Arima13))
checkresiduals(fit_Arima13)


# forecast the ARIMA model and viewing the statistics

fcast13 <- forecast(fit_Arima13, h = 1)
print(summary(fcast13))
plot(fcast13)

# Reudial Plots sample vs Theoretical

qqnorm(fcast13$residuals)
acf(fcast13$residuals)
pacf(fcast13$residuals)


# checking the stationary

adf.test(fcast13$residuals)
kpss.test(fcast13$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima13))

# Method -II

accuracy(fit_Arima13)

# forecast plot

autoplot(fcast13)+ggtitle('Time series Graph')+ylab('Depreciation.And.Amortisation.Expenses')

print(summary(fcast13))

#------------------------------------------------------------------------------#

# Visuilizing the data of x14

autoplot(x14) + ggtitle('Time series Graph')+ylab('Other.Expenses')

#Build ARIMA Model and check statistics

fit_Arima14 <- auto.arima(x14)
print(summary(fit_Arima14))
checkresiduals(fit_Arima14)


# forecast the ARIMA model and viewing the statistics

fcast14 <- forecast(fit_Arima14, h = 1)
print(summary(fcast14))
plot(fcast14)

# Reudial Plots sample vs Theoretical

qqnorm(fcast14$residuals)
acf(fcast14$residuals)
pacf(fcast14$residuals)


# checking the stationary

adf.test(fcast14$residuals)
kpss.test(fcast14$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima14))

# Method -II

accuracy(fit_Arima14)

# forecast plot

autoplot(fcast14)+ggtitle('Time series Graph')+ylab('Other.Expenses')

print(summary(fcast14))

#------------------------------------------------------------------------------#

# Visuilizing the data of x15

autoplot(x15) + ggtitle('Time series Graph')+ylab('TOTAL.EXPENSES')

#Build ARIMA Model and check statistics

fit_Arima15 <- auto.arima(x15)
print(summary(fit_Arima15))
checkresiduals(fit_Arima15)


# forecast the ARIMA model and viewing the statistics

fcast15 <- forecast(fit_Arima15, h = 1)
print(summary(fcast15))
plot(fcast15)

# Reudial Plots sample vs Theoretical

qqnorm(fcast15$residuals)
acf(fcast15$residuals)
pacf(fcast15$residuals)


# checking the stationary

adf.test(fcast15$residuals)
kpss.test(fcast15$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima15))

# Method -II

accuracy(fit_Arima15)

# forecast plot

autoplot(fcast15)+ggtitle('Time series Graph')+ylab('TOTAL.EXPENSES')

print(summary(fcast15))

#------------------------------------------------------------------------------#

# Visuilizing the data of x16

autoplot(x16) + ggtitle('Time series Graph')+ylab('PROFIT.LOSS.BEFORE.EXCEPTIONAL..EXTRAORDINARY.ITEMS.AND.TAX')

#Build ARIMA Model and check statistics

fit_Arima16 <- auto.arima(x16)
print(summary(fit_Arima16))
checkresiduals(fit_Arima16)


# forecast the ARIMA model and viewing the statistics

fcast16 <- forecast(fit_Arima16, h = 1)
print(summary(fcast16))
plot(fcast16)

# Reudial Plots sample vs Theoretical

qqnorm(fcast16$residuals)
acf(fcast16$residuals)
pacf(fcast16$residuals)


# checking the stationary

adf.test(fcast16$residuals)
kpss.test(fcast16$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima16))

# Method -II

accuracy(fit_Arima16)

# forecast plot

autoplot(fcast16)+ggtitle('Time series Graph')+ylab('PROFIT.LOSS.BEFORE.EXCEPTIONAL..EXTRAORDINARY.ITEMS.AND.TAX')

print(summary(fcast16))

#------------------------------------------------------------------------------#

# Visuilizing the data of x18

autoplot(x18) + ggtitle('Time series Graph')+ylab('PROFIT.LOSS.BEFORE.TAX')

#Build ARIMA Model and check statistics

fit_Arima18 <- auto.arima(x18)
print(summary(fit_Arima18))
checkresiduals(fit_Arima18)


# forecast the ARIMA model and viewing the statistics

fcast18 <- forecast(fit_Arima18, h = 1)
print(summary(fcast18))
plot(fcast18)

# Reudial Plots sample vs Theoretical

qqnorm(fcast18$residuals)
acf(fcast18$residuals)
pacf(fcast18$residuals)


# checking the stationary

adf.test(fcast18$residuals)
kpss.test(fcast18$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima18))

# Method -II

accuracy(fit_Arima18)

# forecast plot

autoplot(fcast18)+ggtitle('Time series Graph')+ylab('PROFIT.LOSS.BEFORE.TAX')

print(summary(fcast18))

#------------------------------------------------------------------------------#


# Visuilizing the data of x19

autoplot(x19) + ggtitle('Time series Graph')+ylab('Current.Tax')

#Build ARIMA Model and check statistics

fit_Arima19 <- auto.arima(x19)
print(summary(fit_Arima19))
checkresiduals(fit_Arima19)


# forecast the ARIMA model and viewing the statistics

fcast19 <- forecast(fit_Arima19, h = 1)
print(summary(fcast19))
plot(fcast19)

# Reudial Plots sample vs Theoretical

qqnorm(fcast19$residuals)
acf(fcast19$residuals)
pacf(fcast19$residuals)


# checking the stationary

adf.test(fcast19$residuals)
kpss.test(fcast19$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima19))

# Method -II

accuracy(fit_Arima19)

# forecast plot

autoplot(fcast19)+ggtitle('Time series Graph')+ylab('Current.Tax')

print(summary(fcast19))

#------------------------------------------------------------------------------#

# Visuilizing the data of x21

autoplot(x21) + ggtitle('Time series Graph')+ylab('Deferred.Tax')

#Build ARIMA Model and check statistics

fit_Arima21 <- auto.arima(x21)
print(summary(fit_Arima21))
checkresiduals(fit_Arima21)


# forecast the ARIMA model and viewing the statistics

fcast21 <- forecast(fit_Arima21, h = 1)
print(summary(fcast21))
plot(fcast21)

# Reudial Plots sample vs Theoretical

qqnorm(fcast21$residuals)
acf(fcast21$residuals)
pacf(fcast21$residuals)


# checking the stationary

adf.test(fcast21$residuals)
kpss.test(fcast21$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima21))

# Method -II

accuracy(fit_Arima21)

# forecast plot

autoplot(fcast21)+ggtitle('Time series Graph')+ylab('Deferred.Tax')

print(summary(fcast21))

#------------------------------------------------------------------------------#

# Visuilizing the data of x23

autoplot(x23) + ggtitle('Time series Graph')+ylab('TOTAL.TAX.EXPENSES')

#Build ARIMA Model and check statistics

fit_Arima23 <- auto.arima(x23)
print(summary(fit_Arima23))
checkresiduals(fit_Arima23)


# forecast the ARIMA model and viewing the statistics

fcast23 <- forecast(fit_Arima23, h = 1)
print(summary(fcast23))
plot(fcast23)

# Reudial Plots sample vs Theoretical

qqnorm(fcast23$residuals)
acf(fcast23$residuals)
pacf(fcast23$residuals)


# checking the stationary

adf.test(fcast23$residuals)
kpss.test(fcast23$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima23))

# Method -II

accuracy(fit_Arima23)

# forecast plot

autoplot(fcast23)+ggtitle('Time series Graph')+ylab('TOTAL.TAX.EXPENSES')

print(summary(fcast23))

#------------------------------------------------------------------------------#

# Visuilizing the data of x24

autoplot(x24) + ggtitle('Time series Graph')+ylab('PROFIT.LOSS.AFTER.TAX.AND.BEFORE.EXTRAORDINARY.ITEMS')

#Build ARIMA Model and check statistics

fit_Arima24 <- auto.arima(x24)
print(summary(fit_Arima24))
checkresiduals(fit_Arima24)


# forecast the ARIMA model and viewing the statistics

fcast24 <- forecast(fit_Arima24, h = 1)
print(summary(fcast24))
plot(fcast24)

# Reudial Plots sample vs Theoretical

qqnorm(fcast24$residuals)
acf(fcast24$residuals)
pacf(fcast24$residuals)


# checking the stationary

adf.test(fcast24$residuals)
kpss.test(fcast24$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima24))

# Method -II

accuracy(fit_Arima24)

# forecast plot

autoplot(fcast24)+ggtitle('Time series Graph')+ylab('PROFIT.LOSS.AFTER.TAX.AND.BEFORE.EXTRAORDINARY.ITEMS')

print(summary(fcast24))

#------------------------------------------------------------------------------#

# Visuilizing the data of x25

autoplot(x25) + ggtitle('Time series Graph')+ylab('PROFIT.LOSS.FROM.CONTINUING.OPERATIONS')

#Build ARIMA Model and check statistics

fit_Arima25 <- auto.arima(x25)
print(summary(fit_Arima25))
checkresiduals(fit_Arima25)


# forecast the ARIMA model and viewing the statistics

fcast25 <- forecast(fit_Arima25, h = 1)
print(summary(fcast25))
plot(fcast25)

# Reudial Plots sample vs Theoretical

qqnorm(fcast25$residuals)
acf(fcast25$residuals)
pacf(fcast25$residuals)


# checking the stationary

adf.test(fcast25$residuals)
kpss.test(fcast25$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima25))

# Method -II

accuracy(fit_Arima25)

# forecast plot

autoplot(fcast25)+ggtitle('Time series Graph')+ylab('PROFIT.LOSS.FROM.CONTINUING.OPERATIONS')

print(summary(fcast25))

#------------------------------------------------------------------------------#

# Visuilizing the data of x26

autoplot(x26) + ggtitle('Time series Graph')+ylab('PROFIT.LOSS.FOR.THE.PERIOD')

#Build ARIMA Model and check statistics

fit_Arima26 <- auto.arima(x26)
print(summary(fit_Arima26))
checkresiduals(fit_Arima26)


# forecast the ARIMA model and viewing the statistics

fcast26 <- forecast(fit_Arima26, h = 1)
print(summary(fcast26))
plot(fcast26)

# Reudial Plots sample vs Theoretical

qqnorm(fcast26$residuals)
acf(fcast26$residuals)
pacf(fcast26$residuals)


# checking the stationary

adf.test(fcast26$residuals)
kpss.test(fcast26$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima26))

# Method -II

accuracy(fit_Arima26)

# forecast plot

autoplot(fcast26)+ggtitle('Time series Graph')+ylab('PROFIT.LOSS.FOR.THE.PERIOD')

print(summary(fcast26))

#------------------------------------------------------------------------------#

# Visuilizing the data of x27

autoplot(x27) + ggtitle('Time series Graph')+ylab('Basic.EPS..Rs..')

#Build ARIMA Model and check statistics

fit_Arima27 <- auto.arima(x27)
print(summary(fit_Arima27))
checkresiduals(fit_Arima27)


# forecast the ARIMA model and viewing the statistics

fcast27 <- forecast(fit_Arima27, h = 1)
print(summary(fcast27))
plot(fcast27)

# Reudial Plots sample vs Theoretical

qqnorm(fcast27$residuals)
acf(fcast27$residuals)
pacf(fcast27$residuals)


# checking the stationary

adf.test(fcast27$residuals)
kpss.test(fcast27$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima27))

# Method -II

accuracy(fit_Arima27)

# forecast plot

autoplot(fcast27)+ggtitle('Time series Graph')+ylab('Basic.EPS..Rs..')

print(summary(fcast27))

#------------------------------------------------------------------------------#


# Visuilizing the data of x28

autoplot(x28) + ggtitle('Time series Graph')+ylab('Diluted.EPS..Rs..')

#Build ARIMA Model and check statistics

fit_Arima28 <- auto.arima(x28)
print(summary(fit_Arima28))
checkresiduals(fit_Arima28)


# forecast the ARIMA model and viewing the statistics

fcast28 <- forecast(fit_Arima28, h = 1)
print(summary(fcast28))
plot(fcast28)

# Reudial Plots sample vs Theoretical

qqnorm(fcast28$residuals)
acf(fcast28$residuals)
pacf(fcast28$residuals)


# checking the stationary

adf.test(fcast28$residuals)
kpss.test(fcast28$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima28))

# Method -II

accuracy(fit_Arima28)

# forecast plot

autoplot(fcast28)+ggtitle('Time series Graph')+ylab('Diluted.EPS..Rs..')

print(summary(fcast28))

#------------------------------------------------------------------------------#

# Visuilizing the data of x33

autoplot(x33) + ggtitle('Time series Graph')+ylab('Equity.Share.Dividend')

#Build ARIMA Model and check statistics

fit_Arima33 <- auto.arima(x33)
print(summary(fit_Arima33))
checkresiduals(fit_Arima33)


# forecast the ARIMA model and viewing the statistics

fcast33 <- forecast(fit_Arima33, h = 1)
print(summary(fcast33))
plot(fcast33)

# Reudial Plots sample vs Theoretical

qqnorm(fcast33$residuals)
acf(fcast33$residuals)
pacf(fcast33$residuals)


# checking the stationary

adf.test(fcast33$residuals)
kpss.test(fcast33$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima33))

# Method -II

accuracy(fit_Arima33)

# forecast plot

autoplot(fcast33)+ggtitle('Time series Graph')+ylab('Equity.Share.Dividend')

print(summary(fcast33))

#------------------------------------------------------------------------------#


# Visuilizing the data of x34

autoplot(x34) + ggtitle('Time series Graph')+ylab('Tax.On.Dividend')

#Build ARIMA Model and check statistics

fit_Arima34 <- auto.arima(x34)
print(summary(fit_Arima34))
checkresiduals(fit_Arima34)


# forecast the ARIMA model and viewing the statistics

fcast34 <- forecast(fit_Arima34, h = 1)
print(summary(fcast34))
plot(fcast34)

# Reudial Plots sample vs Theoretical

qqnorm(fcast34$residuals)
acf(fcast34$residuals)
pacf(fcast34$residuals)


# checking the stationary

adf.test(fcast34$residuals)
kpss.test(fcast34$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima34))

# Method -II

accuracy(fit_Arima34)

# forecast plot

autoplot(fcast34)+ggtitle('Time series Graph')+ylab('Tax.On.Dividend')

print(summary(fcast34))

#------------------------------------------------------------------------------#



# Visuilizing the data of x35

autoplot(x35) + ggtitle('Time series Graph')+ylab('Equity.Dividend.Rate....')

#Build ARIMA Model and check statistics

fit_Arima35 <- auto.arima(x35)
print(summary(fit_Arima35))
checkresiduals(fit_Arima35)


# forecast the ARIMA model and viewing the statistics

fcast35 <- forecast(fit_Arima35, h = 1)
print(summary(fcast35))
plot(fcast35)

# Reudial Plots sample vs Theoretical

qqnorm(fcast35$residuals)
acf(fcast35$residuals)
pacf(fcast35$residuals)


# checking the stationary

adf.test(fcast35$residuals)
kpss.test(fcast35$residuals)

# Accuracy of Auto-Arima by 2 methods

# Method -I

print(summary(fit_Arima35))

# Method -II

accuracy(fit_Arima35)

# forecast plot

autoplot(fcast35)+ggtitle('Time series Graph')+ylab('Equity.Dividend.Rate....')

print(summary(fcast35))

#------------------------------------------------------------------------------#



