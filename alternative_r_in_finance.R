
library(data.table)
library(jsonlite)
library(httr)
library(meboot)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
############ original data ###############

# candles
candles = fromJSON('https://hub.limex.com/v1/candles?symbol=AAPL&token=WFU9EF92HF&from=2008-01-01&to=2024-01-01')
candles= data.table(candles)

price_ret = dailyReturn(xts(candles$c,as.Date(candles$ts)),type = 'log')
names(price_ret)[1] = 'ret'
meboot_data = meboot(x=as.numeric(price_ret$ret), reps=100,trim = 0.01)$ensemble
meboot_res = meboot_data
plot(as.numeric(100*exp(cumsum(price_ret)),type='l'),ylim=c(1,10000))
grid()
for(i in 1:30) lines(as.numeric(100)*exp(cumsum(meboot_res[,i])),col='gray')

meboot_data = meboot(x=as.numeric(diff(income$value)), reps=100,trim = 0.01)$ensemble
plot(cumsum(diff(income$value)),type='l',ylim=c(-50,50))
for(i in 1:100) lines(cumsum(meboot_data[,i]),type='l',col='gray')
grid()
lines(cumsum(diff(income$value)),type='l',col='black',lwd=3)

candles = candles[order(ts)]
all_features = candles
all_features[,next_ret:=shift(c/shift(c,1)-1,-1)]
all_features$rsi_70 = TTR::RSI(all_features$c,2)
train_data = all_features[ts<'2023-01-22T08:00:00']
test_data = na.omit(all_features[ts>'2023-01-22T08:00:00'])



# brute overfit strategy
library(randomForest)
#overfit_model <- randomForest(next_ret ~ rsi_70, data=na.omit(train_data), na.action=na.omit,ntree=30,maxnodes=30)
overfit_model <- randomForest(next_ret ~ rsi_70, data=na.omit(train_data), na.action=na.omit,ntree=3,maxnodes=3)
rf_forecast <- predict(overfit_model, train_data)
train_data$predict = rf_forecast
train_data$position = 0
train_data[predict>0,]$position=1
train_data[predict<0,]$position=-1
#add commisions
train_data[,pnl:= position * next_ret]
train_data[,I_trade:= c(NA,diff(position))]
train_data$pnl = train_data$pnl - (0.05/100)*abs(train_data$I_trade)
train_pnl = xts(train_data$pnl,order.by = as.Date(train_data$ts))
charts.PerformanceSummary(na.omit(train_pnl))
real_train_sharpe = SharpeRatio.annualized(train_pnl)



alternative_sharpe = data.table()
for(i in 1:100)
{
  ### test in alternative history
  alternative_dataset = data.table(candles,as.numeric(100)*exp(cumsum(meboot_res[,i])))
  alternative_dataset[,next_ret:=shift(c/shift(c,1)-1,-1)]
  alternative_dataset$rsi_70 = TTR::RSI(alternative_dataset$V2,2)
  a_train_data = alternative_dataset[ts<'2023-01-22T08:00:00']
  rf_forecast <- predict(overfit_model, a_train_data)
  a_train_data$predict = rf_forecast
  a_train_data$position = 0
  a_train_data[predict>0,]$position=1
  a_train_data[predict<0,]$position=-1
  #add commisions
  a_train_data[,pnl:= position * next_ret]
  a_train_data[,I_trade:= c(NA,diff(position))]
  a_train_data$pnl = a_train_data$pnl - (0.05/100)*abs(a_train_data$I_trade)
  a_train_pnl = xts(a_train_data$pnl,order.by = as.Date(a_train_data$ts))
  #charts.PerformanceSummary(na.omit(a_train_pnl),geometric = T)
  alternative_sharpe = rbind(alternative_sharpe,data.table(SharpeRatio.annualized(a_train_pnl)))
}


library(ggplot2)
ggplot(alternative_sharpe, aes(V1,fill='blue',alpha=.2)) + geom_density() + xlim(0, 5) + 
  geom_vline(xintercept = real_train_sharpe)+ labs(x = "Sharpe ratio, 2008-2022, AAPL")

## ##### test data
rf_forecast <- predict(overfit_model, test_data)
test_data$predict = rf_forecast
test_data$position = 0
test_data[predict>0,]$position=1
test_data[predict<0,]$position=-1
#add commisions
test_data[,pnl:= position * next_ret]
test_data[,I_trade:= c(NA,diff(position))]
test_data$pnl = test_data$pnl - (0.05/100)*abs(test_data$I_trade)
test_pnl = xts(test_data$pnl,order.by = as.Date(test_data$ts))
charts.PerformanceSummary(na.omit(test_pnl),geometric = T)
real_test_sharpe = SharpeRatio.annualized(test_pnl)








