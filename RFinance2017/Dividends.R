###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
###############################################################################
# Examples for the R/Finance Presentation
# Copyright (C) 2017  Vyacheslav Arbuzov
#
# For more information please visit my site at www.rusquant.ru
# or drop me a line at arbuzov1989@gmail.com
###############################################################################


#############################################################################
# Dividend_strategy - towards to the efficient market
# R/Finance 2017
###############################################################################

#getDividends
library(quantmod)
library(data.table)
library(scales)

stock_names <- read.csv('https://raw.githubusercontent.com/arbuzovv/RFinance_docs/master/RFinance2017/stock_list_1850.txt',stringsAsFactors = FALSE)

#try to donwload dividends data, it needs few minutes for execution
for(i in 1:length(stock_names[,1]))
{
  x <- try(getDividends(stock_names[i,1],from = "1990-01-01"),silent = TRUE)
  if(class(x)[1]!="logical" &  class(x)[1]!="try-error" & length(x)!=0)
  {
    dividens <- data.frame(stock_names[i,1],index(x),x)
    names(dividens) <- c('Symbol','Date','Dividend')
    if(exists('dividens_list') == TRUE)
      dividens_list <- rbind(dividens_list,dividens)
    if(exists('dividens_list') == FALSE)
      dividens_list <- dividens
  }
  print(c(i,stock_names[i,1]))
}
View(dividens_list)

#try to download price data, it needs few minutes for execution
for(i in 1433:length(stock_names[,1]))
{
  x <- try(getSymbols(stock_names[i,1],from = "1990-01-01",auto.assign = FALSE),silent = TRUE)
  if(class(x)[1]!="logical" &  class(x)[1]!="try-error" & length(x)!=0)
  {
    stock_price <- data.frame(stock_names[i,1],index(x),x)
    names(stock_price) <- c('Symbol','Date','Open','High','Low','Close','Volume','Adjusted')
    if(exists('stock_price_list') == TRUE)
      stock_price_list <- rbind(stock_price_list,stock_price)
    if(exists('stock_price_list') == FALSE)
      stock_price_list <- stock_price
  }
  print(c(i,stock_names[i,1]))
}


rel_div <- merge(dividens_list,stock_price_list,by = c('Symbol','Date'),all.x = TRUE)
rel_div$dividend_yield <- 100*rel_div$Dividend/rel_div$Open
hist(rel_div$dividend_yield,breaks = 400000,xlim=c(0,4),xlab='Dividend yield',main='')

stock_price_list <- data.table(stock_price_list)
night_gaps <- data.frame(stock_price_list[,list(Date[2:length(Date)],NightGap = Open[2:(length(Open))]/Close[1:length(Close)]-1),by='Symbol'])
names(night_gaps)[2] <- 'Date'

div_returns <- merge(rel_div,night_gaps,by = c('Symbol','Date'),all.x = TRUE)
plot(div_returns$dividend_yield,100*div_returns$NightGap,xlim = c(0,4),ylim=c(-3,3),ylab = 'Night gap return, %',xlab = 'Dividend yield, %',cex=.2,col=alpha('darkgreen',0.4))
grid()

