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
# Estimation of LPPL model for Russian market
# R/Finance 2017
###############################################################################

#getDividends
library(quantmod)
stock_names <- read.table('D:/user_file/Arbuzov/stock_list_1850.txt',stringsAsFactors = FALSE)

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

