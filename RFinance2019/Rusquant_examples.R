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
# Copyright (C) 2018  Vyacheslav Arbuzov
#
# For more information please visit my site at www.rusquant.ru
# or drop me a line at arbuzov1989@gmail.com
###############################################################################


#############################################################################
# Rusquant: another way trading with R
# R/Finance 2019
###############################################################################

library(devtools)
install_github('arbuzovv/rusquant') # the last version of package
library(quantmod)
library(rusquant)


stock_names <- loadSymbolList(src = 'poloniex')

#download OHLCV data
price_data <- getSymbols(stock_names$Symbol[1],src='poloniex',auto.assign = FALSE)
candleChart(price_data['2017-10/'])

#load last trades
trades <- getTradelog(Symbols = stock_names$Symbol[1],auto.assign = FALSE,from = Sys.Date()-1,to = Sys.time())
buy_trades_imbalance <- sum(tail(trades,30)[,6])/length(tail(trades,30)[,6])

#calc current order book imbalance
order_book <- getOrderbook(Symbols = stock_names$Symbol[1],auto.assign = FALSE)
buy_imbalance_ratio <- sum(order_book[order_book$isAsk==0,]$Volume)/sum(order_book$Volume)


# simply momentum strategy 
conn <- Connect() # connect to exchange

position <- getPosition(conn,Symbol = stock_names$Symbol[1])
open_orders <- getOpenOrders(conn,Symbol = stock_names$Symbol[1])

if(position==0 & open_orders==NULL)
{
#open position when there is a signal
	if(buy_trades_imbalance>0.7 & buy_imbalance_ratio>0.7)
	{
	lmt_price <- max(order_book[order_book$isAsk==0,1]) # set price as best bid
	if(lmt_price >= range(trades[,3])[1] & lmt_price <= range(trades[,3])[2]) # if price is correct
		{
		lmt_order <- Order(Symbol = stock_names$Symbol[1], action = "BUY", Quantity = 1, orderType = "LMT", Price = lmt_price)
		ord_id <- openOrder(conn, lmt_order)
		print(ord_id)
		}
	}
#open position when there is a signal
	if(buy_trades_imbalance<0.3 & buy_imbalance_ratio<0.3)
	{
	lmt_price <- min(order_book[order_book$isAsk==1,1]) # set price as best bid
	if(lmt_price >= range(trades[,3])[1] & lmt_price <= range(trades[,3])[2]) # if price is correct
		{
		lmt_order <- Order(Symbol = stock_names$Symbol[1], action = "SELL", Quantity = 1, orderType = "LMT", Price = lmt_price)
		ord_id <- openOrder(conn, lmt_order)
		print(ord_id)
		}
	}
}

#close position without signal
if(position>0)
	if(buy_trades_imbalance < 0.7 | buy_imbalance_ratio < 0.7)
		lmt_order <- Order(Symbol = stock_names$Symbol[1], action = "SELL", Quantity = position[1], orderType = "MKT", Price = 0)
#close position without signal
if(position<0)
	if(buy_trades_imbalance > 0.3 | buy_imbalance_ratio > 0.3)
		lmt_order <- Order(Symbol = stock_names$Symbol[1], action = "BUY", Quantity = position[1], orderType = "MKT", Price = 0)




