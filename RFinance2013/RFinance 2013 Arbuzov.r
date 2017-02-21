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
# Copyright (C) 2013  Vyacheslav Arbuzov
#
# For more information please visit my site at www.r-group.mifit.ru
# or drop me a line at arbuzov1989@gmail.com
###############################################################################


#############################################################################
# Estimation of LPPL model for Russian market
# R/Finance 2013
###############################################################################
#connect necessary packages
library(quantmod)
#input parametr in cluster for parallel computaion
args<-"AAPL"
#download data
d<-getSymbols(args, from="1995-01-01", period="day", auto.assign=FALSE) 
#convert time to numeric
time(d)->t
as.double(d[,4])->p
format(t, "%Y")->t1
format(t, "%m")->t2
format(t, "%d")->t3
as.numeric(t1)->t1
as.numeric(t2)->t2
as.numeric(t3)->t3
d<-c(0,0.0849,0.16164,0.2465,0.3287,0.4136,0.4958,0.5808,0.6657,0.7479,0.8328,0.91506)
d[t2]->t2
t3<-t3/365
t<-t1+t2+t3
#if we want to search future bubbles
length(t)->lg
for(i in 1:600)
t[lg +i]<-t[lg +i-1]+0.002739
rere<- array (0,dim=c(48000,9,5))# array of result models
length(t)->full_lg
#------------------------------------------------------------------------------------------------------------------------------
#parametrs for windows of calibrations
pos<-lg
i<- 50 #start of the end of bubble
k<-1# start of the start of bubble
numb_model<-0 # number of model for result array
stepcalc<-60 # step for estimation
plot(t[1:full_lg],p[1:full_lg],xlim=c(min(t),max(t)), ylim=c(min(p),max(p)),type="l", main=args, xlab="t",ylab="p") # plot instrument


#------------------------------------------------------------------------------------------------------------------------------
#estimation of critical time
while(k<=pos)
{
	plot(t[1:full_lg],p[1:full_lg],xlim=c(min(t),max(t)), ylim=c(min(p),max(p)),type="l", main=args, xlab="t",ylab="p")
	while(i<full_lg)
	{
		if(pos<i) p2<-p[k:pos]
		if(pos>=i)  p2<-p[k:i]
		tc<-t[i+1]
		if(pos<i) t2<-t[k:pos]
		if(pos>=i)  t2<-t[k:i]
		t4<-tc-t[k:i]
		t3<- tc-t2
		resid<-array(0,dim=c(120,8))
		sp<-array(0,dim=c(4,1))
		sp2<-array(0,dim=c(4,4))
		#initial parameters
		omega<-2
		m<-0.05
		j2<-0
		j1<-0
		j<-0
		s<-0
		#grid for the two parameters omega and m
		for(j2 in 1 :12)
		{
			for(j1  in 1 :5)
			{
				s<-s+1
				ft<-t3^m
				ft*cos(log(t3,base=exp(1))*omega)->gt
				ft*sin(log(t3,base=exp(1))*omega)->ht
				sum(p2)->sp[1,1]
				sum(p2*ft)->sp[2,1]
				sum(p2*gt)->sp[3,1]
				sum(p2*ht)->sp[4,1]
				sp2[1,1]<-length(p2)
				sp2[2,1]<-sum(ft)
				sp2[1,2]<-sum(ft)
				sp2[1,3]<-sum(gt)
				sp2[3,1]<-sum(gt)
				sp2[4,1]<-sum(ht)
				sp2[1,4]<-sum(ht)
				sp2[2,2]<-sum(ft*ft)
				sp2[3,3]<-sum(gt*gt)
				sp2[4,4]<-sum(ht*ht)
				sp2[4,3]<-sum(gt*ht)#sum(ft*ht)
				sp2[3,4]<-sum(gt*ht)#sum(ft*ht)
				sp2[2,4]<-sum(ht*ft)#sum(gt*ft)
				sp2[4,2]<-sum(ht*ft)#sum(gt*ft)
				sp2[2,3]<-sum(gt*ft)
				sp2[3,2]<-sum(gt*ft)
				if(is.numeric(try(solve(sp2), silent = TRUE))==TRUE) 
					{
					solve(sp2)%*%sp->abc
					pm2<-abc[1]+ abc[2]*ft+ abc[3]*gt
					resid[s,1]<-sum((pm2-p2)^2)
					}
				resid[s,2]<-omega
				resid[s,3]<-m
				resid[s,4]<-abc[1]
				resid[s,5]<-abc[2]
				resid[s,6]<-abc[3]
				resid[s,7]<-abc[4]
				resid[s,8]<-s
				omega<-omega+2
			}
			omega<-2
			m<-m+0.25
		}
		numb_model<-numb_model+1
		#Gauss-Newton for the two minima on the grid
			for(j2 in 1:2)
				{
				#minima on the grid
				resid[resid[,2]>0,]->resid
				resid[resid[,1]==min(resid[,1]),8]->t6#best scenario
				resid[resid[,8]==t6,] 
				resid[resid[,8]==t6,1]  <-100000000	
				#Gauss–Newton algorithm
				if(is.numeric(try(coef(nls(p2 ~A+B*(t3^m)+C1*(t3^m)*cos((log(t3,base=exp(1))*omega))+C2*(t3^m)*sin((log(t3,base=exp(1))*omega)), start = list(A=resid[resid[,8]==t6,4],B=resid[resid[,8]==t6,5],C1=resid[resid[,8]==t6,6],C2= resid[resid[,8]==t6,7],m= resid[resid[,8]==t6,3],omega= resid[resid[,8]==t6,2]) , control = nls.control(maxiter = 100, warnOnly = TRUE))))))
					{		
					coef(nls(p2 ~A+B*(t3^m)+C1*(t3^m)*cos((log(t3,base=exp(1))*omega))+C2*(t3^m)*sin((log(t3,base=exp(1))*omega)), start = list(A=resid[resid[,8]==t6,4],B=resid[resid[,8]==t6,5],C1=resid[resid[,8]==t6,6],C2= resid[resid[,8]==t6,7],m= resid[resid[,8]==t6,3],omega= resid[resid[,8]==t6,2]) , control = nls.control(maxiter = 100, warnOnly = TRUE)))->coo
					pm2<-coo[1]+coo[2]*(t4^coo[5])+coo[3]*(t4^coo[5])*cos( log(t4,base=exp(1))*coo[6] ) + coo[4]*(t4^coo[5])*sin( log(t4,base=exp(1))*coo[6] )
					rere[numb_model,1,j2]<- coo[1]# array of result models, record parameter
					rere[numb_model,2,j2]<- coo[2]# array of result models, record parameter
					rere[numb_model,3,j2]<- coo[3]# array of result models, record parameter
					rere[numb_model,4,j2]<- coo[4]# array of result models, record parameter
					rere[numb_model,5,j2]<- coo[5]# array of result models, record parameter
					rere[numb_model,6,j2]<- coo[6]# array of result models, record parameter
					rere[numb_model,7,j2]<- k# array of result models, record parameter
					rere[numb_model,8,j2]<- i# array of result models, record parameter
					lines(tc-t4,pm2,col=i) # plot our model
					}
				}
			i<-i+stepcalc
			print(paste("i",i))
		}	
	print(paste("k",k))
	k<-k+ stepcalc 
	i<-k+ 50 #initial length of bubble	
}
#Filtration of LPPL models
kk<-0
arr<-array(0,dim=c(12500,9))
for(j in 1:5)
{
	for(i in 1:480)
		if(rere[i,5,j]<1)
			if(rere[i,5,j]>0)
				if(rere[i,2,j]<0)
					if(rere[i,6,j]<14)
						if(rere[i,6,j]>3)
								{
								kk<-kk+1
								arr[kk,]<-rere[i,,j]
								}
}
arr[which(arr[,8]>0),]->arr
unique(arr)->arr
as.data.frame(arr[,1:8])->lppl_models
names(lppl_models)<-c("A","B","C1","C2","M","OMEGA","STARTDATE","ENDDATE")
types_col<-c("double", "double", "double", "double", "double", "double","date","date")
dateconstr<- function(d,y,z)#function for construction of date
 {  

 format( (as.Date(paste(d,".",y,".",z,sep=""),"%Y.%m.%d")),"%Y")->k
 format( (as.Date(paste(d,".",y,".",z,sep=""),"%Y.%m.%d")),"%m")->j
 format( (as.Date(paste(d,".",y,".",z,sep=""),"%Y.%m.%d")),"%d")->i
 result <- paste(k,j,i,sep="")
return(result)
}
#from numeric date to normal(start of bubble)
floor(t[lppl_models[,7]])->year
ceiling(((t[lppl_models[,7]]-floor(t[lppl_models[,7]]))*365)/30)->month
round((t[lppl_models[,7]]-floor(t[lppl_models[,7]]))*365-(month-1)*30)->day
dateconstr(year,month,day)->startdate
lppl_models[,7]<-startdate
startdate->startdate
#from numeric date to normal(end of bubble)
floor(t[lppl_models[,8]])->year
ceiling(((t[lppl_models[,8]]-floor(t[lppl_models[,8]]))*365)/30)->month
round((t[lppl_models[,8]]-floor(t[lppl_models[,8]]))*365-(month-1)*30)->day
dateconstr(year,month,day)->enddate
enddate->enddate
lppl_models[,8]<-enddate
#after that we save our variable as.data.frame(lppl_models)  in database
#############################################################################
# A two-sample location test of the null hypothesis that the means of two distributed populations are equal. 
# R/Finance 2013
###############################################################################
# At Perm cluster we calculate changes in microstructure of bubbles and microstructure of nonbubbles
# downloading sample of bubbles
 url_bubbles <- "http://r-group.mifit.ru/bubbles.txt"
 download.file(url_bubbles, destfile = "bubbles.txt" )
 bubbles.data <- read.table("bubbles.txt",header=TRUE )
# downloading sample of non bubbles 
 url_non_bubbles <- "http://r-group.mifit.ru/nonbubbles.txt"
 download.file(url_non_bubbles, destfile = "nonbubbles.txt" )
 nonbubbles.data <- read.table("nonbubbles.txt",header=TRUE )
 t.test(bubbles.data[,2],nonbubbles.data[,2]) #  t- test for Duration time between orders
 t.test(bubbles.data[,3],nonbubbles.data[,3]) #  t- test for price deviation from best ask/best bid
 t.test(bubbles.data[,4],nonbubbles.data[,4]) #  t- test for volume of orders





