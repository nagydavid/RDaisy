###weather Check

library(data.table)
library(lubridate)
library(ggplot2)

TT_weather<-fread("F:/RDaisy_calib/obs/340560685.csv")

TT_weather_AER<-fread("F:/RDaisy_calib/obs/TT_AER.csv")

TT_weather_AER[,date:=dmy(Date)]

TT_weather[,minrh:=NULL][,maxrh:=NULL][,vpd:=NULL][,makkepot:=NULL]

#vapor pressure

TT_weather[, es:= ifelse(is.na(temp), NA, 
                             ifelse(temp>0 , 6.11*exp(log(10)*7.5*temp/(temp+237.3)),
                                    6.11*exp(log(10)*9.5*temp/(temp+265.5))))]

TT_weather[, ea:=rh/100*es]

TT_weather[, ea:=100*ea]

TT_weather[,wv2:=wv*(log(2/0.001)/log(10/0.001))]

setorder(TT_weather, date, na.last=FALSE)

TT_weather[,date:=ymd(date)]

TT_weather[,year:=year(date)]

TT_weather[,month:=month(date)]

TT_weather[,day:=day(date)]

plot(TT_weather$date,TT_weather$glorad)
hist(TT_weather$glorad)
str(TT_weather)
TT_weather[,glorad:=glorad*(1000000/(3600*24))] 



TT_weather<-merge(TT_weather,TT_weather_AER[,.SD,.SDcols=c("date","Precipitation")],all.x=T,by="date")

corr<-data.table(coef=c(1.21,	1.22,	1.22,	1.18,	1.15,	1.14,	1.15,	1.11,	1.13,	1.14,	1.16,	1.19),month=1:12)

TT_weather<-merge(TT_weather,corr[,.SD,.SDcols=c("month","coef")],all.x=T,by="month")

TT_weather[,Prec_AER:=Precipitation/coef]

TT_weather[prec08=="null",prec08:=NA][,prec08:=as.numeric(prec08)]

TT_weather[is.na(prec08),prec08:=Prec_AER][,Prec_AER:=NULL][,Precipitation:=NULL]

TT_weather[minte=="null",minte:=NA]

TT_weather[,minte:=as.numeric(minte)]

TT_weather[,minte:=ifelse(is.na(minte),as.numeric(shift(minte, 1L, type="lag")),minte)]

str(TT_weather)

TT_weather<-TT_weather[order(date)]

TT_weather[,x:=paste(year, month,day,glorad,maxte, minte,temp,ea,wv2,prec08,sep = " ")]

HEADing<-data.table(x=as.character(readLines("F:/RDaisy_calib/weather_heading.txt")))

x<-rbind(HEADing[,"x",with=F],TT_weather[,"x",with=F])

write.table(x,file = "Tylstrup_New.dwf",sep = "\t",row.names = F,col.names = F)
