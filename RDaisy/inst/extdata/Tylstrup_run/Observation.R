#Nitrogen content

N_content<-fread("F:/RDaisy_calib/obs/succups.csv") 
N_content[,date:=dmy(date)]
N_content[,season:=ifelse(month(date) < 5, year(date)-1 , year(date))]

N_content.m<-melt.data.table(N_content,measure.vars = c("SCA1","SCA2","SCB1","SCB2"), id.vars = c("date","season"))

for(i in 1:length(unique(N_content.m$variable))){
  i_name<-unique(N_content.m$variable)[i]
  
  for(j in 1:length(unique(N_content.m$season))){
    
    Year<-unique(N_content.m$season)[j]
   
      OutLier<-boxplot.stats(N_content.m[variable==i_name & season==Year]$value)
      
      N_content.m[value %in% OutLier$out,value:=NA]
    }
  
}
sum(is.na(N_content.m))
ggplot(melt.data.table(N_content,measure.vars = c("SCA1","SCA2","SCB1","SCB2"),
                       id.vars = "date"))+geom_line(aes(date,value))+facet_grid(variable~.)

N_content.m.d<-as.data.table(dcast.data.table(
  N_content.m,
  date~variable,
  value.var = "value",
  fill=NA
))

N_content.m.d[,AVE1:=rowMeans(N_content.m.d[,.(SCA1,SCB1)])]
N_content.m.d[,AVE2:=rowMeans(N_content.m.d[,.(SCA2,SCB2)])]

ggplot(N_content.m.d)+geom_point(aes(x=date,y=SCB1,col="B"))+geom_point(aes(x=date,y=SCA1,col="A"))
N_content.m.d[,AVE1:=SCB1]
N_content.m.d[,AVE2:=SCB2]

#Yield

Yield <- fread("F:/RDaisy_calib/obs/harvest.csv")
Yield[,date:=dmy(date)]


#BBCH
BBCH<-fread("F:/RDaisy_calib/obs/BBCH.csv")
BBCH[,date:=dmy(date)]

#Soil water content

tdr.obs<-fread("F:/RDaisy_calib/obs/tdr_obs.csv")
tdr.obs[,date:=dmy(date)]
tdr.obs[,season:=ifelse(month(date) < 5, year(date)-1 , year(date))]
tdr.obs<-tdr.obs[!duplicated(date)]

tdr.obs.m<-melt.data.table(tdr.obs,id.vars = c("date","season"), measure.vars = c( "S25","S60","S90","S110","S190","S210"))

for(i in 1:length(unique(tdr.obs.m$variable))){
  i_name<-unique(tdr.obs.m$variable)[i]

  for(j in 1:length(unique(tdr.obs.m$season))){
    
    Year<-unique(tdr.obs.m$season)[j]
    
    for(k in 1:length(unique(month(tdr.obs.m$date)))){
      
      Month<-unique(month(tdr.obs.m$date))[c(k,k+1)]

      OutLier<-boxplot.stats(tdr.obs.m[variable==i_name & season==Year & month(date) %in% Month]$value)
    
      tdr.obs.m[value %in% OutLier$out,value:=NA]
      }
    }
}
sum(is.na(tdr.obs.m))
ggplot(tdr.obs.m)+geom_line(aes(x=date,y=value))+facet_grid(variable~.)

tdr.obs.m.d<-as.data.table(dcast.data.table(
  tdr.obs.m,
  date~variable,
  value.var = "value",
  fill=NA
  ))

#Compiled Observation
TYLS_obs<- list(N_content.m.d,tdr.obs.m.d,BBCH,Yield,Calib.T3)

names(TYLS_obs)<- c("N_content","tdr.obs","BBCH","Yield","Calib.T")


