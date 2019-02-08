 # devtools::install_github(repo="jeroenpullens/RDaisy",subdir = "RDaisy")

library(hydroGOF)
library(data.table)
library(sensitivity)
library(lubridate)
library(RDaisy)
library(foreach)
library(doParallel)
library(ggplot2)
library(zoo)
library(R.utils)
library(birk)
library(grid)
library(gridExtra)
library(stringi)
library(stringr)
library(FME)
library(GGally)
library(cluster)

setwd("F:/RDaisy_calib/")

# copyinternaldata("F:/Jeroen/",package_folder=NULL)

T_RunFile<-"F:/RDaisy_calib/TYLSTRUP_run.dai" # RunFile of daisy
T_path<-"C:/Program Files/Daisy 5.59/bin/daisy.exe" # path of Daisy executable
T_param<-"F:/RDaisy_calib/ParamsToCalibrate_TYLS.txt" #parameter matrix with min max and file names
T_Morris <- "F:/RDaisy_calib/Morris_f.R"

#Test run of daisy with default parameters
runDaisy(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy =  Daisy.control(sensitivity =FALSE,calib=FALSE, dflt = TRUE,costfunction=NULL,wdDir="F:/RDaisy_calib",OutDir="output",interval=c(1993:1995),year=FALSE,All=FALSE,param_sens = NULL,p.config=T_param ,obs=T_obs_all))

#test run of the Fcost function, custom parameter set ,can be tested
f.cost(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity = T,calib = FALSE, dflt = FALSE,costfunction = NULL,
       obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=1993:1995,year=TRUE,All=TRUE,ind=Sys.getpid(),param_sens = NULL,p=fread(T_param)$default, p.config=T_param))


source("Morris_f.R")
source("DEoptim_f.R")
# run of daisy with Morris function
 # Morris.SrC<-runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=TRUE,DEoptim=FALSE, dflt = FALSE,
 #          wdDir="C:/test_RDAISY",OutDir="output",interval=c(1993:1995),year=TRUE,All=TRUE, costfunction = DaisyMorris,param_sens = NULL)

Morris.SrC<-runDaisy(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity=TRUE,calib = FALSE, dflt = FALSE,wdDir="F:/RDaisy_calib",OutDir="output/",interval=c(2005:2013),year=TRUE,All=TRUE, costfunction = DaisyMorris,param_sens = NULL,p.config=T_param ,obs=TYLS_obs))


#Sensitivity treshold

Morris.SrC[,r:=sqrt(sigma^2+mu.star^2)]

Morris.SrC<-Morris.SrC[!obj=="DS"]

Morris.SrC.TH<-NULL

for(i in 1:length(unique(Morris.SrC$PeCr))){#
  
  x <- as.factor(unique(Morris.SrC$PeCr)[i])
  
  for(j in 1:length(unique(Morris.SrC[PeCr == x]$obj))){#
    
    y <- as.factor(unique(Morris.SrC[PeCr == x]$obj)[j])
    z <- Morris.SrC[PeCr == x & obj == y]
    
    ClustR <- kmeans(z[,.(mu.star,sigma)],centers = 3,iter.max = 1000)
    
    z[,group:=ClustR$cluster]
    
    ClustR
    
    xy<-unique(z[order(r)]$group)
    
    z[,group:=ifelse(group==xy[1],1,ifelse(group==xy[2],2,3))]

    Morris.SrC.TH<-rbind(Morris.SrC.TH,z)
  
}}


#############################
Morris.SrC.TH[,sens:=ifelse(group>1,1,0)]

Morris.SrC.TH[,eff:=NULL]
Morris.SrC.TH[,eff:=sum(ifelse(group>1,1,0)),by=c("par","obj") ]
Morris.SrC.TH[,eff:=as.factor(as.integer(eff))]

hist(Morris.SrC.TH$eff)
#############################
T_param_sens <- unique(Morris.SrC.TH[as.numeric(eff)>3,]$par)

Morris.SrC.TH[par %like% "WR"]

T_param_sens.DS<-fread(T_param)[name %like% "DS"]$name

##############################Sensitivity measures



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 50, c = 100)[1:n]
}
cols = gg_color_hue(length(levels(as.factor(Morris.SrC.TH$eff))))

Morris.plot.Color <- ggplot(Morris.SrC.TH[obj %like% "" & PeCr %like% "nMAE" & eff %like% "" ],aes(mu.star,sigma, label=par,color=as.factor(eff)))+geom_point()+
  facet_wrap(~obj, scales = "free")+
  scale_color_manual(values = c(cols),
                     guide = guide_legend(title="sensitive parameters \n by the \n number of affected \n objective",
                                          label.hjust =1, override.aes = list(size=5)))+
  theme(legend.position="right", 
        legend.title.align = 0.5,
        legend.key = element_rect(size = 1),
        plot.title = element_text(size=18),
        legend.box.just=0.5) + 
  geom_text(aes(label=par),hjust=1, vjust=0,show.legend = F)+
  facet_wrap(~obj, scales = "free") +
  theme(strip.text = element_text(size = 14), axis.title = element_text(size = 14), legend.position="bottom",
        legend.title=element_text(size=14), legend.text = element_text(size = 14)) + xlab("μ*") + ylab(expression(sigma))

#Calibration
#test.optim

#Check the current DS rate

Cost.optim_D.kill(x=fread(T_param)[name %in% T_param_sens.DS,]$default ,RunFile=T_RunFile,showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib=TRUE,dflt=FALSE,costfunction=NULL,
                                                                                                  obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=2005:2013,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens.DS,p=fread(T_param)[name %in% T_param_sens.DS,]$default, p.config=T_param))
Calib.T.DS2<-runDaisy(RunFile=T_RunFile, showLogFile=FALSE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib = TRUE,dflt=FALSE,costfunction=DaisyDeoptim,
                                                                                                        obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=2005:2013,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens.DS,p=fread(T_param)[name %in% T_param_sens.DS,]$default, p.config=T_param))
DS_cal<-data.table(value=Calib.T.DS2$optim$bestmem,name=T_param_sens.DS)

write.table(fread(T_param)[name %in% DS_cal$name,default:=DS_cal$value],file = T_param,row.names = F)

#Check the current Calibrated DS rate

Cost.optim_D.kill(x=fread(T_param)[name %in% T_param_sens,]$default ,RunFile=T_RunFile,showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib=TRUE,dflt=FALSE,costfunction=NULL,
                                                                                                                            obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=2005:2013,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens,p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param))
####################################

Calib.T5<-runDaisy(RunFile=T_RunFile, showLogFile=T,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib = TRUE,dflt=FALSE,costfunction=DaisyDeoptim,
                                                                                                        obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=2005:2013,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens,p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param))

names(Calib.T$optim$bestmem)<-fread(T_param)[name %in% T_param_sens,]$name



Cost.optim_D.kill(x=Calib.T$optim$bestmem ,RunFile=T_RunFile,showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib=TRUE,dflt=FALSE,costfunction=NULL,
                                                                                                                                                     obs=TYLS_obs, wdDir="F:/RDaisy_calib/", OutDir="output",interval=2005:2013,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens,p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param))
plot(Calib.T4, plot.type = "bestmemit")

###################################### Test 1000 parameterset
LHS5000<-fread("F:/RDaisy_calib/LHS5000.csv")

OUt_Calib<-PerfMeas(parampop = 5000 ,RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity=TRUE,calib = FALSE, dflt = FALSE,wdDir="F:/RDaisy_calib/",OutDir="output",interval=c(2005:2013),year=TRUE,All=TRUE,
                                                                                                                      costfunction = NULL,param_sens = T_param_sens,p.config=T_param ,obs=TYLS_obs))
setnames(OUt_Calib, make.names(names(OUt_Calib)))

OUt_Calib
sum(is.na(OUt_Calib))

OUt_Calib[is.na(R2)]

OUt_Calib[,.N,by=p.ind]

OUt_Calib[,N:=.N,by=p.ind]

OUt_Calib<-OUt_Calib[!N<73]

OUt_Calib.mean <- OUt_Calib[!is.na(R2)][!Obj=="DS"][,lapply(.SD, mean),by=.(p.ind, Obj)]

OUt_Calib.mean[,NSE:=1-NSE]

OUt_Calib.mean[,mNSE:=1-mNSE]

OUt_Calib.mean[,rNSE:=1-rNSE]

OUt_Calib.mean[,KGE:=1-KGE]

OUt_Calib.mean[,VE:=1-VE]

OUt_Calib.mean[,R2:=1-R2]

OUt_Calib.mean[,d:=1-d]

OUt_Calib.mean[,md:=1-md]

OUt_Calib.mean[,rd:=1-rd]

OUt_Calib.mean[,PBIAS:=abs(PBIAS..)]

OUt_Calib.mean[,lapply(.SD,min),.SDcols=c(colnames(OUt_Calib.mean)[c(-1,-2)])]


my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}


check.mean<-OUt_Calib.mean[,lapply(.SD,mean),.SDcol=c("nMAE", "NRMSE..","NSE","mNSE","rNSE","d","md","rd","VE","KGE","R2","PBIAS"),by=p.ind]

check.mean[which.min(NSE)]

ggpairs(OUt_Calib.mean[,lapply(.SD,mean),.SDcol=c("nMAE", "NRMSE..","NSE","mNSE","rNSE","d","md","rd","VE","KGE","R2","PBIAS"),by="p.ind"][,-"p.ind",with=F], lower = list(continuous = wrap(my_fn, method="lm")))

CorMat<-(cor(OUt_Calib.mean[,lapply(.SD,mean),.SDcol=c("nMAE", "NRMSE..","NSE","mNSE","rNSE","d","md","rd","VE","KGE","R2","PBIAS"),by=p.ind][,-"p.ind",with=F]))

inds <- which(CorMat >= 0.95 & CorMat < 1, arr.ind=TRUE)
rnames <- rownames(CorMat)[inds[,1]]
cnames <- colnames(CorMat)[inds[,2]]
value <- CorMat[which(CorMat >= 0.95 & CorMat < 1, arr.ind=TRUE)]
CorMat.s<-data.table(cbind(rnames,cnames,value))
CorMat.s[duplicated(value)]
unique(CorMat.s[,c(rnames,cnames)])
