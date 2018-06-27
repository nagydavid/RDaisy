# devtools::install_github(repo="jeroenpullens/RDaisy",subdir = "RDaisy")

library(hydroGOF)
library(data.table)
library(sensitivity)
library(lubridate)
library(RDaisy)
library(foreach)
library(doParallel)
library(ggplot2)


setwd("C:/test_RDAISY")

#copytestdata("C:/test_RDAISY/",package_folder=NULL)

T_RunFile<-"C:/test_RDAISY/ZALF_Muncheberg.dai" # RunFile of daisy
T_path<-"C:/Program Files/Daisy 5.59/bin/daisy.exe" # path of Daisy executable
T_param<-"C:/test_RDAISY/ParamsToCalibrate.txt" #parameter matrix with min max and file names
T_Morris <- "C:/test_RDAISY/R_Daisy_package/Final/Morris_f.R"

#observation
T_obs<-fread("C:/Users/au595572/Documents/_Data/Data_RDaisy/csv/Soil_H2O_NO3_NH4.csv") 
T_obs[,date:=dmy(DATE_)] #convert Date_ to date format

T_obs_yield <- fread("C:/Users/au595572/Documents/_Data/Data_RDaisy/csv/Crop_ontogenesis.csv")
T_obs_yield[,date:=dmy(DATE_)] #convert Date_ to date format
T_obs_yield <- T_obs_yield[,-"V1"]


T_obs_all <- merge(x=T_obs,y=T_obs_yield,all = T)


setwd("C:/test_RDAISY/")




#Test run of daisy with default parameters
# runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=FALSE,DEoptim=FALSE, dflt = TRUE,costfunction=NULL,
         # wdDir="C:/test_RDAISY",OutDir="output",interval=c(1993:1995),year=FALSE,All=FALSE,param_sens = NULL)

runDaisy(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy =  Daisy.control(sensitivity =FALSE,calib=FALSE, dflt = TRUE,costfunction=NULL,wdDir="C:/test_RDAISY",OutDir="output",interval=c(1993:1995),year=FALSE,All=FALSE,param_sens = NULL,p.config=T_param ,obs=T_obs_all))

#test run of the Fcost function, custom parameter set ,can be tested
# f.cost(p=fread(T_param)$default, p.config=T_param, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=TRUE,DEoptim=FALSE, dflt = FALSE,costfunction = NULL,
       # obs=T_obs, wdDir="C:/test_RDAISY", OutDir="output",interval=1993:1995,year=TRUE,All=TRUE,ind=Sys.getpid(),param_sens = NULL)

f.cost(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity = T,calib = FALSE, dflt = FALSE,costfunction = NULL,
       obs=T_obs_all, wdDir="C:/test_RDAISY", OutDir="output",interval=1993:1995,year=TRUE,All=TRUE,ind=Sys.getpid(),param_sens = NULL,p=fread(T_param)$default, p.config=T_param))


source("Morris_f.R")
source("DEoptim_f.R")
# run of daisy with Morris function
 # Morris.SrC<-runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=TRUE,DEoptim=FALSE, dflt = FALSE,
 #          wdDir="C:/test_RDAISY",OutDir="output",interval=c(1993:1995),year=TRUE,All=TRUE, costfunction = DaisyMorris,param_sens = NULL)

Morris.SrC<-runDaisy(RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity=TRUE,calib = FALSE, dflt = FALSE,wdDir="C:/test_RDAISY",OutDir="output",interval=c(1993:1995),year=TRUE,All=TRUE, costfunction = DaisyMorris,param_sens = NULL,p.config=T_param ,obs=T_obs_all))


Morris.plot.NSE <- ggplot(Morris.SrC,aes(mu.star,sigma, label=par))+geom_point()+
  geom_text(aes(label=par),hjust=0, vjust=0)+facet_wrap(~obj, scales = "free")

#Sensitivity treshold

T_treshold<-fread("C:/test_RDAISY/S_treshold.csv")

Morris.SrC[,r:=sqrt(sigma^2+mu.star^2)]

Morris.SrC<-merge(Morris.SrC,T_treshold[,.SD,.SDcols=c("obj","r.tH")],by="obj")

Morris.SrC[,sens:=ifelse(r>r.tH,1,0)]

T_param_sens <- unique(Morris.SrC[sens==1,par])

#Calibration
#test.optim

Cost.optim(RunFile=T_RunFile,showLogFile=TRUE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib=TRUE,dflt=FALSE,costfunction=NULL,
           obs=T_obs_all, wdDir="C:/test_RDAISY", OutDir="output",interval=1993:1995,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens,p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param))


Calib.T<-runDaisy(RunFile=T_RunFile, showLogFile=FALSE,PathToDaisy=T_path,ctrldaisy = Daisy.control(sensitivity =FALSE,calib=TRUE,dflt=FALSE,costfunction=DaisyDeoptim,
                                                                                               obs=T_obs_all, wdDir="C:/test_RDAISY", OutDir="output",interval=1993:1995,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens,p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param))
