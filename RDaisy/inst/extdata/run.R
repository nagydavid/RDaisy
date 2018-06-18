devtools::install_github(repo="jeroenpullens/RDaisy",subdir = "RDaisy")

library(hydroGOF)
library(data.table)
library(sensitivity)
library(lubridate)
library(RDaisy)
library(foreach)
library(doParallel)
library(ggplot2)


setwd("F:/RDaisy_stuff")

#copytestdata("F:/RDaisy_stuff/",package_folder=NULL)

T_RunFile<-"F:/RDaisy_stuff/ZALF_Muncheberg.dai" # RunFile of daisy
T_path<-"C:/Program Files/Daisy 5.59/bin/daisy.exe" # path of Daisy executable
T_param<-"F:/RDaisy_stuff/ParamsToCalibrate.txt" #parameter matrix with min max and file names
T_Morris <- "F:/RDaisy_stuff/R_Daisy_package/Final/Morris_f.R"

#observation
T_obs<-fread("F:/RDaisy_stuff/R_Daisy_package/csv/Soil_H2O_NO3_NH4.csv") 
T_obs[,date:=dmy(DATE_)] #convert Date_ to date format

setwd("F:/RDaisy_stuff/")

#Test run of daisy with default parameters
runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=FALSE,DEoptim=FALSE, dflt = TRUE,costfunction=NULL,
         wdDir="F:/RDaisy_stuff",OutDir="output",interval=c(1993:1995),year=FALSE,All=FALSE,param_sens = NULL)

#test run of the Fcost function, custom parameter set ,can be tested
f.cost(p=fread(T_param)$default, p.config=T_param, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=TRUE,DEoptim=FALSE, dflt = FALSE,costfunction = NULL,
       obs=T_obs, wdDir="F:/RDaisy_stuff", OutDir="output",interval=1993:1995,year=TRUE,All=TRUE,ind=Sys.getpid(),param_sens = NULL)

# run of daisy with Morris function
Morris.SrC<-runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=TRUE,PathToDaisy=T_path,Morris=TRUE,DEoptim=FALSE, dflt = FALSE,
         wdDir="F:/RDaisy_stuff",OutDir="output",interval=c(1993:1995),year=TRUE,All=TRUE, costfunction = DaisyMorris,param_sens = NULL)

Morris.plot.NSE <- ggplot(Morris.SrC,aes(mu.star,sigma, label=par))+geom_point()+
  geom_text(aes(label=par),hjust=0, vjust=0)+facet_wrap(~obj, scales = "free")

#Sensitivity treshold

T_treshold<-fread("F:/RDaisy_stuff/R_Daisy_package/csv/S_treshold.csv")

Morris.SrC[,r:=sqrt(sigma^2+mu.star^2)]

Morris.SrC<-merge(Morris.SrC,T_treshold[,.SD,.SDcols=c("obj","r.tH")],by="obj")

Morris.SrC[,sens:=ifelse(r>r.tH,1,0)]

T_param_sens <- unique(Morris.SrC[sens==1,par])

#Calibration
#test.optim

Cost.optim(p=fread(T_param)[name %in% T_param_sens,]$default, p.config=T_param, RunFile=T_RunFile,showLogFile=TRUE,PathToDaisy=T_path,Morris=FALSE,DEoptim=TRUE,dflt=FALSE,costfunction=NULL,
           obs=T_obs, wdDir="F:/RDaisy_stuff", OutDir="output",interval=1993:1995,year=FALSE,All=FALSE,ind=Sys.getpid(),param_sens = T_param_sens)



Calib.T<-runDaisy(p.config=T_param ,obs=T_obs, RunFile=T_RunFile, showLogFile=FALSE,PathToDaisy=T_path,Morris=FALSE,DEoptim=TRUE, dflt = FALSE,
                     wdDir="F:/RDaisy_stuff",OutDir="output",interval=c(1993:1995),year=FALSE,All=FALSE, costfunction = DaisyDeoptim,param_sens = T_param_sens)
