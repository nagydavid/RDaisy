#Reading modelled SWT output and calculating NSE from the package HydroGOF
read.optim.0_30.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 1992 and 1993 drainage year. 
  # year = if is TRUE than user gets the all output objectives of the years, if it FALSE, it gives the mean of all calculated annual objective.
  # calib = if it is TRUE, read output files with PID as index, if it falls it reads from index file.
  
  if (calib == T){#If calib is TRUE, then it reads output file which named after the running core(sys.getpid),
            
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_swct.dlf", sep=""))
  } else {     #if false is named after the current rownumber of the sensitivity matrix
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_swct.dlf", sep=""))
  }
  
  #debugging section, if fread drop an error, it creates a high hydroGOF output
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    simOut[,sim0_30:=rowMeans(simOut[,5:9])]
  
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simOut[,.SD, .SDcols=c( "date", "sim0_30")],
                         obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"WATER_0_30_CM_VOL_PROZ")], all.y  = T,by="date")
    
    #Due to our observation data is in percentage water content and daisy provide it with decimal, we convert our observation to decimals
    SimObsSWC[, WATER_0_30_CM_VOL_PROZ:=WATER_0_30_CM_VOL_PROZ/100]
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))] 
    #Calculating HydroGOF matrix
    HydGOF1 <- gof(sim = SimObsSWC1$sim0_30, obs = SimObsSWC1$WATER_0_30_CM_VOL_PROZ,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsSWC2$sim0_30, obs = SimObsSWC2$WATER_0_30_CM_VOL_PROZ,  norm = "maxmin",digits = 4)

    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating NSE from the package HydroGOF
read.optim.30_60.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_swct.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_swct.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simOut[,sim30_60:=rowMeans(simOut[,10:12])]
    
    SimObsSWC <- merge(simOut[,.SD, .SDcols=c( "date", "sim30_60")],
                       obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"WATER_30_60_CM_VOL_PROZ")], all.y = T,by="date")
    
    SimObsSWC[, WATER_30_60_CM_VOL_PROZ:=WATER_30_60_CM_VOL_PROZ/100]
    
    SimObsSWC <- na.omit(SimObsSWC)
    
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]  
    
    HydGOF1 <- gof(sim = SimObsSWC1$sim30_60, obs = SimObsSWC1$WATER_30_60_CM_VOL_PROZ,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsSWC2$sim30_60, obs = SimObsSWC2$WATER_30_60_CM_VOL_PROZ,  norm = "maxmin",digits = 4)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }
  }


#Reading modelled SWT output and calculating NSE from the package HydroGOF
read.optim.60_90.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_swct.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_swct.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simOut[,sim60_90:=rowMeans(simOut[,13:15])]
    
    
    SimObsSWC <- merge(simOut[,.SD, .SDcols=c( "date", "sim60_90")],
                       obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"WATER_60_90_CM_VOL_PROZ")], all.y = T,by="date")
    
    SimObsSWC[, WATER_60_90_CM_VOL_PROZ:=WATER_60_90_CM_VOL_PROZ/100]
    
    SimObsSWC <- na.omit(SimObsSWC)
    
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))] 
    
    HydGOF1 <- gof(sim = SimObsSWC1$sim60_90, obs = SimObsSWC1$WATER_60_90_CM_VOL_PROZ,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsSWC2$sim60_90, obs = SimObsSWC2$WATER_60_90_CM_VOL_PROZ,  norm = "maxmin",digits = 4)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}


#Nitrate function
#Reading modelled N output and calculating NSE from the package HydroGOF
read.optim.0_30.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_soil_NO3-N.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_soil_NO3-N.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simOut[,sim0_30:=rowMeans(simOut[,5:9])]
    
    
    SimObsN <- merge(simOut[,.SD, .SDcols=c( "date", "sim0_30")],
                     obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"NITRAT_0_30_CM_KG_HA")], all.y = T,by="date")
    
    SimObsN[, NITRATE_N_0_30_CM_g_cm2:=NITRAT_0_30_CM_KG_HA*0.226*10^-5]#http://newsroom.unl.edu/announce/beef/5891/33379
    SimObsN[, NITRATE_N_0_30_CM_g_cm3:=NITRATE_N_0_30_CM_g_cm2*30]
    
    SimObsN <- na.omit(SimObsN)
    
    SimObsN1 <- SimObsN[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsN2 <- SimObsN[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]  
    
    HydGOF1 <- gof(sim = SimObsN1$sim0_30, obs = SimObsN1$NITRATE_N_0_30_CM_g_cm3,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsN2$sim0_30, obs = SimObsN2$NITRATE_N_0_30_CM_g_cm3,  norm = "maxmin",digits = 4)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled drain output and calculating NSE from the package HydroGOF
read.optim.30_60.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_soil_NO3-N.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_soil_NO3-N.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simOut[,sim30_60:=rowMeans(simOut[,10:12])]
    
    
    SimObsN <- merge(simOut[,.SD, .SDcols=c( "date", "sim30_60")],
                     obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"NITRAT_30_60_CM_KG_HA")], all.y = T,by="date")
    
    SimObsN[, NITRATE_N_30_60_CM_g_cm2:=NITRAT_30_60_CM_KG_HA*0.226*10^-5]#http://newsroom.unl.edu/announce/beef/5891/33379
    SimObsN[, NITRATE_N_30_60_CM_g_cm3:=NITRATE_N_30_60_CM_g_cm2*30]
    
    SimObsN <- na.omit(SimObsN)
    
    SimObsN1 <- SimObsN[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsN2 <- SimObsN[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]
    
    HydGOF1 <- gof(sim = SimObsN1$sim30_60, obs = SimObsN1$NITRATE_N_30_60_CM_g_cm3,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsN2$sim30_60, obs = SimObsN2$NITRATE_N_30_60_CM_g_cm3,  norm = "maxmin",digits = 4)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}


#Reading modelled drain output and calculating NSE from the package HydroGOF
read.optim.60_90.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_soil_NO3-N.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_soil_NO3-N.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 2432){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simOut[,sim60_90:=rowMeans(simOut[,13:15])]
    
    
    SimObsN <- merge(simOut[,.SD, .SDcols=c( "date", "sim60_90")],
                     obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"NITRAT_60_90_CM_KG_HA")], all.y = T,by="date")
    
    SimObsN[, NITRATE_N_60_90_CM_g_cm2:=NITRAT_60_90_CM_KG_HA*0.226*10^-5]#http://newsroom.unl.edu/announce/beef/5891/33379
    SimObsN[, NITRATE_N_60_90_CM_g_cm3:=NITRATE_N_60_90_CM_g_cm2*30]
    
    SimObsN <- na.omit(SimObsN)
    
    SimObsN1 <- SimObsN[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsN2 <- SimObsN[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]
    
    HydGOF1 <- gof(sim = SimObsN1$sim60_90, obs = SimObsN1$NITRATE_N_60_90_CM_g_cm3,  norm = "maxmin",digits = 4)
    HydGOF2 <- gof(sim = SimObsN2$sim60_90, obs = SimObsN2$NITRATE_N_60_90_CM_g_cm3,  norm = "maxmin",digits = 4)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}


read.optim.Yield<- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_harvest.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_harvest.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else if (nrow(simOut)< 8){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,NSE:=1000]
    return(x)
  } else {
    simOut=simOut[-1]
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(paste(day, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
      simOut$simAGB=as.numeric(simOut$stem_DM)+as.numeric(simOut$leaf_DM)+as.numeric(simOut$sorg_DM)
    simOut$simgrain=simOut$simAGB*simOut$HI

    
    SimObsyield <- merge(simOut[,.SD, .SDcols=c( "date", "simgrain")],
                     obs[PLOT=="plot2",.SD, .SDcols=c("date" ,"HARVHAND_DR_STORG_BE_KG_HA")], all.y = T,by="date")
    
    SimObsyield[, HARVHAND_DR_STORG_BE_T_HA:=HARVHAND_DR_STORG_BE_KG_HA/1000]
    
    SimObsyield <- na.omit(SimObsyield)
    
    SimObsyield1 <- SimObsyield[date %between% c(dmy(paste("01-01",interval[1])),dmy(paste("31-12",interval[1])))]
    SimObsyield2 <- SimObsyield[date %between% c(dmy(paste("01-01",interval[2])),dmy(paste("31-12",interval[2])))]
    
    
    HydGOF <- data.table(t(gof(sim = SimObsyield$simgrain, obs = SimObsyield$HARVHAND_DR_STORG_BE_T_HA,  norm = "maxmin",digits = 4)))
    HydGOF[,year:=interval[1]]
    # HydGOF1 <- gof(sim = SimObsyield1$simgrain, obs = SimObsyield1$HARVHAND_DR_STORG_BE_T_HA,  norm = "maxmin",digits = 4)
    # HydGOF2 <- gof(sim = SimObsyield2$simgrain, obs = SimObsyield2$HARVHAND_DR_STORG_BE_T_HA,  norm = "maxmin",digits = 4)
    # 
    # 
    # HydGOF1 <- data.table(t(HydGOF1))
    # HydGOF1[,year:=interval[1]]
    # HydGOF2 <- data.table(t(HydGOF2))
    # HydGOF2[,year:=interval[2]]
    # 
    # HydGOF <- rbind(HydGOF1,HydGOF2)
    GOFMean <- HydGOF
    # GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    # GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

read.optim <- function(ind, obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  swct0_30<-read.optim.0_30.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct30_60<-read.optim.30_60.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct60_90<-read.optim.60_90.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  N0_30<-read.optim.0_30.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  N30_60<-read.optim.30_60.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  N60_90<-read.optim.60_90.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  Yield <- read.optim.Yield(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
    
  NSE<- (1-swct0_30$NSE)+(1-swct30_60$NSE)+(1-swct60_90$NSE)+(1-N0_30$NSE)+(1-N30_60$NSE)+(1-N60_90$NSE)+(1-Yield$NSE)
  
  return(NSE)
}

Cost.optim <- function(RunFile,showLogFile,PathToDaisy,ctrldaisy){
  
  ctrldaisy <- do.call(Daisy.control, as.list(ctrldaisy))
  p.config <- ctrldaisy$p.config
    ctrldaisy$ind=Sys.getpid()
  f.cost(RunFile = RunFile,showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy=Daisy.control(sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,dflt=ctrldaisy$dflt,costfunction=ctrldaisy$costfunction,obs=ctrldaisy$obs,wdDir=ctrldaisy$wdDir,OutDir = ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=ctrldaisy$ind,param_sens=ctrldaisy$param_sens,p=ctrldaisy$p, p.config=ctrldaisy$p.config))
  
  return(read.optim(ind = ctrldaisy$ind,obs =  ctrldaisy$obs,wdDir = ctrldaisy$wdDir,OutDir =  ctrldaisy$OutDir,interval =  ctrldaisy$interval,year = ctrldaisy$year,sensitivity = ctrldaisy$sensitivity,calib = ctrldaisy$calib,All = ctrldaisy$All))
}



DaisyDeoptim<-function(RunFile,showLogFile,PathToDaisy,ctrldaisy){

  set.seed(1)
  p.config <- ctrldaisy$p.config
  param_matrix<-fread(p.config)
  
  #DEoptim contol parameters
  
  Base.Functions<-c("CheckParameters","runDaisy","f.update","updateParameters","f.cost")
  My.Packages <- c("data.table", "hydroGOF", "lubridate","RDaisy")
  My.Functions <- c("read.optim.0_30.swct","read.optim.30_60.swct","read.optim.60_90.swct","read.optim.0_30.N","read.optim.30_60.N","read.optim.60_90.N","read.optim.Yield","read.optim","Cost.optim_D","DaisyMorris")

  #DeOptim calibration
  #Paralell cluster setup
  
  lowR <- param_matrix[name %in% ctrldaisy$param_sens, ]$min
  uppR <- param_matrix[name %in% ctrldaisy$param_sens, ]$max

  maxIT <- 10
  
  #ctrldaisy is not transfered to Cost.optim
  
  Calib.Sens <- DEoptim::DEoptim(fn=Cost.optim_D,
                                  lower = lowR,
                                  upper = uppR,
                                  DEoptim::DEoptim.control(itermax=maxIT,parallelType = 1,packages = My.Packages, parVar = c(My.Functions), NP=detectCores()),
                                  RunFile,showLogFile,PathToDaisy,ctrldaisy
                                  )
}

Cost.optim_D <- function(x,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  print(x)
  
  ctrldaisy <- do.call(Daisy.control, as.list(ctrldaisy))
  p.config <- ctrldaisy$p.config
  ctrldaisy$p=x
  ctrldaisy$ind=Sys.getpid()
  f.cost(RunFile = RunFile,showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy=Daisy.control(sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,dflt=ctrldaisy$dflt,costfunction=ctrldaisy$costfunction,obs=ctrldaisy$obs,wdDir=ctrldaisy$wdDir,OutDir = ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=ctrldaisy$ind,param_sens=ctrldaisy$param_sens,p=ctrldaisy$p, p.config=ctrldaisy$p.config))
  
  return(read.optim(ctrldaisy$ind, ctrldaisy$obs,ctrldaisy$wdDir, ctrldaisy$OutDir, ctrldaisy$interval,ctrldaisy$year,ctrldaisy$calib))
}