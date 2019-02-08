#min-max function
maxmin <- function(X){
  
  return(max(X,na.rm =T)-min(X,na.rm =T))
}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.25.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....25.5"),by="date"]
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....25.5" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S25")], all.y  = T,by="date")
    

    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....25.5, obs = SimObsSWC1$S25,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....25.5, obs = SimObsSWC2$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....25.5, obs = SimObsSWC3$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....25.5, obs = SimObsSWC4$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....25.5, obs = SimObsSWC5$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....25.5, obs = SimObsSWC6$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....25.5, obs = SimObsSWC7$S25,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....25.5, obs = SimObsSWC8$S25,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.60.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....62.5"),by="date"]
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....62.5" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S60")], all.y  = T,by="date")
    
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....62.5, obs = SimObsSWC1$S60,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....62.5, obs = SimObsSWC2$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....62.5, obs = SimObsSWC3$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....62.5, obs = SimObsSWC4$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....62.5, obs = SimObsSWC5$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....62.5, obs = SimObsSWC6$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....62.5, obs = SimObsSWC7$S60,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....62.5, obs = SimObsSWC8$S60,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.90.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....92.5"),by="date"]
    
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....92.5" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S90")], all.y  = T,by="date")
    
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....92.5, obs = SimObsSWC1$S90,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....92.5, obs = SimObsSWC2$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....92.5, obs = SimObsSWC3$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....92.5, obs = SimObsSWC4$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....92.5, obs = SimObsSWC5$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....92.5, obs = SimObsSWC6$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....92.5, obs = SimObsSWC7$S90,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....92.5, obs = SimObsSWC8$S90,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.110.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....115"),by="date"]
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....115" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S110")], all.y  = T,by="date")
    
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....115, obs = SimObsSWC1$S110,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....115, obs = SimObsSWC2$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....115, obs = SimObsSWC3$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....115, obs = SimObsSWC4$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....115, obs = SimObsSWC5$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....115, obs = SimObsSWC6$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....115, obs = SimObsSWC7$S110,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....115, obs = SimObsSWC8$S110,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.190.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....195"),by="date"]
    
    
    #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....195" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S190")], all.y  = T,by="date")
    
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....195, obs = SimObsSWC1$S190,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....195, obs = SimObsSWC2$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....195, obs = SimObsSWC3$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....195, obs = SimObsSWC4$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....195, obs = SimObsSWC5$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....195, obs = SimObsSWC6$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....195, obs = SimObsSWC7$S190,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....195, obs = SimObsSWC8$S190,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled SWT output and calculating nMAE from the package HydroGOF
read.optim.210.swct <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  # ind = index file, to decide which output to read
  # obs = observation data with date and measurement, same dimension of the simulation
  # wdDIR = working environment path
  # OutDir = output path
  # interval = is the interval of the calibration period, in our case the from 2005 and 2013 drainage year. 
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
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file is lower than the expected rowline, then create a high HydroGOF output
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    #making daisy output file readable for R function
    setnames(simOut, make.names(names(simOut)))
    
    #create date from daisy file for the simulation
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    
    simOut<-simOut[!duplicated(simOut$date)]
    
    simTDR <- simOut[,lapply(.SD,mean), .SDcols=c("Theta....215"),by="date"]
    
        #This part of the function has to be user defined. Here we provide an approach to create an output objective.
    #We take the mean of the Soil Water Content from 0 to 30 cm in order to match it with our observed data.
    
    #here we merging the input and the output in order to further process
    SimObsSWC <- merge(simTDR[,.SD, .SDcols=c( "date","Theta....215" )],
                       obs$tdr.obs[,.SD, .SDcols=c("date" ,"S210")], all.y  = T,by="date")
    
    
    #omitting all NA to not bias our HydroGOF output
    SimObsSWC <- na.omit(SimObsSWC)
    
    #the objective function are set up to be able to see the annual difference of our objective
    SimObsSWC1 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]#2005-2006
    SimObsSWC2 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]#2006-2007
    SimObsSWC3 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]#2007-2008
    SimObsSWC4 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]#2008-2009
    SimObsSWC5 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]#2009-2010 
    SimObsSWC6 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]#2010-2011 
    SimObsSWC7 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]#2011-2012 
    SimObsSWC8 <- SimObsSWC[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))]#2012-2013
    
    
    
    #Calculating HydroGOF matrix
    HydGOF1 <- gof.default.D(sim = SimObsSWC1$Theta....215, obs = SimObsSWC1$S210,  norm = "maxmin",digits = 4, j = 1) 
    HydGOF2 <- gof.default.D(sim = SimObsSWC2$Theta....215, obs = SimObsSWC2$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsSWC3$Theta....215, obs = SimObsSWC3$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsSWC4$Theta....215, obs = SimObsSWC4$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsSWC5$Theta....215, obs = SimObsSWC5$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsSWC6$Theta....215, obs = SimObsSWC6$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsSWC7$Theta....215, obs = SimObsSWC7$S210,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsSWC8$Theta....215, obs = SimObsSWC8$S210,  norm = "maxmin",digits = 4, j = 1)
    
    
    #transponating HydroGOF matrix for further use
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,year:=interval[8]]
    
    #rbinding HydroGOF
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    #mean of all annual objectives
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Nitrate function
#Reading modelled N output and calculating nMAE from the package HydroGOF
read.optim.SC1.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_soil_NO3-N.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_soil_NO3-N.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simSUC <- simOut[,lapply(.SD,mean), .SDcols=c("C....100"),by="date"]
    
    
    SimObsN <- merge(simSUC[,.SD, .SDcols=c( "date", "C....100")],
                     obs$N_content, all.y = T,by="date")
    
    SimObsN <- na.omit(SimObsN)
    
    SimObsN1 <- SimObsN[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsN2 <- SimObsN[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]
    SimObsN3 <- SimObsN[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]
    SimObsN4 <- SimObsN[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]  
    SimObsN5 <- SimObsN[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]
    SimObsN6 <- SimObsN[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]
    SimObsN7 <- SimObsN[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]
    SimObsN8 <- SimObsN[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))] 
    
    HydGOF1 <- gof.default.D(sim = SimObsN1$C....100, obs = SimObsN1$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF2 <- gof.default.D(sim = SimObsN2$C....100, obs = SimObsN2$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsN3$C....100, obs = SimObsN3$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsN4$C....100, obs = SimObsN4$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsN5$C....100, obs = SimObsN5$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsN6$C....100, obs = SimObsN6$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsN7$C....100, obs = SimObsN7$AVE1,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsN8$C....100, obs = SimObsN8$AVE1,  norm = "maxmin",digits = 4, j = 1)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,nMAE:=MAE/mean(SimObsN1$AVE1)]
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,nMAE:=MAE/mean(SimObsN2$AVE1)]
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,nMAE:=MAE/mean(SimObsN3$AVE1)]
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,nMAE:=MAE/mean(SimObsN4$AVE1)]
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,nMAE:=MAE/mean(SimObsN5$AVE1)]
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,nMAE:=MAE/mean(SimObsN6$AVE1)]
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,nMAE:=MAE/mean(SimObsN7$AVE1)]
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,nMAE:=MAE/mean(SimObsN8$AVE1)]
    HydGOF8[,year:=interval[8]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}

#Reading modelled drain output and calculating nMAE from the package HydroGOF
read.optim.SC2.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_soil_NO3-N.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_soil_NO3-N.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 78815){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    setnames(simOut, make.names(names(simOut)))
    
    simOut[, date := dmy(paste(paste(mday, month, year,sep="-")))]
    simOut<-simOut[!duplicated(simOut$date)]
    
    simSUC <- simOut[,lapply(.SD,mean), .SDcols=c("C....200"),by="date"]
    
    
    SimObsN <- merge(simSUC[,.SD, .SDcols=c( "date", "C....200")],
                     obs$N_content, all.y = T,by="date")
    
    SimObsN <- na.omit(SimObsN)
    
    SimObsN1 <- SimObsN[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("01-04",interval[2])))]
    SimObsN2 <- SimObsN[date %between% c(dmy(paste("31-03",interval[2])),dmy(paste("01-04",interval[3])))]
    SimObsN3 <- SimObsN[date %between% c(dmy(paste("31-03",interval[3])),dmy(paste("01-04",interval[4])))]
    SimObsN4 <- SimObsN[date %between% c(dmy(paste("31-03",interval[4])),dmy(paste("01-04",interval[5])))]  
    SimObsN5 <- SimObsN[date %between% c(dmy(paste("31-03",interval[5])),dmy(paste("01-04",interval[6])))]
    SimObsN6 <- SimObsN[date %between% c(dmy(paste("31-03",interval[6])),dmy(paste("01-04",interval[7])))]
    SimObsN7 <- SimObsN[date %between% c(dmy(paste("31-03",interval[7])),dmy(paste("01-04",interval[8])))]
    SimObsN8 <- SimObsN[date %between% c(dmy(paste("31-03",interval[8])),dmy(paste("01-04",interval[9])))] 
    
    HydGOF1 <- gof.default.D(sim = SimObsN1$C....200, obs = SimObsN1$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF2 <- gof.default.D(sim = SimObsN2$C....200, obs = SimObsN2$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF3 <- gof.default.D(sim = SimObsN3$C....200, obs = SimObsN3$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF4 <- gof.default.D(sim = SimObsN4$C....200, obs = SimObsN4$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF5 <- gof.default.D(sim = SimObsN5$C....200, obs = SimObsN5$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF6 <- gof.default.D(sim = SimObsN6$C....200, obs = SimObsN6$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF7 <- gof.default.D(sim = SimObsN7$C....200, obs = SimObsN7$AVE2,  norm = "maxmin",digits = 4, j = 1)
    HydGOF8 <- gof.default.D(sim = SimObsN8$C....200, obs = SimObsN8$AVE2,  norm = "maxmin",digits = 4, j = 1)
    
    
    HydGOF1 <- data.table(t(HydGOF1))
    HydGOF1[,nMAE:=MAE/mean(SimObsN1$AVE2)]
    HydGOF1[,year:=interval[1]]
    HydGOF2 <- data.table(t(HydGOF2))
    HydGOF2[,nMAE:=MAE/mean(SimObsN2$AVE2)]
    HydGOF2[,year:=interval[2]]
    HydGOF3 <- data.table(t(HydGOF3))
    HydGOF3[,nMAE:=MAE/mean(SimObsN3$AVE2)]
    HydGOF3[,year:=interval[3]]
    HydGOF4 <- data.table(t(HydGOF4))
    HydGOF4[,nMAE:=MAE/mean(SimObsN4$AVE2)]
    HydGOF4[,year:=interval[4]]
    HydGOF5 <- data.table(t(HydGOF5))
    HydGOF5[,nMAE:=MAE/mean(SimObsN5$AVE2)]
    HydGOF5[,year:=interval[5]]
    HydGOF6 <- data.table(t(HydGOF6))
    HydGOF6[,nMAE:=MAE/mean(SimObsN6$AVE2)]
    HydGOF6[,year:=interval[6]]
    HydGOF7 <- data.table(t(HydGOF7))
    HydGOF7[,nMAE:=MAE/mean(SimObsN7$AVE2)]
    HydGOF7[,year:=interval[7]]
    HydGOF8 <- data.table(t(HydGOF8))
    HydGOF8[,nMAE:=MAE/mean(SimObsN8$AVE2)]
    HydGOF8[,year:=interval[8]]
    
    HydGOF <- rbind(HydGOF1,HydGOF2,HydGOF3,HydGOF4,HydGOF5,HydGOF6,HydGOF7,HydGOF8)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }}


#Reading modelled yield output and calculating nMAE from the package HydroGOF
read.optim.DS<- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_crop.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_crop.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 3285){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(mday, month, year,sep="-"))]
    

    BBCHobs<-obs$BBCH[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("07-08",interval[9])))]
    setkey(BBCHobs,DS_t)

    
    simOut[date %between% c(min(BBCHobs[Crop=="MA1"]$date+1),max(BBCHobs[Crop=="MA1"]$date)),Crop:="MA1"]
    simOut[date %between% c(min(BBCHobs[Crop=="WW1"]$date+1),max(BBCHobs[Crop=="WW1"]$date)),Crop:="WW1"] 
    simOut[date %between% c(min(BBCHobs[Crop=="WR1"]$date+1),max(BBCHobs[Crop=="WR1"]$date)),Crop:="WR1"]
    simOut[date %between% c(min(BBCHobs[Crop=="PO1"]$date+1),max(BBCHobs[Crop=="PO1"]$date)),Crop:="PO1"]
    simOut[date %between% c(min(BBCHobs[Crop=="SB1"]$date+1),max(BBCHobs[Crop=="SB1"]$date)),Crop:="SB1"]
    simOut[date %between% c(min(BBCHobs[Crop=="SB2"]$date+1),max(BBCHobs[Crop=="SB2"]$date)),Crop:="SB2"]
    simOut[date %between% c(min(BBCHobs[Crop=="SB3"]$date+1),max(BBCHobs[Crop=="SB3"]$date)),Crop:="SB3"]
    simOut[date %between% c(min(BBCHobs[Crop=="SB4"]$date+1),max(BBCHobs[Crop=="SB4"]$date)),Crop:="SB4"]

    
    #Maize 1    
    SimBMObs_MA1 <- simOut[Crop=="MA1",.(Crop,date,DS)]
    
    DS_t<-c(0,1,1.8)
    SimBMObs_MA1[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="MA1" & DS_t==-1,]$date)]
    
    SimBMObs_MA1<-rbind(
      SimBMObs_MA1[which(DS >= DS_t[1])][1],
      SimBMObs_MA1[which(DS >= DS_t[2])][1],
      SimBMObs_MA1[which.closest(SimBMObs_MA1$DS,DS_t[3])])
    
    SimBMObs_MA1$DS_t<-DS_t
    setkey(SimBMObs_MA1,DS_t)
    
    SimBMObs_MA1 <- merge(SimBMObs_MA1,BBCHobs[Crop=="MA1" & DS_t %in% c(0,1,1.8) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_MA1[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_MA1[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Winter Wheat 
    SimBMObs_WW1 <- simOut[Crop=="WW1",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_WW1[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="WW1" & DS_t==-1,]$date)]
    
    SimBMObs_WW1<-rbind(
      SimBMObs_WW1[which(DS >= DS_t[1])][1],
      SimBMObs_WW1[which(DS >= DS_t[2])][1],
      SimBMObs_WW1[which.closest(SimBMObs_WW1$DS,DS_t[3])])
    
    SimBMObs_WW1$DS_t<-DS_t
    setkey(SimBMObs_WW1,DS_t)
    
    SimBMObs_WW1 <- merge(SimBMObs_WW1,BBCHobs[Crop=="WW1" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_WW1[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_WW1[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Winter Rape 
    SimBMObs_WR1 <- simOut[Crop=="WR1",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_WR1[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="WR1" & DS_t==-1,]$date)]
    
    SimBMObs_WR1<-rbind(
      SimBMObs_WR1[which(DS >= DS_t[1])][1],
      SimBMObs_WR1[which(DS >= DS_t[2])][1],
      SimBMObs_WR1[which.closest(SimBMObs_WR1$DS,DS_t[3])])
    
    SimBMObs_WR1$DS_t<-DS_t
    setkey(SimBMObs_WR1,DS_t)
    
    SimBMObs_WR1 <- merge(SimBMObs_WR1,BBCHobs[Crop=="WR1" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_WR1[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_WR1[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Potato 1    
    SimBMObs_PO1 <- simOut[Crop=="PO1",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_PO1[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="PO1" & DS_t==-1,]$date)]
    
    SimBMObs_PO1<-rbind(
      SimBMObs_PO1[which(DS >= DS_t[1])][1],
      SimBMObs_PO1[which(DS >= DS_t[2])][1],
      SimBMObs_PO1[which.closest(SimBMObs_PO1$DS,DS_t[3])])
    
    SimBMObs_PO1$DS_t<-DS_t
    setkey(SimBMObs_PO1,DS_t)
    
    SimBMObs_PO1 <- merge(SimBMObs_PO1,BBCHobs[Crop=="PO1" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_PO1[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_PO1[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Spring Barley 1    
    SimBMObs_SB1 <- simOut[Crop=="SB1",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_SB1[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="SB1" & DS_t==-1,]$date)]
    
    SimBMObs_SB1<-rbind(
      SimBMObs_SB1[which(DS >= DS_t[1])][1],
      SimBMObs_SB1[which(DS >= DS_t[2])][1],
      SimBMObs_SB1[which.closest(SimBMObs_SB1$DS,DS_t[3])])
    
    SimBMObs_SB1$DS_t<-DS_t
    setkey(SimBMObs_SB1,DS_t)
    
    SimBMObs_SB1 <- merge(SimBMObs_SB1,BBCHobs[Crop=="SB1" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_SB1[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_SB1[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Spring Barley 2    
    SimBMObs_SB2 <- simOut[Crop=="SB2",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_SB2[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="SB2" & DS_t==-1,]$date)]
    
    SimBMObs_SB2<-rbind(
      SimBMObs_SB2[which(DS >= DS_t[1])][1],
      SimBMObs_SB2[which(DS >= DS_t[2])][1],
      SimBMObs_SB2[which.closest(SimBMObs_SB2$DS,DS_t[3])])
    
    SimBMObs_SB2$DS_t<-DS_t
    setkey(SimBMObs_SB2,DS_t)
    
    SimBMObs_SB2 <- merge(SimBMObs_SB2,BBCHobs[Crop=="SB2" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_SB2[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_SB2[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Spring Barley 3    
    SimBMObs_SB3 <- simOut[Crop=="SB3",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_SB3[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="SB3" & DS_t==-1,]$date)]
    
    SimBMObs_SB3<-rbind(
      SimBMObs_SB3[which(DS >= DS_t[1])][1],
      SimBMObs_SB3[which(DS >= DS_t[2])][1],
      SimBMObs_SB3[which.closest(SimBMObs_SB3$DS,DS_t[3])])
    
    SimBMObs_SB3$DS_t<-DS_t
    setkey(SimBMObs_SB3,DS_t)
    
    SimBMObs_SB3 <- merge(SimBMObs_SB3,BBCHobs[Crop=="SB3" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_SB3[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_SB3[,DS_day.obs:=(DS_t+1)*day_df]
    
    #Spring Barley 4    
    SimBMObs_SB4 <- simOut[Crop=="SB4",.(Crop,date,DS)]
    
    DS_t<-c(0,1,2)
    SimBMObs_SB4[,day_df_sim:=as.numeric(date-BBCHobs[Crop=="SB4" & DS_t==-1,]$date)]
    
    SimBMObs_SB4<-rbind(
      SimBMObs_SB4[which(DS >= DS_t[1])][1],
      SimBMObs_SB4[which(DS >= DS_t[2])][1],
      SimBMObs_SB4[which.closest(SimBMObs_SB4$DS,DS_t[3])])
    
    SimBMObs_SB4$DS_t<-DS_t
    setkey(SimBMObs_SB4,DS_t)
    
    SimBMObs_SB4 <- merge(SimBMObs_SB4,BBCHobs[Crop=="SB4" & DS_t %in% c(0,1,2) ,.(DS_t,day_df)][1:3])
    
    SimBMObs_SB4[,DS_day.sim:=(DS+1)*day_df_sim]
    SimBMObs_SB4[,DS_day.obs:=(DS_t+1)*day_df]
    
    
    HydGOF_MA1<-data.table(t(gof.default.D(sim = SimBMObs_MA1$DS_day.sim,obs = SimBMObs_MA1$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_MA1[,year:=2005]
    
    HydGOF_WW1<-data.table(t(gof.default.D(sim = SimBMObs_WW1$DS_day.sim,obs = SimBMObs_WW1$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_WW1[,year:=2008]
    
    HydGOF_WR1<-data.table(t(gof.default.D(sim = SimBMObs_WR1$DS_day.sim,obs = SimBMObs_WR1$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_WR1[,year:=2007]
    
    HydGOF_PO1<-data.table(t(gof.default.D(sim = SimBMObs_PO1$DS_day.sim,obs = SimBMObs_PO1$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_PO1[,year:=2010]
   
    HydGOF_SB1<-data.table(t(gof.default.D(sim = SimBMObs_SB1$DS_day.sim,obs = SimBMObs_SB1$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_SB1[,year:=2006]
    
    HydGOF_SB2<-data.table(t(gof.default.D(sim = SimBMObs_SB2$DS_day.sim,obs = SimBMObs_SB2$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_SB2[,year:=2009]
    
    HydGOF_SB3<-data.table(t(gof.default.D(sim = SimBMObs_SB3$DS_day.sim,obs = SimBMObs_SB3$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_SB3[,year:=2011]
    
    HydGOF_SB4<-data.table(t(gof.default.D(sim = SimBMObs_SB4$DS_day.sim,obs = SimBMObs_SB4$DS_day.obs,norm = "maxmin",digits = 4, j = 1)))
    HydGOF_SB4[,year:=2012]
    
    HydGOF <- rbind(HydGOF_MA1,HydGOF_WW1,HydGOF_WR1,HydGOF_PO1,HydGOF_SB1,HydGOF_SB2,HydGOF_SB3,HydGOF_SB4)
    
    GOFMean <- HydGOF[,lapply(.SD,mean, na.rm=F)]
    GOFMean[,year:=0000]
    
    if (year==T){
      return(HydGOF)
    } else {
      return(GOFMean)
    }
  }
}

#Reading modelled drain output and calculating nMAE from the package HydroGOF
read.optim.HV.y <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_harvest.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_harvest.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 12){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    
    obs$Yield[,date:=ymd(date)]
    
    HRVHobs<-obs$Yield[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("07-08",interval[9])))]
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(day, month, year,sep="-"))]
    
    SimHRVObs <- merge(simOut[,.SD, .SDcols=c("date", "stem_DM",	"leaf_DM",	"sorg_DM")],
                       HRVHobs[,.SD, .SDcols=c("date", "Total_yield","crop")], all.y =T )
    SimHRVObs[,Total_Sim:= stem_DM+leaf_DM+sorg_DM]
    SimHRVObs[,Total_yield:= Total_yield/1000]
    
    
    HydGOF<-data.table(t(gof.default.D(sim = SimHRVObs$Total_Sim,obs = SimHRVObs$Total_yield,norm = "maxmin",digits = 4, j = 1)))
    
    HydGOF[,year:=0000]
    return(HydGOF)
    
  }}

#Reading modelled drain output and calculating nMAE from the package HydroGOF
read.optim.HV.N <- function(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  if (calib == T){
    
    daisyOut <- file.path(wdDir,OutDir,paste(Sys.getpid(), "_harvest.dlf", sep=""))
  } else {
    daisyOut <- file.path(wdDir,OutDir,paste(ind, "_harvest.dlf", sep=""))
  }
  
  
  simOut <- tryCatch({fread(daisyOut)}, error = function(e) {NULL})
  if (is.null(simOut) ) {
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else if (nrow(simOut)< 12){ #number of lines of fully processed file
    print(c(daisyOut,"error"))
    x <- data.table(t(gof.default.D(1:10,1:10)))
    x[x>=0]<-(1000)
    x[,year:=1000]
    x[,nMAE:=1000]
    return(x)
  } else {
    obs$Yield[,date:=ymd(date)]
    HRVHobs<-obs$Yield[date %between% c(dmy(paste("31-03",interval[1])),dmy(paste("07-08",interval[9])))]
    
    setnames(simOut, make.names(names(simOut)))
    simOut[, date := dmy(paste(day, month, year,sep="-"))]
    
    SimHRVObs <- merge(simOut[,.SD, .SDcols=c("date", "stem_N",	"leaf_N",	"sorg_N")],
                       HRVHobs[,.SD, .SDcols=c("date", "Total_N","crop")], all.y =T )
    SimHRVObs[,Total_Sim:= stem_N+leaf_N+sorg_N]
    SimHRVObs[,Total_N:= Total_N]
    
    HydGOF<-data.table(t(gof.default.D(sim = SimHRVObs$Total_Sim,obs = SimHRVObs$Total_N,na.rm = T,norm = "maxmin",digits = 4, j = 1)))
    
    HydGOF[,year:=0000]
    return(HydGOF)
    
  }}

read.optim <- function(ind, obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  S25<-read.optim.25.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  S60<-read.optim.60.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  S90<-read.optim.90.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  S110<-read.optim.110.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  S190<-read.optim.190.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  S210<-read.optim.210.swct(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  # SC1.N<-read.optim.SC1.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  # 
  # SC2.N<-read.optim.SC2.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  # DS<-read.optim.DS(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  HV.y<-read.optim.HV.y(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  HV.N<-read.optim.HV.N(ind, obs ,wdDir, OutDir, interval,year,sensitivity,calib,All)

  nMAE<- S25$nMAE+S60$nMAE+S90$nMAE+S110$nMAE+S190$nMAE+S210$nMAE+HV.y$nMAE+HV.N$nMAE #SC1.N$nMAE+SC2.N$nMAE
  
  # nMAE<-DS$nMAE
  
  return(nMAE)
}

daisy.PID <- function(PathToDaisy)
{
  #build the command
  daisy_old <- PathToDaisy
  daisy_new <- paste0("c:/Program Files/Daisy 5.59/bin/d", Sys.getpid(),".exe")
  
  file.copy(daisy_old,daisy_new, overwrite = T)
  return(daisy_new)
}


Cost.optim_D <- function(x,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  print(x)
  
  ctrldaisy <- do.call(Daisy.control, as.list(ctrldaisy))
  p.config <- ctrldaisy$p.config
  ctrldaisy$p=x
  ctrldaisy$ind=Sys.getpid()
  PathToDaisy<- daisy.PID(PathToDaisy)
  f.cost(RunFile = RunFile,showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy=Daisy.control(sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,dflt=ctrldaisy$dflt,costfunction=ctrldaisy$costfunction,obs=ctrldaisy$obs,wdDir=ctrldaisy$wdDir,OutDir = ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=ctrldaisy$ind,param_sens=ctrldaisy$param_sens,p=ctrldaisy$p, p.config=ctrldaisy$p.config))
  if(file.exists(PathToDaisy)){file.remove(PathToDaisy)}
  return(read.optim(ind = ctrldaisy$ind, obs = ctrldaisy$obs,wdDir = ctrldaisy$wdDir, OutDir = ctrldaisy$OutDir, interval = ctrldaisy$interval,year = ctrldaisy$year,sensitivity = ctrldaisy$sensitivity,calib = ctrldaisy$calib,All = ctrldaisy$All))
}



Cost.optim_D.kill<- function(x,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  
  tryCatch(R.utils::withTimeout(Cost.optim_D(x,RunFile,showLogFile,PathToDaisy,ctrldaisy),timeout=5*60,onTimeout="error"),
           error = function(kill) {
             KILL.daisy <- paste0('taskkill /f /im d', Sys.getpid(),'.exe')
             system(KILL.daisy)
             daisy_new <- paste0("c:/Program Files/Daisy 5.59/bin/d", Sys.getpid(),".exe")
             if(file.exists(daisy_new)){file.remove(daisy_new)}
             return(runif(1,200,1000))
           })
}

DaisyDeoptim<-function(RunFile,showLogFile,PathToDaisy,ctrldaisy){
  closeAllConnections()
  set.seed(1)
  p.config <- ctrldaisy$p.config
  param_matrix<-fread(p.config)
  
  #DEoptim contol parameters
  
  Base.Functions<-c("CheckParameters","runDaisy","f.update","updateParameters","f.cost","tryCatch","withTimeout","daisy.PID","gof.default.D")
  My.Packages <- c("data.table", "hydroGOF", "lubridate","RDaisy","R.utils","birk")
  My.Functions <- c(Base.Functions,"maxmin","read.optim.25.swct","read.optim.60.swct","read.optim.90.swct","read.optim.110.swct","read.optim.190.swct","read.optim.210.swct","read.optim.SC1.N","read.optim.SC2.N","read.optim.DS","read.optim.HV.y","read.optim.HV.N","read.optim","Cost.optim_D","Cost.optim_D.kill","DaisyMorris")
  
  
  #DeOptim calibration
  #Paralell cluster setup
  
  
  lowR <- param_matrix[name %in% ctrldaisy$param_sens, ]$min
  uppR <- param_matrix[name %in% ctrldaisy$param_sens, ]$max
  
  maxIT <- 200
  

  # initPOP <-as.matrix(data.table(t(ctrldaisy$obs$Calib.T$optim$bestmem)))
  # initPOP<-as.matrix(rbind(initPOP,
  #                            Latinhyper(param_matrix[name %in% ctrldaisy$param_sens,c("min","max")],detectCores()-1)))
   
  # colnames(initPOP)<-ctrldaisy$param_sens
  
  #ctrldaisy is not transfered to Cost.optim
  Calib.Sens <- DEoptim::DEoptim(fn=Cost.optim_D.kill,
                                 lower = lowR,
                                 upper = uppR,
                                 DEoptim::DEoptim.control(itermax=maxIT,parallelType = 1,packages = My.Packages, parVar = c(My.Functions),storepopfrom = 1, NP=detectCores()-10),#initialpop = initPOP,
                                 RunFile,showLogFile,PathToDaisy,ctrldaisy)
  
  return(Calib.Sens)
  closeAllConnections()
}




############################## RETENTION AND CONDUCTIVITY

plot.fitSHP.D <- function(x, ttle, ...) {
  
  if (x$input$suc.negativ == TRUE) {
    if (x$input$fit == 'both') {
      suc.th <- -1 * x$input$suc$th
      suc.K <- -1 * x$input$suc$K
    }
    if (x$input$fit == 'swc') {
      suc.th <- -1 * x$input$suc$th
    }
    if (x$input$fit == 'ku') {
      suc.K <- -1 * x$input$suc$K
    }
  }
  if (x$input$suc.negativ == FALSE) {
    if (x$input$fit == 'both') {
      suc.th <- x$input$suc$th
      suc.K <- x$input$suc$K
    }
    if (x$input$fit == 'swc') {
      suc.th <- x$input$suc$th
    }
    if (x$input$fit == 'ku') {
      suc.K <- x$input$suc$K
    }
  }
  ## Plot
  if(x$input$fit == 'both') {
    temp1 <- 10^(seq(log10(1), log10(max(suc.th)), length.out = 100))
    fit.swc <- SWC(temp1, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
    temp2 <- 10^(seq(log10(1), 4.2, length.out = 100))
    fit.ku <- Ku(temp2, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }
  if(x$input$fit == 'swc') {
    temp1 <- exp(seq(log(1), log(max(suc.th)), length.out = 100))
    
    fit.swc <- SWC(temp1, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }
  if(x$input$fit == 'ku') {
    temp2 <- exp(seq(log(1), log(max(suc.K)), length.out = 100))
    fit.ku <- Ku(temp2, par.shp = x$par, FUN.shp = x$input$FUN.shp, modality = x$input$modality, suc.negativ = FALSE)
  }
  
  ## Plot
  if(x$input$fit == 'both') {
    
    p1<-ggplot()+geom_point(aes(y=log10(suc.th[suc.th >0]), x = x$input$obs$th[suc.th >0],color="observation"))+
      geom_line(aes(y=log10(temp1),x=fit.swc,color="fitted"))+
      scale_color_manual(values = c("red","black"))+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "bottom")+
      ylab(expression(paste('pF '," log"[10]," (Pressure Head [cm])")))+
      xlab(expression(paste(theta," (cm"^3, " cm"^-3,')')))
    
    
    # Unsaturated hydraulic conductivity
    
    p2<-ggplot()+geom_point(aes(x=log10(suc.K[suc.K >0]), y = log10(x$input$obs$K[suc.K >0]),color="observation"))+
      geom_line(aes(y=log10(fit.ku), x=log10(temp2),color="fitted"))+
      scale_color_manual(values = c("red","black"))+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "bottom")+
      xlab(expression(paste('pF '," log"[10]," (Pressure Head [cm])")))+
      ylab(expression(paste("log"[10],"(K(h)"," (cm"^"h","))")))
    
    p<-grid.arrange(p1,p2,ncol=2,top = textGrob(ttle,gp=gpar(fontsize=10,font=1)))
    
    swc.obs<-cbind(obs.swc.h=suc.th[suc.th >0],obs.swc.th=x$input$obs$th[suc.th >0])
    swc.sim<-cbind(sim.swc.h=temp1,sim.swc.th=fit.swc)
    
    K.obs<-cbind(obs.K.h=suc.K[suc.K >0],obs.K.ku=x$input$obs$K[suc.K >0])
    K.sim<-cbind(sim.K.h=temp2,sim.K.ku=fit.ku)
    
    Data.SWC<-list(swc.obs,swc.sim)
    
    names(Data.SWC)<-c("swc.obs","swc.sim")
    
    Data.K<-list(K.obs,K.sim)
    
    names(Data.K)<-c("K.obs","K.sim")
    
    Data<-list(Data.SWC,Data.K)
    
    names(Data)<-c("Data.SWC","Data.K")
    
    return(Data)
    
    
  }
  if(x$input$fit == 'swc') {
    # Soil water content
    ggplot()+geom_point(aes(y=log10(suc.th[suc.th >0]), x = x$input$obs$th[suc.th >0],color="observation"))+
      geom_line(aes(y=log10(temp1),x=fit.swc,color="fitted"))+
      scale_color_manual(values = c("red","black"))+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "bottom")+
      ylab(expression(paste('pF '," log"[10]," (Pressure Head [cm])")))+
      xlab(expression(paste(theta," (cm"^3, " cm"^-3,')')))
    return(list(Data.swc))
  }
  if(x$input$fit == 'ku') {
    # Unsaturated hydraulic conductivity
    ggplot()+geom_point(aes(x=log10(suc.K[suc.K >0]), y = x$input$obs$K[suc.K >0],color="observation"))+
      geom_line(aes(y=fit.ku, x=log10(temp2),color="fitted"))+
      scale_color_manual(values = c("red","black"))+theme_bw()+
      theme(legend.title = element_blank(),
            legend.position = "bottom")+
      xlab(expression(paste('pF '," log"[10]," (Pressure Head [cm])")))+
      ylab(expression(paste("K(h)"," (cm"^"h",")")))
    return(list(Data.K))
  }
}


############################## PErformance Measure test test

Perf.run <- function(x,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  print(x)
  ctrldaisy <- do.call(Daisy.control, as.list(ctrldaisy))
  p.config <- ctrldaisy$p.config
  ctrldaisy$p=x
  PathToDaisy<- daisy.PID(PathToDaisy)
  f.cost(RunFile = RunFile,showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy=Daisy.control(sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,dflt=ctrldaisy$dflt,costfunction=ctrldaisy$costfunction,obs=ctrldaisy$obs,wdDir=ctrldaisy$wdDir,OutDir = ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=ctrldaisy$ind,param_sens=ctrldaisy$param_sens,p=ctrldaisy$p, p.config=ctrldaisy$p.config))
  if(file.exists(PathToDaisy)){file.remove(PathToDaisy)}
  return("done")
}



Perf.run.kill<- function(x,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  
  tryCatch(R.utils::withTimeout(Perf.run(x,RunFile,showLogFile,PathToDaisy,ctrldaisy),timeout=15*60,onTimeout="error"),
           error = function(kill) {
             KILL.daisy <- paste0('taskkill /f /im d', Sys.getpid(),'.exe')
             system(KILL.daisy)
             daisy_new <- paste0("c:/Program Files/Daisy 5.59/bin/d", Sys.getpid(),".exe")
             if(file.exists(daisy_new)){file.remove(daisy_new)}
             return("killed")
           })
}

PerfMeas<- function(parampop,RunFile,showLogFile,PathToDaisy,ctrldaisy){
  
  set.seed(1988)
  
  param_matrix<-fread(ctrldaisy$p.config)[!name %like% "DS"]
  
  TestMatrix<-as.matrix(Latinhyper(param_matrix[name %in% ctrldaisy$param_sens,c("min","max")],parampop))
  
  colnames(TestMatrix)<-ctrldaisy$param_sens
  
  #Paralell cluster setup
  ncores <- detectCores()-6#detecting how many cores, your computer has.
  cltype <- ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")
  
  closeAllConnections() #before open connection, make sure all of them is closed.
  
  clusters <- makeCluster(ncores, type = cltype) #creating computation clusters
  registerDoParallel(clusters) #register all clusters
  
  showConnections(all = TRUE)# with this you are able to inspect how many clusters you have
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk","R.utils")
  
  write.csv(TestMatrix,file="LHS5000.csv",row.names = F)
  # #run daisy for each paramater set.
  
  # Out.Sens <- foreach(i=1:nrow(TestMatrix),
  #                     .packages = My.Packages,
  #                     .export=c("Perf.run","Perf.run.kill","daisy.PID")) %dopar% Perf.run(x = TestMatrix[i,],
  #                                                                                    RunFile,
  #                                                                                    showLogFile,
  #                                                                                    PathToDaisy,
  #                                                                                    ctrldaisy =   Daisy.control(sensitivity=ctrldaisy$sensitivity,
  #                                                                                                                calib = ctrldaisy$calib,
  #                                                                                                                dflt = ctrldaisy$dflt,
  #                                                                                                                wdDir=ctrldaisy$wdDir,
  #                                                                                                                OutDir=ctrldaisy$OutDir,
  #                                                                                                                interval=ctrldaisy$interval,
  #                                                                                                                year=ctrldaisy$year,
  #                                                                                                                All=ctrldaisy$All,
  #                                                                                                                costfunction = NULL,
  #                                                                                                                param_sens = ctrldaisy$param_sens,
  #                                                                                                                p.config=ctrldaisy$p.config,
  #                                                                                                                obs=ctrldaisy$obs,
  #                                                                                                                ind=i))
  # 
  # 
  
  ObjMEas <- read_morris(param_matrix = as.data.table(TestMatrix),obs = ctrldaisy$obs,wdDir = ctrldaisy$wdDir, OutDir = ctrldaisy$OutDir, interval = ctrldaisy$interval,year = ctrldaisy$year,sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,All = ctrldaisy$All)
  
  return(ObjMEas)
}


gof.default.D <- function (sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                           j=1, norm="maxmin", s=c(1,1,1), method=c("2009", "2012"), 
                           lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){
  
  method   <- match.arg(method)
  
  ME     <- me(sim, obs, na.rm=na.rm)
  MAE    <- mae(sim, obs, na.rm=na.rm)
  MSE    <- mse(sim, obs, na.rm=na.rm)
  RMSE   <- rmse(sim, obs, na.rm=na.rm) 
  NRMSE  <- nrmse(sim, obs, na.rm=na.rm, norm=norm)
  RSR    <- rsr(sim, obs, na.rm=na.rm, ...)
  rSD    <- rSD(sim, obs, na.rm=na.rm)     
  PBIAS  <- pbias(sim, obs, na.rm=na.rm, ...)
  NSE    <- NSE(sim, obs, na.rm=na.rm, ...)
  mNSE   <- mNSE(sim, obs, na.rm=na.rm, j=j, ...)
  rNSE   <- rNSE(sim, obs, na.rm=na.rm, ...)
  d      <- d(sim, obs, na.rm=na.rm, ...)
  md     <- md(sim, obs, na.rm=na.rm, ...)
  rd     <- rd(sim, obs, na.rm=na.rm, ...)
  cp     <- cp(sim, obs, na.rm=na.rm, ...)
  r      <- rPearson(sim, obs)
  bR2    <- br2(sim, obs, na.rm=na.rm, ...)     
  KGE    <- KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", ...) 
  VE     <- VE(sim, obs, na.rm=na.rm, ...)  
  nMAE   <- MAE/mean(obs, na.rm=na.rm, ...)  
  
  # 'R2' is the Coefficient of Determination
  # The coefficient of determination, R2, is useful because it gives the proportion of
  # the variance (fluctuation) of one variable that is predictable from the other variable.
  # It is a measure that allows us to determine how certain one can be in making
  # predictions from a certain model/graph.
  # The coefficient of determination is the ratio of the explained variation to the total
  # variation.
  # The coefficient of determination is such that 0 <  R2 < 1,  and denotes the strength
  # of the linear association between x and y. 
  R2 <- r^2
  
  if (do.spearman) {
    r.Spearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs") 
    
    # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
    # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
    if ( is.matrix(r.Spearman) | is.data.frame(r.Spearman) ) {
      r.Spearman        <- diag(r.Spearman)
    } # IF end
    
  } # IF end
  
  if (do.pbfdc) { pbfdc  <- pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, ...) }
  
  gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, md, rd, cp, r, R2, bR2, KGE, VE, nMAE)     
  
  rownames(gof)[5] <- "NRMSE"
  rownames(gof)[6] <- "PBIAS"    
  
  if (do.spearman) { gof <- rbind(gof, r.Spearman) }
  
  if (do.pbfdc) { 
    gof <- rbind(gof, pbfdc) 
    rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
  } # IF end
  
  # Rounding the final results, ofr avoiding scientific notation
  gof <- round(gof, digits)
  
  return(gof)
  
}