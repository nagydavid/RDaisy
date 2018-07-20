#morris

#compile Morris Matrix output
morris.swct0_30 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.swct")) %dopar%  Mod.ouT[i, read.optim.0_30.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct0_30_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct0_30=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="swct0_30"]
    return(Mod.ouT)
  } else {
    return(swct0_30_ouT)
  }}

morris.swct30_60 <- function(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.30_60.swct")) %dopar%  Mod.ouT[i, read.optim.30_60.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct30_60_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct30_60=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="swct30_60"]
    return(Mod.ouT)
  } else {
    return(swct30_60_ouT)
  }}

morris.swct60_90 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.60_90.swct")) %dopar%  Mod.ouT[i, read.optim.60_90.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct60_90_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct60_90=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="swct60_90"]
    return(Mod.ouT)
  } else {
    return(swct60_90_ouT)
  }}


#compile Morris Matrix output
morris.N0_30 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.N")) %dopar%  Mod.ouT[i, read.optim.0_30.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N0_30_ouT <- data.table(p.ind=Mod.ouT$p.ind, N0_30=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="N0_30"]
    return(Mod.ouT)
  } else {
    return(N0_30_ouT)
  }}

morris.N30_60 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.30_60.N")) %dopar%  Mod.ouT[i, read.optim.30_60.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N30_60_ouT <- data.table(p.ind=Mod.ouT$p.ind, N30_60=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="N30_60"]
    return(Mod.ouT)
  } else {
    return(N30_60_ouT)
  }}

morris.N60_90 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.60_90.N")) %dopar%  Mod.ouT[i, read.optim.60_90.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N60_90_ouT <- data.table(p.ind=Mod.ouT$p.ind, N60_90=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="N60_90"]
    return(Mod.ouT)
  } else {
    return(N60_90_ouT)
  }}

morris.Yield <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.Yield")) %dopar%  Mod.ouT[i, read.optim.Yield(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                       by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  Yield_ouT <- data.table(p.ind=Mod.ouT$p.ind, Yield=1-Mod.ouT$NSE)
  if (All==T){
    Mod.ouT[,Obj:="Yield"]
    return(Mod.ouT)
  } else {
    return(Yield_ouT)
  }}




read_morris <- function(param_matrix,obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  swct0_30<-morris.swct0_30(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All)

  swct30_60<-morris.swct30_60(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)

  swct60_90<-morris.swct60_90(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)

  N0_30<-morris.N0_30(param_matrix,obs,  wdDir, OutDir, interval,year,sensitivity,calib,All)

  N30_60<-morris.N30_60(param_matrix,obs,  wdDir,OutDir, interval,year,sensitivity,calib,All)

  N60_90<-morris.N60_90(param_matrix,obs,  wdDir, OutDir, interval,year,sensitivity,calib,All)

  Yield<-morris.Yield(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)

  AllObj <- rbind(swct0_30,swct30_60,swct60_90,N0_30,N30_60,N60_90,Yield)
  setkey(AllObj, p.ind)
  
  return(AllObj)
}

DaisyMorris<-function(RunFile,showLogFile,PathToDaisy,ctrldaisy){

  set.seed(1)
  
  param_matrix<-fread(ctrldaisy$p.config)

  #running empty morris to get the Morris matrix
  Morris.sens <- sensitivity::morris(model = NULL, factors = param_matrix$name, r=10 ,design = list(type = "oat", levels = 10, grid.jump = 5), 
                        binf = param_matrix$min, bsup = param_matrix$max)
  
  
  #Paralell cluster setup
  ncores <- detectCores()#detecting how many cores, your computer has.
  cltype <- ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")
  
  closeAllConnections() #before open connection, make sure all of them is closed.
  
  clusters <- makeCluster(ncores, type = cltype) #creating computation clusters
  registerDoParallel(clusters) #register all clusters
  
  showConnections(all = TRUE)# with this you are able to inspect how many clusters you have
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy")
  
  #run daisy for each paramater set from the Sensitivity Matrix
  
  Out.Sens <- foreach(i=1:nrow(Morris.sens$X), .packages = My.Packages ) %dopar% f.cost(RunFile=RunFile, showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy = Daisy.control(sensitivity = T,calib = FALSE, dflt = FALSE,costfunction = NULL,obs=ctrldaisy$obs, wdDir=ctrldaisy$wdDir, OutDir=ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=i,param_sens = NULL,p=Morris.sens$X[i,], p.config=ctrldaisy$p.config))

  sens.dt <- as.data.table(Morris.sens$X)
  
  #read all sensitivity measures for each parameter set.
  
  Morris.Obj <- read_morris(param_matrix = Morris.sens$X,obs = ctrldaisy$obs,wdDir = ctrldaisy$wdDir, OutDir = ctrldaisy$OutDir, interval = ctrldaisy$interval,year = ctrldaisy$year,sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,All = ctrldaisy$All)
  
  #Getting the mean of all sensitivity measures, IT CAN BE CHANGED, here we are taking the mean of the two year of calibration period
  Morris.Sens<- Morris.Obj[,lapply(.SD, mean),by=.(p.ind, Obj)]
  Morris.Sens[,NSE:=1-NSE]
  
  #subsetting only for NSE values
  Morris.NSE <- dcast(Morris.Sens[,.(p.ind,Obj,NSE)], p.ind  ~ Obj, value.var="NSE")
  
  #Creating response function to Morris, in order to get the elementary effect(EE) of the parameters in light of each objectives
  
  get.response<-function(X)
  {
    swct0_30 <- Morris.NSE[,get(x="swct0_30")]
    swct30_60 <- Morris.NSE[,get(x="swct30_60")]
    swct60_90 <- Morris.NSE[,get(x="swct60_90")]
    N0_30 <- Morris.NSE[,get(x="N0_30")]
    N30_60 <- Morris.NSE[,get(x="N30_60")]
    N60_90 <- Morris.NSE[,get(x="N60_90")]
    Yield <- Morris.NSE[,get(x="Yield")]
    
    cbind(swct0_30,swct30_60,swct60_90,N0_30,N30_60,N60_90,Yield)
  }
  
  # running Morris again to get the EE
  set.seed(1)
  
  Morris.sens <- sensitivity::morris(model = get.response, factors = param_matrix$name, r=10 ,design = list(type = "oat", levels = 10, grid.jump = 5), 
                        binf = param_matrix$min, bsup = param_matrix$max)
  
  
  #Morris mean(mu)
  mu <- apply(Morris.sens$ee, 3, function(M){
    apply(M, 2, mean)
  })
  
  #Morris absolute mean(mu.star) F. Campolongo, J. Cariboni and A. Saltelli, 2007, An effective screening design for sensitivity, Environmental Modelling \& Software, 22, 1509–1518.
  mu.star <- apply(abs(Morris.sens$ee), 3, function(M){
    apply(M, 2, mean)
  })
  
  #Morris standard deviation
  sigma <- apply(Morris.sens$ee, 3, function(M){
    apply(M, 2, sd)
  })
  
  mu.star.m <- as.data.table(melt(mu.star,value=.("swct0_30","swct30_60","swct60_90","N0_30","N30_60","N60_90")))
  
  sigma.m <- as.data.table(melt(sigma,value=.("swct0_30","swct30_60","swct60_90","N0_30","N30_60","N60_90")))
  
  Morris.plot.dt<-merge(mu.star.m,sigma.m,by=c("Var1","Var2"))
  
  names(Morris.plot.dt)<- c("par","obj","mu.star","sigma")
  
  closeAllConnections()
  
  return(Morris.plot.dt)
  
}
