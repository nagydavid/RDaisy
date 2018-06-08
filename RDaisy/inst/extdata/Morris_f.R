#morris

#compile Morris Matrix output
morris.swct0_30 <- function(ind,obs, wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.swct")) %dopar%  Mod.ouT[i, read.optim.0_30.swct(ind,obs, wdDir, OutDir, interval,year,DEoptim),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct0_30_ouT <- data.table(p.ind=Mod.ouP$p.ind, swct0_30=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="swct0_30"]
    return(Mod.ouP)
  } else {
    return(swct0_30_ouT)
  }}

morris.swct30_60 <- function(ind, obs,wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.swct")) %dopar%  Mod.ouT[i, read.optim.30_60.swct(ind, obs,wdDir, OutDir, interval,year,DEoptim),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct30_60_ouT <- data.table(p.ind=Mod.ouP$p.ind, swct30_60=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="swct30_60"]
    return(Mod.ouP)
  } else {
    return(swct30_60_ouT)
  }}

morris.swct60_90 <- function(ind,obs, wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.swct")) %dopar%  Mod.ouT[i, read.optim.60_90.swct(ind, obs,wdDir, OutDir, interval,year,DEoptim),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  swct60_90_ouT <- data.table(p.ind=Mod.ouP$p.ind, swct60_90=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="swct60_90"]
    return(Mod.ouP)
  } else {
    return(swct60_90_ouT)
  }}


#compile Morris Matrix output
morris.N0_30 <- function(ind,obs, wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.N")) %dopar%  Mod.ouT[i, read.optim.0_30.N(ind,obs, wdDir, OutDir, interval,year,DEoptim),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N0_30_ouT <- data.table(p.ind=Mod.ouP$p.ind, N0_30=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="N0_30"]
    return(Mod.ouP)
  } else {
    return(N0_30_ouT)
  }}

morris.N30_60 <- function(ind,obs, wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.N")) %dopar%  Mod.ouT[i, read.optim.30_60.N(ind,obs, wdDir, OutDir, interval,year,DEoptim),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N30_60_ouT <- data.table(p.ind=Mod.ouP$p.ind, N30_60=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="N30_60"]
    return(Mod.ouP)
  } else {
    return(N30_60_ouT)
  }}

morris.N60_90 <- function(ind,obs, wdDir, OutDir, interval,year,DEoptim,All){
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.0_30.N")) %dopar%  Mod.ouT[i, read.optim.60_90.N(ind,obs, wdDir, OutDir, interval,year,DEoptim),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouP)
  Mod.ouT <- Mod.ouT[!NSE==(1000)]
  N60_90_ouT <- data.table(p.ind=Mod.ouP$p.ind, N60_90=1-Mod.ouP$NSE)
  if (All==T){
    Mod.ouP[,Obj:="N60_90"]
    return(Mod.ouP)
  } else {
    return(N60_90_ouT)
  }}



read_morris <- function(param_matrix,obs,wdDir, OutDir, interval,year,DEoptim,All){
  swct0_30<-morris.swct0_30(ind, obs,wdDir, OutDir, interval,year,DEoptim,All)
  
  swct30_60<-morris.swct30_60(ind,obs, wdDir, OutDir, interval,year,DEoptim,All)
  
  swct60_90<-morris.swct60_90(ind,obs, wdDir, OutDir, interval,year,DEoptim,All)
  
  N0_30<-morris.N0_30(ind, wdDir, obs,OutDir, interval,year,DEoptim,All)
  
  N30_60<-morris.N30_60(ind, wdDir,obs, OutDir, interval,year,DEoptim,All)
  
  N60_90<-morris.N60_90(ind, wdDir,obs, OutDir, interval,year,DEoptim,All)
  
  AllObj <- rbind(swct0_30,swct30_60,swct60_90,N0_30,N30_60,N60_90)
  setkey(AllObj, p.ind)
  
  return(AllObj)
}

Daisy.Morris<-function(p.config,RunFile,PathToDaisy,obs,wdDir, OutDir, interval,year,DEoptim,All){
  require(data.table)
  require(sensitivity)
  require(doParallel)
  require(foreach)
  require(hydroGOF)
  require(lubridate)
  
  set.seed(1)
  
  param_matrix<-fread(p.config)
  
  #running empty morris to get the Morris matrix
  Morris.sens <- morris(model = NULL, factors = param_matrix$name, r = 10 ,design = list(type = "oat", levels = 5, grid.jump = 3), 
                        binf = param_matrix$min, bsup = param_matrix$max)
  
  
  #Paralell cluster setup
  ncores <- detectCores()#detecting how many cores, your computer has.
  cltype <- ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")
  
  closeAllConnections() #before open connection, make sure all of them is closed.
  
  clusters <- makeCluster(ncores-10, type = cltype) #creating computation clusters
  registerDoParallel(clusters) #register all clusters
  
  #showConnections(all = TRUE) with this you are able to inspect how many clusters you have
  
  My.packages <- c("lubridate","data.table", "HydroGOF")
  
  #run daisy for each paramater set from the Sensitivity Matrix
  
  Out.Sens <- foreach(i=1:nrow(Morris.sens$X), .packages = My.Packages ) %dopar% f.cost(p=Morris.sens$X[i], p.config, obs, RunFile,PathToDaisy,Morris=FALSE,DEoptim=FALSE,ind=i)
  
  sens.dt <- as.data.table(Morris.sens$X)
  
  #read all sensitivity measures for each parameter set.
  
  Morris.Obj <- read_morris(param_matrix,obs,wdDir, OutDir, interval,year,DEoptimptim,All)
  
  #Getting the mean of all sensitivity measures, IT CAN BE CHANGED, here we are taking the mean of the two year of calibration period
  Morris.Sens <- Morris.Obj[,lapply(.SD, mean),by=.(p.ind, Obj)]
  Morris.Sens[,NSE:=1-NSE]
  
  #subsetting only for NSE values
  Morris.NSE <- dcast(Morris.Sens[,.(p.ind,Obj,NSE)], p.ind  ~ Obj, value.var="NSE")
  
  #Creating response function to Morris, ion order to get the elementary effect(EE) of the parameters in light of each objectives
  
  get.response<-function(X)
  {
    swct0_30 <- Morris.NSE[,get(x="swct0_30")]
    swct30_60 <- Morris.NSE[,get(x="swct30_60")]
    swct60_90 <- Morris.NSE[,get(x="swct60_90")]
    N0_30 <- Morris.NSE[,get(x="N0_30")]
    N30_60 <- Morris.NSE[,get(x="N30_60")]
    N60_90 <- Morris.NSE[,get(x="N60_90")]
    
    cbind(swct0_30,swct30_60,swct60_90,N0_30,N30_60,N60_90)
  }
  
  # running Morris again to get the EE
  set.seed(1)
  Morris.sens <- morris(model = get.response, factors = pconfig$name, r = 10 ,design = list(type = "oat", levels = 5, grid.jump = 3), 
                        binf = pconfig$min, bsup = pconfig$max)
  
  
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
  
  return(Morris.plot.dt)
  
}

#Daisy.Morris(p.config,RunFile,PathToDaisy,obs,wdDir, OutDir, interval,year,DEoptim,All)
