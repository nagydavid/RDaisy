#compile Morris Matrix output
morris.swct25 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.25.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.25.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                         by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct25_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct25=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct25"]
    return(Mod.ouT)
  } else {
    return(swct25_ouT)
  }}

morris.swct60 <- function(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.60.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.60.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                          by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct60_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct60=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct60"]
    return(Mod.ouT)
  } else {
    return(swct60_ouT)
  }}

morris.swct90 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.90.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.90.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                          by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct90_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct90=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct90"]
    return(Mod.ouT)
  } else {
    return(swct90_ouT)
  }}


#compile Morris Matrix output
morris.swct110 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.110.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.110.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                       by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct110_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct110=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct110"]
    return(Mod.ouT)
  } else {
    return(swct110_ouT)
  }}

morris.swct190 <- function(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.190.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.190.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                       by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct190_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct190=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct190"]
    return(Mod.ouT)
  } else {
    return(swct190_ouT)
  }}

morris.swct210 <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.210.swct" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.210.swct(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                       by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  swct210_ouT <- data.table(p.ind=Mod.ouT$p.ind, swct210=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="swct210"]
    return(Mod.ouT)
  } else {
    return(swct210_ouT)
  }}


#compile Morris Matrix output
morris.SC1.N <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.SC1.N" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.SC1.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                      by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  SC1.N_ouT <- data.table(p.ind=Mod.ouT$p.ind, SC1.N=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="SC1.N"]
    return(Mod.ouT)
  } else {
    return(SC1.N_ouT)
  }}

morris.SC2.N <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.SC2.N" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.SC2.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                     by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  SC2.N_ouT <- data.table(p.ind=Mod.ouT$p.ind, SC2.N=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="SC2.N"]
    return(Mod.ouT)
  } else {
    return(SC2.N_ouT)
  }}



morris.DS <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.DS" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.DS(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                  by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  DS_ouT <- data.table(p.ind=Mod.ouT$p.ind, Yield=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="DS"]
    return(Mod.ouT)
  } else {
    return(DS_ouT)
  }}

morris.HV.y <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.HV.y" , "gof.default.D")) %do%  Mod.ouT[i, read.optim.HV.y(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                     by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  HV.y_ouT <- data.table(p.ind=Mod.ouT$p.ind, HV.y=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="HV.y"]
    return(Mod.ouT)
  } else {
    return(HV.y_ouT)
  }}

morris.HV.N <- function(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All){
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  #create data table to store output files 
  Mod.ouT <- data.table("p.ind" = 1:nrow(param_matrix)) 
  #reading
  Mod.ouT <- foreach(i=1:nrow(param_matrix), .packages = My.Packages, 
                     .export=c("read.optim.HV.N" , "gof.default.D")) %dopar%  Mod.ouT[i, read.optim.HV.N(ind = i,obs = obs,wdDir =  wdDir,OutDir =  OutDir, interval = interval,year = year,sensitivity = sensitivity,calib = calib),
                                                                    by = p.ind]
  #merging output
  Mod.ouT <- rbindlist(Mod.ouT)
  Mod.ouT <- Mod.ouT[!nMAE==(1000)]
  HV.N_ouT <- data.table(p.ind=Mod.ouT$p.ind, HV.N=Mod.ouT$nMAE)
  if (All==T){
    Mod.ouT[,Obj:="HV.N"]
    return(Mod.ouT)
  } else {
    return(HV.N_ouT)
  }}



read_morris <- function(param_matrix,obs,wdDir, OutDir, interval,year,sensitivity,calib,All){
  swct25<-morris.swct25(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct60<-morris.swct60(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct90<-morris.swct90(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct110<-morris.swct110(param_matrix, obs,wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct190<-morris.swct190(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  swct210<-morris.swct210(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  # SC1.N<-morris.SC1.N(param_matrix,obs,  wdDir, OutDir, interval,year,sensitivity,calib,All)
  # 
  # SC2.N<-morris.SC2.N(param_matrix,obs,  wdDir,OutDir, interval,year,sensitivity,calib,All)
  # 
  HV.Y<-morris.HV.y(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  HV.N<-morris.HV.N(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
 # DS<-morris.DS(param_matrix,obs, wdDir, OutDir, interval,year,sensitivity,calib,All)
  
  AllObj <- rbind(swct25,swct60,swct90,swct110,swct190,swct210,HV.Y,HV.N)#,DS ,SC1.N,SC2.N
  setkey(AllObj, p.ind)
  
  return(AllObj)
}

DaisyMorris<-function(RunFile,showLogFile,PathToDaisy,ctrldaisy){
  
  SeeD<-1
  
  set.seed(SeeD)
  
  param_matrix<-fread(ctrldaisy$p.config)[!name %like% "DS"]
  
  #running empty morris to get the Morris matrix
  Morris.sens <- sensitivity::morris(model = NULL, factors = param_matrix$name,scale=T ,r=10 ,design = list(type = "oat", levels = 10, grid.jump = 5), 
                                     binf = param_matrix$min, bsup = param_matrix$max)
  
  
  #Paralell cluster setup
  ncores <- detectCores()#detecting how many cores, your computer has.
  cltype <- ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")
  
  closeAllConnections() #before open connection, make sure all of them is closed.
  
  clusters <- makeCluster(ncores, type = cltype) #creating computation clusters
  registerDoParallel(clusters) #register all clusters
  
  showConnections(all = TRUE)# with this you are able to inspect how many clusters you have
  
  My.Packages <- c("lubridate","data.table", "hydroGOF","RDaisy","birk")
  
  write.csv(Morris.sens$X,file="morris.csv")
  
  #run daisy for each paramater set from the Sensitivity Matrix
  
  # Out.Sens <- foreach(i=1:nrow(Morris.sens$X), .packages = My.Packages ) %dopar% f.cost(RunFile=RunFile, showLogFile=showLogFile,PathToDaisy=PathToDaisy,ctrldaisy = Daisy.control(sensitivity = T,calib = FALSE, dflt = FALSE,costfunction = NULL,obs=ctrldaisy$obs, wdDir=ctrldaisy$wdDir, OutDir=ctrldaisy$OutDir,interval=ctrldaisy$interval,year=ctrldaisy$year,All=ctrldaisy$All,ind=i,param_sens = param_matrix$name,p=Morris.sens$X[i,], p.config=ctrldaisy$p.config))
  # 
  sens.dt <- as.data.table(Morris.sens$X)
  
  #read all sensitivity measures for each parameter set.
  
  Morris.Obj <- read_morris(param_matrix = Morris.sens$X,obs = ctrldaisy$obs,wdDir = ctrldaisy$wdDir, OutDir = ctrldaisy$OutDir, interval = ctrldaisy$interval,year = ctrldaisy$year,sensitivity = ctrldaisy$sensitivity,calib=ctrldaisy$calib,All = ctrldaisy$All)
  
  setnames(Morris.Obj, make.names(names(Morris.Obj)))
   
  
  Morris.Obj[,NRMSE:=NRMSE]
  Morris.Obj[,mNSE:=1-mNSE]
  Morris.Obj[,VE:=1-VE]
  Morris.Obj[,PBIAS:=abs(PBIAS)]
  #Getting the mean of all sensitivity measures, IT CAN BE CHANGED, here we are taking the mean of the two year of calibration period
  Morris.Sens<- Morris.Obj[,lapply(.SD, mean),by=.(p.ind, Obj)]
  
  
  #subsetting only for nMAE values
  
  Morris.nMAE <- dcast(Morris.Sens[,.(p.ind,Obj,nMAE)], p.ind  ~ Obj, value.var="nMAE")
  Morris.mNSE <- dcast(Morris.Sens[,.(p.ind,Obj,mNSE)], p.ind  ~ Obj, value.var="mNSE")
  Morris.NRMSE <- dcast(Morris.Sens[,.(p.ind,Obj,NRMSE)], p.ind  ~ Obj, value.var="NRMSE")
  Morris.VE <- dcast(Morris.Sens[,.(p.ind,Obj,VE)], p.ind  ~ Obj, value.var="VE")
  Morris.PBIAS <- dcast(Morris.Sens[,.(p.ind,Obj,PBIAS)], p.ind  ~ Obj, value.var="PBIAS")
  
  Morris.list<-list(Morris.nMAE,Morris.mNSE,Morris.NRMSE,Morris.VE,Morris.PBIAS)
  
  names(Morris.list)<-c("nMAE","mNSE","NRMSE","VE","PBIAS")
  
  #Creating response function to Morris, in order to get the elementary effect(EE) of the parameters in light of each objectives
  
  get.response<-function(X,DT)
  {
    swct25 <- DT[,get(x="swct25")]
    swct60 <- DT[,get(x="swct60")]
    swct90 <- DT[,get(x="swct90")]
    swct110 <- DT[,get(x="swct110")]
    swct190 <- DT[,get(x="swct190")]
    swct210 <- DT[,get(x="swct210")]
    # SC1.N <- DT[,get(x="SC1.N")]
    # SC2.N <- DT[,get(x="SC2.N")]
    HV.y <- DT[,get(x="HV.y")]
    HV.N <- DT[,get(x="HV.N")]
   # DS <- DT[,get(x="DS")]
    
    cbind(swct25,swct60,swct90,swct110,swct190,swct210,HV.y,HV.N)#SC1.N,SC2.N,DS
  }
  
  Morris.dt.list<-NULL
  for(i in 1:length(Morris.list)){
    
  # running Morris again to get the EE
  set.seed(SeeD)
  
  Morris.sens <- sensitivity::morris(model = get.response, factors = param_matrix$name ,r=10 ,design = list(type = "oat", levels = 10, grid.jump = 5), 
                                     binf = param_matrix$min, bsup = param_matrix$max, DT=Morris.list[[i]])
  
  
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
  
  mu.star.m <- as.data.table(melt(mu.star,value=.("swct25","swct60","swct90","swct110","swct190","swct210","HV.y","HV.N","DS")))#,"SC1.N","SC2.N"
  
  sigma.m <- as.data.table(melt(sigma,value=.("swct25","swct60","swct90","swct110","swct190","swct210","HV.y","HV.N","DS")))#,"SC1.N","SC2.N"
  
  Morris.plot.dt<-merge(mu.star.m,sigma.m,by=c("Var1","Var2"))
  
  names(Morris.plot.dt)<- c("par","obj","mu.star","sigma")
  
  Morris.plot.dt[,PeCr:=names(Morris.list)[i]]
  
  Morris.dt.list<-rbind(Morris.dt.list,Morris.plot.dt)
  
  
  }
  
  closeAllConnections()
  
  return(Morris.dt.list)
  
}

