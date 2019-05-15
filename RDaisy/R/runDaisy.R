#'@title Function to run the Daisy model
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function runs the Daisy model.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Define where Daisy is installed. NOTE: This user should have admin rights to this folder.
#'@param ctrldaisy Extra arguments that are needed for Optimization/Calibration and Sensitivity analysis (see  \code{\link{Daisy.control}}. Do not add it when only a single simple run is neded)
#'
#'
#'@examples
#'\dontrun{
#'RunFile="C:/Program Files/Daisy 5.59/sample/test.dai"
#'PathToDaisy = "C:/Program Files/Daisy 5.59/bin/daisy.exe"
#'runDaisy(RunFile=RunFile, showLogFile = FALSE, PathToDaisy = PathToDaisy,ctrldaisy=Daisy.control())
#'}
#'@import data.table
#'@import stringr
#' @export

runDaisy <- function(RunFile, showLogFile = TRUE, PathToDaisy = "C:/Program Files/Daisy 5.49/bin/daisy.exe",ctrldaisy=Daisy.control()){
  control <- do.call(Daisy.control, as.list(ctrldaisy))
  sensitivity <- control$'sensitivity'
  calib <- control$calib
  dflt <- control$dflt
  costfunction <- control$costfunction
  obs <- control$obs
  wdDir <- control$wdDir
  OutDir <- control$OutDir
  interval <- control$interval
  year <- control$year
  All <- control$All
  param_sens <- control$param_sens
  p.config <- control$p.config
  p <- control$p
  ind <- control$ind
  
    
  if(sensitivity==TRUE & calib ==TRUE & dflt==TRUE){
    stop("Default, Sensitivity and calibration are selected. Please select only one option.")
  }  
    if(sensitivity==TRUE & calib ==TRUE){
    stop("Sensitivity and calibration are selected. Please select only one option.")
  }
  if(dflt==TRUE & calib ==TRUE){
    stop("Both default and calibration are selected. Please select only one option.")
  }
  if(sensitivity==TRUE & dflt ==TRUE){
    stop("Both Sensitivity and default are selected. Please select only one option.")
  }
  if(sensitivity==TRUE){
    costfunction<-as.function(costfunction)
    MR<-costfunction(RunFile,showLogFile,PathToDaisy,ctrldaisy)
    return(MR)
  }
  
  if(calib ==TRUE){
    costfunction<-as.function(costfunction)
    DE<-costfunction(RunFile,showLogFile,PathToDaisy,ctrldaisy)
    return(DE)
  }
  if(dflt == TRUE){
    if(file.exists(p.config)==TRUE){p.config <- data.table::fread(p.config)}
    CheckParameters(p.config) #Here we check for any duplicates in the p.config file
    
    ind<-NULL
    p.config[, f.update(p.config, RunFile,wdDir,sensitivity, calib,dflt,ind)]
    
    RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
                   "input",paste0(Sys.getpid(),"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    
    #build the command
    cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
    #run it
    system(cmdToRun, show.output.on.console = showLogFile  )
  } else {
    
    #RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
    #               "input",paste0(Sys.getpid(),"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    #build the command
    cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
    #run it
    system(cmdToRun, show.output.on.console = showLogFile  )}
  
}
