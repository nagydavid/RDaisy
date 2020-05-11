#'@title Function to run the Daisy model
#'
#'@author D Nagy, JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Define where Daisy is installed. NOTE: This user should have admin rights to this folder.
#'@param ctrluser Extra arguments that are needed for user designed Cost function (see  \code{\link{User.control}}. Do not add it when only a single simple run is neded)
#'@param ctrldaisy Extra arguments that are needed for Optimization/Calibration and Sensitivity analysis (see  \code{\link{Daisy.control}}. Do not add it when only a single simple run is neded)
#'
#'@examples
#'\dontrun{
#'RunFile="C:/Program Files/Daisy 5.59/sample/test.dai"
#'PathToDaisy = "C:/Program Files/Daisy 5.59/bin/daisy.exe"
#'runDaisy(RunFile=RunFile, showLogFile = FALSE, PathToDaisy = PathToDaisy)
#'}
#'@import data.table processx stringr
#' @export

runDaisy <- function(RunFile, showLogFile = TRUE, PathToDaisy = "C:/Program Files/Daisy 5.49/bin/daisy.exe",ctrldaisy=Daisy.control(), ctrluser = NULL){
  
  control.user <- do.call(User.control, as.list(ctrluser)) 
  control.daisy <- do.call(Daisy.control, as.list(ctrldaisy))
  
  sensitivity <- control.daisy$sensitivity
  calib <- control.daisy$calib
  dflt <- control.daisy$dflt
  costfunction <- control.daisy$costfunction
  obs <- control.daisy$obs
  wdDir <- control.daisy$wdDir
  OutDir <- control.daisy$OutDir
  param_sens <- control.daisy$param_sens
  p.config <- control.daisy$p.config
  p <- control.daisy$p
  ind <- control.daisy$ind
  timeout <- control.daisy$timeout

    
  if(sensitivity==TRUE & calib ==TRUE & dflt==TRUE){
    stop("Default, Sensitivity and calibration are selected. Please select only one option.")
  }  
  else if(sensitivity==TRUE & calib ==TRUE){
    stop("Sensitivity and calibration are selected. Please select only one option.")
  }
  else if(dflt==TRUE & calib ==TRUE){
    stop("Both default and calibration are selected. Please select only one option.")
  }
  else if(sensitivity==TRUE & dflt ==TRUE){
    stop("Both Sensitivity and default are selected. Please select only one option.")
  }
  else if(sensitivity==TRUE){
    costfunction<-as.function(costfunction)
    MR<-costfunction(RunFile = RunFile, showLogFile = showLogFile, PathToDaisy = PathToDaisy, ctrluser = control.user, ctrldaisy = control.daisy)
    return(MR)
  }
  
  else if(calib ==TRUE){
    costfunction<-as.function(costfunction)
    DE<-costfunction(RunFile = RunFile, showLogFile = showLogFile, PathToDaisy = PathToDaisy, ctrluser = control.user, ctrldaisy = control.daisy)
    return(DE)
  }
  else if(dflt == TRUE){
    if(file.exists(p.config)==TRUE){p.config <- data.table::fread(p.config)}
    CheckParameters(p.config) #Here we check for any duplicates in the p.config file
    
    ind<-"default"
    
    p.config[, f.update(p.config, RunFile, wdDir, OutDir, sensitivity, calib, dflt, ind)]
    
    RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
                   "input",paste0(ind,"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    
    #run it
    tryCatch({
      dproc <- processx::run(command = PathToDaisy,
                             args = RunFile,
                             echo = showLogFile,
                             timeout = timeout,
                             error_on_status = TRUE,
                             stderr_to_stdout = TRUE)
      
      return(FALSE)
      
    }, error = function(e) {
      #Timed Out ?
      print("Timed out")
      return(TRUE)
      
    })
    
  }
  else {
    
    RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
                   "input",paste0(ind,"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    
    #run it
    tryCatch({
      dproc <- processx::run(command = PathToDaisy,
                             args = RunFile,
                             echo = showLogFile,
                             timeout = timeout,
                             error_on_status = TRUE,
                             stderr_to_stdout = TRUE)
      
      return(FALSE)
      
    }, error = function(e) {
      #Timed Out ?
      print("Timed out")
      return(TRUE)
      
    })
  }
}








