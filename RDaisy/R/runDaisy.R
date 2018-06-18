#'@title Function to run the Daisy model
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function runs the Daisy model.
#'
#'@param p.config This argument will provide the ma,es om the p-config data table. This argument can either be a file or a data table already loaded in the R environment.
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Define where Daisy is installed. NOTE: This user should have admin rights to this folder.
#'@param Morris Specify if Morris sensitivity analylis is needed. (from package sensitivity)
#'@param DEoptim Specify if Deoptimizaltion is needed. (from package DEoptim)
#'@param dflt Specify if you run Daisy for default values
#'@param costfunction read.optim (needs explination)
#'@param obs  observation data with date and measurement, same dimension of the simulation
#'@param wdDir working environment path
#'@param OutDir  output path
#'@param interval is the interval of the calibration period, in our case the from 1992 and 1993 drainage year.
#'@param year if is TRUE than user gets the all output objectives of the years, if it FALSE, it gives the mean of all calculated annual objective.
#'@param All If TRUE Giving all performance measure, if FALSE giving the specified performance measure in the script
#'@param param_sens sensitive parameters
#'
#'
#'@examples
#'\dontrun{
#'runfile="C:/Daisy 5.31/sample/test.dai"
#'PathToDaisy = "C:/Daisy 5.31/bin/daisy.exe"
#'runDaisy(RunFile=RunFile, showLogFile = FALSE, PathToDaisy = PathToDaisy)
#'}
#'@import data.table
#'@import stringr
#' @export

runDaisy <- function(p.config,RunFile, showLogFile = TRUE, PathToDaisy = "C:/Program Files/Daisy 5.49/bin/daisy.exe",Morris,DEoptim,dflt,
                     costfunction,obs,wdDir,OutDir,interval,year, All,param_sens){
  obs=obs
  wdDir=wdDir
  OutDir=OutDir
  interval=interval
  year=year
  All=All
  
  
  if(Morris==TRUE & DEoptim ==TRUE & dflt==TRUE){
    stop("Default, Morris and DEoptim are selected. Please select only one option.")
  }  
  if(Morris==TRUE & DEoptim ==TRUE){
    stop("Both Morris and DEoptim are selected. Please select only one option.")
  }
  if(dflt==TRUE & DEoptim ==TRUE){
    stop("Both default and DEoptim are selected. Please select only one option.")
  }
  if(Morris==TRUE & dflt ==TRUE){
    stop("Both Morris and default are selected. Please select only one option.")
  }
  if(Morris==TRUE){
    costfunction<-as.function(costfunction)
    MR<-costfunction(p.config,RunFile,showLogFile,PathToDaisy,Morris, DEoptim, dflt,obs,wdDir, OutDir, interval,year,All)
    return(MR)
  }
  
  if(DEoptim ==TRUE){
    costfunction<-as.function(costfunction)
    DE<-costfunction(p.config,RunFile,showLogFile,PathToDaisy,Morris, DEoptim,dflt,obs,wdDir, OutDir, interval,year,All,param_sens)
    return(DE)
  }
  if(dflt == TRUE){
    if(file.exists(p.config)==TRUE){p.config <- data.table::fread(p.config)}
    CheckParameters(p.config) #Here we check for any duplicates in the p.config file
    
    ind<-NULL
    p.config[, f.update(p.config, RunFile,wdDir,Morris, DEoptim,dflt,ind)]
    
    RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
                   "input",paste0(Sys.getpid(),"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    
    #build the command
    cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
    #run it
    system(cmdToRun, show.output.on.console = showLogFile  )
  }else{  
    
    RunFile<-paste(paste((strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1]),collapse ="/"),
                   "input",paste0(Sys.getpid(),"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/")
    #build the command
    cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
    #run it
    system(cmdToRun, show.output.on.console = showLogFile  )}
}