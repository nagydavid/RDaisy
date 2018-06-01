#'@title Function to run the Daisy model
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Define where Daisy is installed. NOTE: This user should have admin rights to this folder.
#'@param Morris Specify if Morris sensitivity analylis is needed. (from package sensitivity)
#'@param DEoptim Specify if Deoptimizaltion is needed. (from package DEoptim)
#'@param costfunction read.optim (needs explination)
#'@param ind index file, to decide which output to read
#'@param obs  observation data with date and measurement, same dimension of the simulation
#'@param wdDir working environment path
#'@param OutDir  output path
#'@param interval is the interval of the calibration period, in our case the from 1992 and 1993 drainage year.
#'@param year if is TRUE than user gets the all output objectives of the years, if it FALSE, it gives the mean of all calculated annual objective.
#
#'
#'
#'
#'@examples
#'\dontrun{
#'runfile="C:/Daisy 5.31/sample/test.dai"
#'PathToDaisy = "C:/Daisy 5.31/bin/daisy.exe"
#'runDaisy(RunFile=RunFile, showLogFile = FALSE, PathToDaisy = PathToDaisy)
#'}
#' @export

runDaisy <- function(RunFile, showLogFile = FALSE, PathToDaisy = "C:/Program Files/Daisy 5.49/bin/daisy.exe",Morris=FALSE,DEoptim=FALSE,
            costfunction,obs,wdDir,OutDir,interval,year){
  obs=obs
  wdDir=wdDir
  OutDir=OutDir
  interval=interval
  year=year

  if(Morris==TRUE & DEoptim ==TRUE){
    stop("Both Morris and DEoptim are selected. Please select only one option.")
  }
  if(Morris==TRUE){
    source(costfunction)
  }

  if(DEoptim ==TRUE){
    source(costfunction)
  }


  #build the command
  cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
  #run it
  system(cmdToRun, show.output.on.console = showLogFile  )
}
