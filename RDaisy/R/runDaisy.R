#'@title Function to run the Daisy model
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Define where Daisy is installed. NOTE: This user should have admin rights to this folder.
#'
#'@examples
#'\dontrun{
#'runfile="C:/Daisy 5.31/sample/test.dai"
#'PathToDaisy = "C:/Daisy 5.31/bin/daisy.exe"
#'runDaisy(RunFile=RunFile, showLogFile = FALSE, PathToDaisy = PathToDaisy)
#'}
#' @export

runDaisy <- function(RunFile, showLogFile = FALSE, PathToDaisy = "C:/Program Files/Daisy 5.49/bin/daisy.exe"){
  #build the command
  cmdToRun <- paste("\"", PathToDaisy, "\"", " \"", RunFile, "\"", sep="")
  #run it
  system(cmdToRun, show.output.on.console = showLogFile  )
}
