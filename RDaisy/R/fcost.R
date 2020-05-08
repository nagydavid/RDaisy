#'@title Cost function
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function updates the parameters, runs the model with the new parameters and calculates the cost/fit.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Specify the path to the Daisy executable
#'@param ctrluser Extra arguments that are needed for user designed Cost function (see  \code{\link{User.control}}. Do not add it when only a single simple run is neded)
#'@param ctrldaisy Extra arguments that are needed for Optimization/Calibration and Sensitivity analysis (see  \code{\link{Daisy.control}})
#'
#'@examples
#'\dontrun{
#'f.cost(p, p.config, RunFile,showLogFile,PathToDaisy,Morris,DEoptim,
#'dflt,costfunction,obs,wdDir,OutDir,interval,year,All,ind,param_sens,timeout)
#'}
#' @export

f.cost <- function(RunFile, showLogFile, PathToDaisy, ctrluser = NULL, ctrldaisy){


  updateParameters(p = ctrldaisy$p,
                   p.config = ctrldaisy$p.config,
                   RunFile = RunFile,
                   wdDir = ctrldaisy$wdDir, 
                   sensitivity = ctrldaisy$sensitivity,
                   calib = ctrldaisy$calib,
                   dflt = ctrldaisy$dflt, 
                   ind = ctrldaisy$ind,
                   param_sens = ctrldaisy$param_sens,
                   timeout = ctrldaisy$timeout)
  
  #2. Run Daisy with the new input files
  runDaisy(RunFile,
           showLogFile, 
           PathToDaisy,
           ctrluser = ctrluser,
           ctrldaisy = Daisy.control(sensitivity=F,
                                     calib=F,
                                     dflt = F,
                                     costfunction=NULL,
                                     obs = ctrldaisy$obs,
                                     wdDir = ctrldaisy$wdDir,
                                     OutDir = ctrldaisy$OutDir,
                                     param_sens = ctrldaisy$param_sens,
                                     p.config = ctrldaisy$p.config,
                                     ind = ctrldaisy$ind,
                                     timeout = ctrldaisy$timeout))
}