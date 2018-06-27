#'@title Cost function
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function updates the parameters, runs the model with the new parameters and calculates the cost/fit.
#'
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param showLogFile Either True or False to show the log file. (default = FALSE)
#'@param PathToDaisy Specify the path to the Daisy executable
#'@param ctrldaisy Extra arguments that are needed for Optimization/Calibration and Sensitivity analysis (see  \code{\link{Daisy.control}})
#'
#'@examples
#'\dontrun{
#'f.cost(p, p.config, RunFile,showLogFile,PathToDaisy,Morris,DEoptim,
#'dflt,costfunction,obs,wdDir,OutDir,interval,year,All,ind,param_sens)
#'}
#' @export

f.cost <- function(RunFile,showLogFile,PathToDaisy,ctrldaisy){
  sub <- do.call(Daisy.control, as.list(ctrldaisy))
  updateParameters(p = sub$p, p.config = sub$p.config,RunFile = RunFile,wdDir = sub$wdDir, sensitivity = sub$sensitivity, calib = sub$calib,dflt = sub$dflt, ind = sub$ind,param_sens = sub$param_sens)
  #2. Run Daisy with the new input files
  runDaisy(RunFile,showLogFile ,PathToDaisy,ctrldaisy = Daisy.control(sensitivity=F,
                                                                    calib=F,
                                                                    dflt=F,
                                                                    costfunction=sub$costfunction,
                                                                    obs = sub$obs,
                                                                    wdDir = sub$wdDir,
                                                                    OutDir = sub$OutDir,
                                                                    interval=sub$interval,
                                                                    year=sub$year,
                                                                    All=sub$All,
                                                                    param_sens = sub$param_sens,
                                                                    p.config = sub$p.config,
                                                                    ind = sub$ind))
}