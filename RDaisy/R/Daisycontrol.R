#'@title Control various aspects of the RDAISY implementation
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description Allow the user to set some characteristics of the functions in RDaisy, such as calibration and sensitivity analysis
#'
#'
#'@param sensitivity Specify if a sensitivity analylis is needed
#'@param calib Specify if Optimization/calibration is needed.
#'@param dflt Specify if you run Daisy for default values
#'@param costfunction read.optim (needs explination)
#'@param obs  observation data with date and measurement, same dimension of the simulation
#'@param wdDir working environment path
#'@param OutDir  output path
#'@param param_sens sensitive parameters
#'@param p.config This argument will provide the makes om the p-config data table. This argument can either be a file or a data table already loaded in the R environment.
#'@param ind index
#'@param p parameters
#'@param timeout time out parameter, if Daisy does not finish on time out, the process will be killed
#'@export





Daisy.control <- function(sensitivity=FALSE,calib=FALSE,dflt=FALSE,costfunction=NULL,obs=NULL,wdDir=NULL,OutDir=NULL,param_sens=NULL,p.config=NULL,ind=NULL,p=NULL,timeout=Inf){
  
end=list(sensitivity=sensitivity,calib=calib,dflt=dflt,costfunction=costfunction,obs=obs,wdDir=wdDir,
         OutDir=OutDir,param_sens=param_sens,p.config=p.config,ind=ind,p=p,timeout=timeout)
return(end)
}
