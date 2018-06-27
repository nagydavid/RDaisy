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
#'@param interval is the interval of the calibration period, in our case the from 1992 and 1993 drainage year.
#'@param year if is TRUE than user gets the all output objectives of the years, if it FALSE, it gives the mean of all calculated annual objective.
#'@param All If TRUE Giving all performance measure, if FALSE giving the specified performance measure in the script
#'@param param_sens sensitive parameters
#'@param p.config This argument will provide the makes om the p-config data table. This argument can either be a file or a data table already loaded in the R environment.
#'@param ind index
#'@param p parameters

#'@export





Daisy.control <- function(sensitivity=FALSE,calib=FALSE,dflt=FALSE,costfunction=NULL,obs=NULL,wdDir=NULL,OutDir=NULL,interval=NULL,year=NULL, All=NULL,param_sens=NULL,p.config=NULL,ind=NULL,p=NULL){
  
end=list(sensitivity=sensitivity,calib=calib,dflt=dflt,costfunction=costfunction,obs=obs,wdDir=wdDir,OutDir=OutDir,interval=interval,year=year, All=All,param_sens=param_sens,p.config=p.config,ind=ind,p=p)
return(end)
}
