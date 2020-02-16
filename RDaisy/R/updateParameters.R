#'@title update Parameters
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function reads the parameter settings
#'
#'@param p The vector of parameter values that will be used to update the input files
#'@param p.config This argument will provide the ma,es om the p-config data table. This argument can either be a file or a data table already loaded in the R environment.
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param wdDir working environment path
#'@param sensitivity Specify if you run Daisy for Morris
#'@param calib Specify if you run Daisy for DEOptim
#'@param dflt Specify if you run Daisy for default values
#'@param ind index
#'@param param_sens sensitive parameters
#'@param timeout time out parameter, if Daisy does not finish on time out, the process will be killed


#'
#'@examples
#'\dontrun{
#'updateParameters(p= "", p.config="")
#'}
#'@import data.table
#' @export

#read parameter settings
updateParameters<- function(p, p.config,RunFile,wdDir, sensitivity, calib, dflt, ind,param_sens,timeout) {
  if(file.exists(p.config)==TRUE){p.config <- data.table::fread(p.config)}
  CheckParameters(p.config) #Here we check for any duplicates in the p.config file
  RunFile=RunFile
 
  
  if(!is.null(param_sens)==TRUE){
    name <- p.config$name
    p.config[name %in% param_sens, ]$default<-p
    
  } else {
    
    p.config$default<-p
    
  }

  p.config[, f.update(file = p.config, RunFile = RunFile,wdDir = wdDir,sensitivity = sensitivity,calib =  calib, dflt = dflt,ind = ind)]
  
  return("Input Files successfully updated")
}