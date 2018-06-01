#'@title update Parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function reads the parameter settings
#'
#'@param p The vector of parameter values that will be used to update the input files
#'@param p.config This argument will provide the ma,es om the p-config data table. This argument can either be a file or a data table already loaded in the R environment.
#'@param DEoptim Specify if you run Daisy for DEOptim
#'
#'
#'@examples
#'\dontrun{
#'updateParameters(p= "", p.config="")
#'}
#'@import data.table
#' @export

#read parameter settings
updateParameters<- function(p, p.config,DEoptim=F) {
  if(file.exists(p.config)==TRUE){p.config <- data.table::fread(p.config)}
  CheckParameters(p.config) #Here we check for any duplicates in the p.config file

  #   params <- fread(param.file)
  p.config$default<-p
  # p.config[, f.update(p.config), by=c(file$from.file, file$to.file)]
  p.config[, f.update(p.config,DEoptim=F)]

  return("Input Files successfully updated")
}


