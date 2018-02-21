#'@title update Parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function reads the parameter settings
#'
#'@param p The parameter
#'@param p.config The new parameter value
#'
#'
#'@examples
#'\dontrun{
#'updateParameters(p= "", p.config="")
#'}
#' @export

#read parameter settings
updateParameters<- function(p, p.config) {
  CheckParameters(p.config)

  #   params <- fread(param.file)
  p.config$default=p
  # p.config[, f.update(p.config), by=c(file$from.file, file$to.file)]
  p.config[, f.update(p.config)]

  return("Input Files successfully updated")
}


