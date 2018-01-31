#'@title update Parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function reads the parameter settings
#'
#'@param p The parameter
#'@param p.config The new parameter value
#'@param v The value to which the defined parameter has to change.
#'@param f The file in whioch the parameter is located.
#'@param t The'
#'
#'@examples
#'\dontrun{
#'updateParameters(p= "", p.config="")
#'}
#' @export

#read parameter settings
updateParameters<- function(p, p.config,v,f,t) {
  #   params <- fread(param.file)
  p.config$default=p
  p.config[, f.update(p,v,f,t), by=c(f, t)]

  return("Done")
}
