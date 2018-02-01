#'@title Function to update the parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param p The name of the parameter that needs to be changed.
#'@param v The value to which the defined parameter has to change.
#'@param f The file in which the parameter is located.
#'@param t The file to which the parameter is stored.
#'
#'@examples
#'\dontrun{
#'f.update <- function(p, v, f, t)
#'}
#'
#'
#'
# This function is not exported and is will not be visible for the users. Teh function is internally used in updateParameters.R
f.update <- function(p, v, f, t) {
  txt <- readLines(f)
  for (i in 1:length(p)) {
    txt <- gsub(p[i], v[i], txt, fixed = TRUE)
  }
  cat(txt, file = t, sep="\n")
}
