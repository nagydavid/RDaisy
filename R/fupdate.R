#'@title Function to update the parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param p The name of the parameter that needs to be changed.
#'@param v The value to which the defined parameter has to change.
#'@param f The file in whioch the parameter is located.
#'@param t The
#'
#'@examples
#'\dontrun{
#'f.update <- function(p, v, f, t)
#'}
#' @export
#'

f.update <- function(p, v, f, t) {
  txt <- readLines(f)
  for (i in 1:length(p)) {
    txt <- gsub(p[i], v[i], txt, fixed = TRUE)
  }
  cat(txt, file = t, sep="\n")
}
