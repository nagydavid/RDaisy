#'@title Function to update the parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function runs the Daisy model.
#'
#'@param file The name of the parameter that needs to be changed.
#'
#'@examples
#'\dontrun{
#'f.update <- function(file)
#'}
#'
#'
#'
# This function is not exported and is will not be visible for the users. Teh function is internally used in updateParameters.R
f.update <- function(file){


  for (i in 1:length(file$name)) {
    if(i>1){
    txt <- readLines(file$to.file[i])
    txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
    cat(txt, file = file$to.file[i], sep="\n")}else{
    txt <- readLines(file$from.file[i])
    txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
    cat(txt, file = file$to.file[i], sep="\n")}
  }

}
