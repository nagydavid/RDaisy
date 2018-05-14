#'@title Function to check if there are no duplicates in the parameter file
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function checks the if there are no duplicates in the file with the new parameter values
#'
#'@param file The name of the file with the parameters that are used in the calibration.
#'
#'@examples
#'\dontrun{
#' CheckParameters(file)
#'}
#' @export


CheckParameters<-function(file){
  if(anyDuplicated(file$name)==0){
    return("No duplicates were found")
  }else{
    stop(paste("Note: ",gsub(pattern = "$",replacement = "",x=file$name[which(duplicated(file$name)==TRUE)],fixed=TRUE)," is duplicate",sep = ""))
  }
}
