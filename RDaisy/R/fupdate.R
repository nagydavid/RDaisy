#'@title Function to update the parameters
#'
#'@author JWM Pullens, M Jabloun
#'
#'@description This function updates the parameter file.
#'
#'@param file The name of the file with the parameters to be calibrated
#'@param DEO Specify if you run Daisy for DEOptim
#'@param ind Index (only use internally for Morris function)
#'
#'@examples
#'\dontrun{
#'f.update(file)
#'}
#'
# This function is not exported and it will not be visible for the users. Teh function is internally used in updateParameters.R

f.update <- function(file,DEO,ind=NULL){
  index="$ind"

  for (i in 1:length(file$name)){
    if(i>1){
    txt <- readLines(file$to.file[i])
    txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
    if(DEO==T){
    txt <- gsub(index, Sys.getpid(), txt, fixed = TRUE)}else{
    txt <- gsub(index, ind, txt, fixed = TRUE)
    }
    cat(txt, file = file$to.file[i], sep="\n")}else{
    txt <- readLines(file$from.file[i])
    txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
    cat(txt, file = file$to.file[i], sep="\n")}
  }

}
