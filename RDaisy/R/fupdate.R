#'@title Function to update the parameters
#'
#'@author JWM Pullens, M Jabloun, D Nagy
#'
#'@description This function updates the parameter file.
#'
#'@param file The name of the file with the parameters to be calibrated
#'@param RunFile The complete path to the file with extension ".dai". This file is the setup file of the model.
#'@param sensitivity Specify if you run Daisy for Morris
#'@param calib Specify if you run Daisy for DEOptim
#'@param wdDir working environment path
#'@param OutDir  output path
#'@param dflt Specify if you run Daisy for default values
#'@param ind Index (only use internally for Morris function)
#'
#'@examples
#'\dontrun{
#'f.update(file)
#'}
#'@import data.table processx stringr
# This function is not exported and it will not be visible for the users. Teh function is internally used in updateParameters.R
#' @export

f.update <- function(file,RunFile,wdDir,OutDir,sensitivity,calib,dflt,ind){
  index="$ind"
  wdDir_txt="$wdDir"
  OutDir_txt="$OutDir"
  
  if(dir.exists(file.path(wdDir,"input"))==F){dir.create(file.path(wdDir,"input"))}
  
  file$to.file <- paste0("input/",ind,"_",file[,strsplit(file$to.file,"/")][2])
  
  for (i in 1:length(file$name)){
    
    if(i>1){
      if(file$from.file[i]==file$from.file[i-1]){
        txt <- readLines(file$to.file[i],warn=FALSE)
        txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
        if(ncol(file)>7){
          if(file$exp.pm[i]==1){
            y<-0
            for(j in 1:length(file$name)){
              file$exp[i] <- gsub(file$name[j],file$default[j],file$exp[i],fixed = TRUE)
            }
            y<-eval(parse(text=file$exp[i]))
            
            txt <- gsub(file$name.exp[i], y, txt, fixed = TRUE)
          }
        }  
        cat(txt, file = file$to.file[i], sep="\n")
      } else {
        txt <- readLines(file$from.file[i],warn=FALSE)
        txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
        cat(txt, file = file$to.file[i], sep="\n")}
    } else {
      txt <- readLines(file$from.file[i],warn=FALSE)
      txt <- gsub(file$name[i], file$default[i], txt, fixed = TRUE)
      cat(txt, file = file$to.file[i], sep="\n")}
  }
  
  txt2 <- readLines(RunFile,warn=FALSE)
  txt2 <- gsub(index, ind, txt2, fixed = TRUE)
  
  for (j in 1:length(unique(file$to.file))){
    txt2 <- gsub(unique(file$origin)[j], unique(file$to.file)[j], txt2, fixed = TRUE)}
  
  txt2 <- gsub(wdDir_txt, wdDir, txt2, fixed = TRUE)
  
  if(dir.exists(file.path(wdDir,OutDir))==F){dir.create(file.path(wdDir,"output"), showWarnings = FALSE)}
  if(dir.exists(file.path(wdDir,OutDir))==F){dir.create(file.path(wdDir,OutDir), showWarnings = FALSE)}
  txt2 <- gsub(OutDir_txt, paste0(file.path(wdDir,OutDir),"/"), txt2, fixed = TRUE)
  
  cat(txt2, file = paste(paste(strsplit(RunFile,"/")[[1]][1:length(strsplit(RunFile,"/")[[1]])-1],collapse ="/"),
                         "input",
                         paste0(ind,"_",stringr::str_sub(strsplit(RunFile,"/")[[1]][length(strsplit(RunFile,"/")[[1]])],0,-5),"_opt.dai"),sep = "/"),
      sep="\n")
}