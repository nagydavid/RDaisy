#' @title Function to copy internal data and R-scripts to user specified folder
#' @description
#' This function copies the internal dataand R-scripts from the R package to a user-defined folder. This is necesarry since the model does not read the data from R.
#'
#' The model needs to be run in a seperate folder.
#'
#' If the specified folder does not exist, the function will create it at the user defined loaction. If the packages are installed on default path, then the package_folder argument can be kept empty. If not, the user has to provide the path where the R package is installed.
#'
#'
#' The test data is plot 2 from: Leibniz-Zentrum für Agrarlandschaftsforschung (ZALF) e.V.
#' Freely available: http://dx.doi.org/10.4228/ZALF.1992.167
#' Mirschel, Wilfried; Wenkel, Karl Otto; Wegehenkel, Martin; Kersebaum, Kurt Christian; Schindler, Uwe( 2010): Comprehensive multivariable field data set for agro-ecosystem modelling from Muencheberg Experimental Stations in 1992 - 1998 ,Leibniz-Zentrum für Agrarlandschaftsforschung(ZALF e.V.).[doi: 10.4228/ZALF.1992.167]
#'
#' The R-scripts, "Morris.R", "DEoptim.R" and "run.R" are used in the manuscript and can be used as examples and/ or templates.
#'
#' @author JWM Pullens, D Nagy
#'
#' @usage copyinternaldata(new_folder,package_folder=NULL)
#' @param new_folder Folder to where the data and R-scripts needs to be copied
#' @param package_folder Folder where the R package is installed, if this is not specified during installation leave this empty.
#'
#' @examples
#' \dontrun{
#'  for Windows:
#'    copyinternaldata(new_folder="C:/testdata/",package_folder=NULL)
#'
#'  for Linux:
#'    copyinternaldata(new_folder="~/testdata/",package_folder=NULL)
#' }
#'@export

copyinternaldata<-function(new_folder,package_folder=NULL){
  #define path where the files/folders should be copied to
  if(dir.exists(new_folder)==FALSE){dir.create(new_folder)}
  if(is.null(package_folder)){
    package_folder <- paste(.libPaths()[1],"/RDaisy/extdata/",sep="")
    a <- list.files(package_folder)
    file.copy(file.path(package_folder,a),new_folder)
  }else{
    a <- list.files(package_folder)
    file.copy(file.path(package_folder,a),new_folder)
  }
  print(paste("Folder structure succefully copied to ",new_folder))

  dir.create(paste(new_folder,"/output",sep=""))
  print(paste("Created an output folder in the folder",new_folder))
}

