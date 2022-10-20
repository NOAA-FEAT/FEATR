#' @title read_EV_exports_compile: Read and append certain columns from multiple
#' variable Echoview exports
#' 
#' @description  Reads variables from multiple EV files.  The columns from the 
#' first variable are completely kep.  The requested columns (CompCols) from the rest of the
#' variables are appended onto the end of the dataframe.  Usually for exports from
#' multiple EV files.  Reads in multiple 
#' variables exported  from each EV file.   
#' Example columns are Sv_mean and PRC_NASC. 
#' 
#' @param variables - variables to read in (subdirectories need to be named as variables)
#' @param ComCols - the columns to be kept from the non-primary variable and appended 
#' to the end of the data.
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files
#' 
#' @example temp2<-read_EV_exports_compile(variables,CompCols=c("Sv_mean","PRC_NASC"),SurveyName, DirNameFile)
#' 
#' @export


read_EV_exports_compile<-function(variables,CompCols,SurveyName, DirNameFile){
  #Sub-Function that extracts specific columns from tables that are read in
  extractcols<-function(EKdatatemp){
    Ekdataout<-select(EKdatatemp,dplyr::all_of(CompCols))
  }
  
  nvars=length(variables)  #number of variables
  #Read in first variable for full data set
  EKDatainit=read_EV_exports(variables[1],SurveyName,DirNameFile) #read in first case.  
    if (nvars>2){  #if have more than one variable, read in the rest, appending the requested columns
      for (k in 2:length(variables)){
        EKdatatemp <-read_EV_exports(variables[k],SurveyName,DirNameFile)
        EKDatainit<-cbind(EKDatainit,extractcols(EKdatatemp))
      }
  }
  EKDatainit
}

