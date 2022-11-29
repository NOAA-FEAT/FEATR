#' createlog:  Using a single or multiple survey/legs, open the EV file, export the region log, and concatenate 
#' the logs into a single file called CruiseLog.csv
#'
#' @description Needs RDCOMclient, readxl
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files
#' @param exportloc is an optional argument for using the Export_Dir or the Region_Export_dir column
#' entry in the DirNameFile spreadsheet.  1 is Export_Dir, and 0 is Region_Export_dir
#'
#' @examples SurveyName='2022_LEG1LOG'
#' SurveyName=c('2022_LEG1LOG', '2022_LEG2LOG')
#' DirNameFile='C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_EK80_conv_updated/Directory Structure EK60 EK80 conversion updated.xlsx'
#' createlog(SurveyName,DirNameFile)
#'
#'
#' @export

createlog<-function(SurveyName, DirNameFile){

DirTable <- readxl::read_excel(DirNameFile)  #load in directories/paths/ltocations
DirTable_yr<- subset(DirTable, Survey %in% SurveyName) #just year(s) of interest
DirTable_yr[] <- lapply(DirTable_yr, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

     
###############################################
#          Set Paths                          #
###############################################
Exportbase <- DirTable_yr$Export_Dir
EVdir<-DirTable_yr$Orig_EV_Dir
BaseDatadirSH<-DirTable_yr$Raw_Dir
EVdirs<-EVdir

#Country_export_dir<-c("US","CAN")
Country_export_dir<-c("","")  #put all into same directory
date_exportdir<-format(Sys.time(),"%Y%m%d")
Export<-file.path(Exportbase,Country_export_dir, paste("Export",date_exportdir,sep=""))


#start country (or directory/leg) loop
for (k in seq(length(EVdir))){
dir.create(Export[k], showWarnings = FALSE)
###############################################
#          Load in EV file names              #
###############################################

# list the EV files
EVfile.list <- list.files(file.path(EVdirs[k]), pattern="*.EV")


###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")

################################################
#             Create cruise log                #
################################################


## ------------- start loop


  for (i in EVfile.list){
    EVfileName <- file.path(EVdirs[k], i)
  
  # open EV file
  EVfile <- EVApp$OpenFile(EVfileName)
  print(i)
  
  #set EvVariable object
  evVarObj <- EVfile[["Variables"]]
  
  #get 'Sv raw pings T1' in EvVariableAcoustic 
  EvVar <- evVarObj$FindByName("Sv pings T1")$AsVariableAcoustic()
  
  #export log book
  logName <- strsplit(i, split = '*.EV')[[1]]
  logFileName <- file.path(Export[k], paste(logName , ".csv", sep=""))
  #Originally tried explorting a log class, which works for US, but in CAN, is type marker, and class is BT, ET, etc
  EvVar$ExportRegionsLogAll(logFileName)
  EVApp$CloseFile(EVfile)   #close EV file
  
  
  ## ------------- end loop
  }
  
  
  ##########################################
  #quit echoview
  EVApp$Quit()
  
  
  
  
  
  #### ------ If there are more EV files in another folder -> Run again with EV folder change 
  
  
  
  
  
  ################################################
  #           Combine all .csv logs              #
  ################################################
  
  unlink(file.path(Export[k], "CruiseLog.csv"))   #deletes old cruise log
  
  log.list <- list.files(file.path(Export[k]), pattern="*.csv")
  
  df <- NULL
  for (i in log.list){
    d <- read.csv(file.path(Export[k], i), header = T)
    if (nrow(d)>0){
      a <- d
      a$File <- i
    df <- rbind(df,a)
    }
  }
  
  write.csv(df, file = file.path(Export[k],  "CruiseLog.csv"))
  
} 

}
