#'update_template_EV
#'
#' @description function to take in an the survey name and an excel file.  The excel 
#' file has parameters such as file paths, template paths, etc.
#'
#' @param SurveyName name of survey as in excel file
#' @param DirNameFile excel file name of paths
#' @param ni is an optional argument for which file to start with (default ni=1 is the first file).
#'
#' Excel file has the following columns (with header names)
#' Cal_File: calibration file name
#' Base_Path:
#' Orig_EV_Dir:
#' Raw_Dir
#' EK80_Cal_File
#' EK80_Raw_Dir
#'
#' @examples
#' DirNameFile="C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_Ek80_conv_updated/Directory Structure EK60 EK80 conversion updated.xlsx"
#' SurveyName='2019_US'
#' update_template_EV(SurveyName,DirNameFile, ni=1)
#' @export

update_template_EV <- function(Survey_Name, DirNameFile, ni=1,...) {
  ### This version is for filesets with both EK60 and EK80 files

  ### Uses COM objects to run Echoview
  ### Exports regions and lines from EV files
  ### Imports regions and lines into new EV files with new template

  #-----------------------------------------------------------------------------------------

  # required packages
  #require(RDCOMClient)
  #require(readxl)
  
  DirTable <- readxl::read_excel(DirNameFile)  #read in directory table
  DirTableSurvey<- subset(DirTable, Survey == SurveyName)
  DirTableSurvey[] <- lapply(DirTableSurvey, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

  # set relative working directory
  BasePath<-DirTableSurvey$Base_Path
  BaseJudgePath<-DirTableSurvey$Orig_EV_Dir
  setwd(BasePath)

  #Which echoview acoustic variable to use for line export
  LineExportVarName=c('Sv raw pings T2')


  ###############################################
  # INPUT #
  ###############################################


  # Location of original EV files
  EVdir<-BaseJudgePath

  # Location for files updated to Conversion template
  CONV_new_template <- DirTableSurvey$Post_EV_Dir
  suppressWarnings(dir.create(CONV_new_template))

  #tempate name and location
  template <- DirTableSurvey$Template_File

  # Name of the bottom line in original EV files
  EVbottom <- TRUE
  EVbottomname <- DirTableSurvey$Bottom_Name

  # Does the new template include a bottom line? What is it's name?
  bottomline <- TRUE
  bottomname <- DirTableSurvey$New_Bottom_Name

  #location of .raw or .ek60 files and EK80 files
  RAWdir<-DirTableSurvey$Raw_Dir
  RAWdir_EK80<-DirTableSurvey$EK80_Raw_Dir

  #location for Exports (things from original EV file that we want to keep)
  Exports <- "Exports"
  suppressWarnings(dir.create(file.path(CONV_new_template, Exports)))

  #location for region exports (things from original EV file that we want to keep)
  Reg <- "Regions"
  Regdir<-file.path(CONV_new_template, Reg)
  suppressWarnings(dir.create(Regdir))


  #location for line exports (things from original EV file that we want to keep)
  Line <- "Exports/Lines"
  Linedir<-file.path(CONV_new_template, Line)
  suppressWarnings(dir.create(Linedir))

  # list the EV files to run
  EVfile.list <- list.files(file.path(EVdir), pattern=".ev$", ignore.case = TRUE) #just the three ".ev" files!

  ###############################################
  #           Open EV file to update            #
  ###############################################

  nfiles=length(EVfile.list)  #number of EV files
  for (i in EVfile.list[ni:nfiles]){
    ni=which(EVfile.list==i)
    print(ni)

    # EV filename
    name <- sub(".ev","",i)
    EVfileName <- file.path(EVdir, i)
    print(EVfileName)

    # create COM connection between R and Echoview
    EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")
    EVApp$Minimize()  #Minimize EV file to run in background

    # open EV file
    EVfile <- EVApp$OpenFile(EVfileName)

#################
    #fileset 1 (0)
#################
    
    # Set fileset object for EK60
    filesetObj <- EVfile[["Filesets"]]$Item(0)

    # list raw files
    num <- filesetObj[["DataFiles"]]$Count()
    raws <- NULL
    for (l in 0:(num-1)){
      dataObj <- filesetObj[["DataFiles"]]$Item(l)
      dataPath <- dataObj$FileName()
      dataName <- sub(".*\\\\|.*/","",dataPath)
      raws <- c(raws,dataName)
    }

    #Get the raw file extension (should be "raw" or "ek60")
    rawfile_ext <- tools::file_ext(dataName)

    # set and check .ecs 
    calPath <- filesetObj$GetCalibrationFileName()
    calName <- sub(".*\\\\|.*/","",calPath)
    calFile <- filesetObj$GetCalibrationFileName()
    if(!calFile == DirTableSurvey$Cal_File){
        stop("Fileset1 .ecs file is wrong or missing", call. = FALSE)
    }

#############
#fileset 2 (1)
############
    # Set fileset object for EK80, if it exists
    nfilesets=EVfile[["Filesets"]]$Count()  #number of filesets
    if (nfilesets>1) {
      filesetObj <- EVfile[["Filesets"]]$Item(1)
      # list raw files
      num <- filesetObj[["DataFiles"]]$Count()
      raws_EK80 <- NULL
      for (l in 0:(num-1)){
        dataObj <- filesetObj[["DataFiles"]]$Item(l)
        dataPath <- dataObj$FileName()
        dataName <- sub(".*\\\\|.*/","",dataPath)
        raws_EK80 <- c(raws_EK80,dataName)
      }
  
      #Get the raw file extension (should be "raw" or "ek60")
      rawfile_ext_EK80 <- tools::file_ext(dataName)
  
      # set and check .ecs
      calPath_EK80 <- filesetObj$GetCalibrationFileName()
      calName_EK80 <- sub(".*\\\\|.*/","",calPath)
      calFile_EK80 <- filesetObj$GetCalibrationFileName()
      if(!calFile_EK80 == DirTableSurvey$EK80_Cal_File){
        stop("Fileset2 .ecs file is wrong or missing", call. = FALSE)
      }
    }
    
############
#### exports
############
    # export .evr file
    regionfilename <-  file.path(Regdir, paste(name, "evr", sep="."))     # filename
    EVfile[["Regions"]]$ExportDefinitionsAll(regionfilename)      # export

    # export bottom line
      name_sub=sub('\\.','_',name) #to remove the "." in the first three transect names so bottom line gets properly named and exported...
      if(EVbottom == TRUE){
      linesObj <- EVfile[["Lines"]]
      bottom <- linesObj$FindbyName(EVbottomname)
      bottomfilename <- file.path(Linedir, paste(name_sub, "bottom", "evl", sep="."))
      varObj <- EVfile[["Variables"]]
      LineExportVar = varObj$FindbyName(LineExportVarName)
      LineExportVar$ExportLine(bottom,bottomfilename,-1,-1)
    }

    EVApp$Quit()


  #####################################
  #          Make EV file             #
  #####################################

  # create COM connection between R and Echoview
  EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")

  # Open template EV file
  EVfile = EVApp$NewFile(file.path(template))  #open as a new file with a template

  # Set fileset object for EK60
  filesetObj <- EVfile[["Filesets"]]$Item(0)

  # Set calibration file

  if(!calPath == ""){

      add.calibration <- filesetObj$SetCalibrationFile(calFile)
        } else {
          stop(".ecs file is wrong or missing", call. = FALSE)
  }


  # Add raw files, either with .raw or .ek60 extension; if no raw data located, error

  ###########
  #Fileset 1 (0)
  ##########
  
  #For EK60
  for (r in raws){
      if(rawfile_ext == "ek60") {
      filesetObj[["DataFiles"]]$Add(file.path(EK6dir,r))
      }  else if(rawfile_ext == "raw") {
      filesetObj[["DataFiles"]]$Add(file.path(RAWdir,r))
      }  else {
      stop("No raw data found", call. = FALSE)
        }
      }

  #Check to make sure data actually loaded into EV file!!
    if (filesetObj[["DataFiles"]]$Count() == 0){
      stop("No EK60 raw data found", call. = FALSE)
    }

  ###########
  #Fileset 2 (1)
  ##########
  
  nfilesets=EVfile[["Filesets"]]$Count()  #number of filesets
  if (nfilesets>1) {
    # Set fileset object for EK80, if it exists
    filesetObj <- EVfile[["Filesets"]]$Item(1)
  
    #For EK80
    for (r in raws_EK80){
      if(rawfile_ext_EK80 == "ek60") {
        filesetObj[["DataFiles"]]$Add(file.path(EK6dir,r))
      }  else if(rawfile_ext_EK80 == "raw") {
        filesetObj[["DataFiles"]]$Add(file.path(RAWdir_EK80,r))
      }  else {
        stop("No EK80 raw data found", call. = FALSE)
      }
    }
  
    #Check to make sure data actually loaded into EV file!!
    if (filesetObj[["DataFiles"]]$Count() == 0){
      stop("No raw data found", call. = FALSE)
    }
  }

  # Add regions
  EVfile$Import(regionfilename)

  # number of editable lines in template
  ls <- NULL
  linesObj <- EVfile[["Lines"]]
  for(k in 0:(linesObj$Count()-1)){
    tmp <- linesObj$Item(k)
    linedit <- tmp$AsLineEditable()
    ls <- c(ls,linedit)
  }
  linenum <- length(ls)

  # Add bottom line and overwrite template bottom line if it exists
  if(EVfile$Import(bottomfilename)==FALSE){
    print("bottom file did not import correctly")
  }
  #bottom <- linesObj$FindbyName(paste0("Line",linenum+1))
  #this doesn't work anymore, because it's not coming in as the name of the next line
  bottom<-linesObj$FindbyName(name_sub)  #Add this instead - looks like the name of the line comes from the filename
  linenum <- linenum + 1
  if(bottomline == TRUE){
    oldbottom <- linesObj$FindbyName(bottomname)
    oldbottom$OverwriteWith(bottom)
    linesObj$Delete(bottom)
  } else if(bottomline == FALSE){
    bottom[["Name"]] <- "Bottom"
  }

  #Update export parameters
  Obj_ExpVar<-EVfile[['Properties']][['Export']][['Variables']]  #set export variables object
  SetBiomassExpParamsFun(Obj_ExpVar)  #update the export parameters to match standard biomass exports

  # Save EV file
  EVfile$SaveAS(file.path(CONV_new_template,i))

  # Close EV file
  EVApp$CloseFile(EVfile)

  # Quit echoview
  EVApp$Quit()

    }
}
