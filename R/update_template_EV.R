

update_template_EV <- function(Survey_Name, DirNameFile, ni=1,...) {
  ### This version is for filesets with both EK60 and EK80 files
  
  ### Uses COM objects to run Echoview
  ### Exports regions and lines from EV files
  ### Imports regions and lines into new EV files with new template
  
  #-----------------------------------------------------------------------------------------
  #3/11/2021 Added extra to set transducer depth
  
  # required packages
  #require(RDCOMClient)
  #require(readxl)
  
  #DirNameFile="C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_Ek80_conv_updated/Directory Structure EK60 EK80 conversion updated.xlsx"
  #DirTable <- readxl::read_excel(DirNameFile)
  ####put in copy .ecs file to new directory
  
  #SurveyName='2019_US'
  SurveyName='2018_US'
  #Below 2 lines modified from Beth's updated code
  DirTableSurvey<- subset(DirTable, Survey == SurveyName)
  DirTableSurvey[] <- lapply(DirTableSurvey, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset
  
  # set relative workin-160g directory
  #setwd('..');setwd('..')
  BasePath<-DirTableSurvey$Base_Path
  BaseJudgePath<-DirTableSurvey$Orig_EV_Dir
  #BaseProjPath<-"N:/Survey.Acoustics/Projects & Analysis/Euphausiids/EUPH"
  setwd(BasePath)
  
  #Which echoview acoustic variable to use for line export
  LineExportVarName=c('Sv raw pings T2') 
  
  #FilePaths <- read.csv("E:/Survey.Acoustics/Projects & Analysis/Euphausiids/EUPH documents/EUPH_paths.csv") #This file should have most updated file.paths for all directories
  #FilePaths <- subset(FilePaths, SurveyName == surveyName)
  #FilePaths[] <- lapply(FilePaths, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset
  
  #ecsfile_1_43 <- sapply(strsplit(as.character(FilePaths$CalFile),","), "[",1) #since there are two .ecs files listed (separated by comma), need to get correct one out
  #ecsfile_44_77 <- sapply(strsplit(as.character(FilePaths$CalFile),","), "[",2)
  #ecsfile_44_77 <- trimws(ecsfile_44_77) #trim leading whitespace after comma
  
  #ecsfile_44_77 <- "Hake2009_MF_x44-77.ecs"
  
  ###############################################
  # INPUT #
  ###############################################
  
  
  # Location of original EV files
  #EVdir<-"copy of FINAL for biomass"
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
  
  ###############################################
  
  
  
  
  ###################################################
  # Locate #
  
  #location of calibration file (.ecs)
  
  #CALdir<-"Judging"  #From BaseYearPath
  
  #location of .raw or .ek60 files and EK80 files
  RAWdir<-DirTableSurvey$Raw_Dir
  RAWdir_EK80<-DirTableSurvey$EK80_Raw_Dir
  #RAWdir<-"Data_MF/Acoustics/EK60_raw"  #From BaseYearPath
  #EK6dir
  
  #location for Exports
  Exports <- "Exports"
  suppressWarnings(dir.create(file.path(CONV_new_template, Exports)))
  
  #location for region exports
  Reg <- "Regions"
  Regdir<-file.path(CONV_new_template, Reg)
  suppressWarnings(dir.create(Regdir))
  
  
  #location for line exports
  Line <- "Exports/Lines"
  Linedir<-file.path(CONV_new_template, Line)
  suppressWarnings(dir.create(Linedir))
  
  #location for marker region exports
  #Marks<-"EUPH_new_template/Exports/Markers"
  
  #########################
  # list the EV files to run
  #EVfile.list <- list.files(file.path(getwd(),EVdir), pattern=".EV$", ignore.case = TRUE)
  EVfile.list <- list.files(file.path(EVdir), pattern=".ev$", ignore.case = TRUE) #just the three ".ev" files!
  
  ###############################################
  #           Open EV file to update            #
  ###############################################
  #ni=44
  nfiles=length(EVfile.list)  #number of EV files
  for (i in EVfile.list[ni:nfiles]){
    #for (i in EVfile.list){
    #for (i in EVfile.list[1:length(EVfile.list)]){
    #for (i in EVfile.list[vec]){ #if need to run ni=just part of EVfile list - as partial loop and conintue above
    #for (i in EVfile.list[-1*seq(1:ni)]){ #if need to run just part of EVfile list in a single loop
    ni=which(EVfile.list==i)
    print(ni)    
  
    # EV filename
    name <- sub(".ev","",i)
    #EVfileName <- file.path(getwd(),EVdir, i)
    EVfileName <- file.path(EVdir, i)
    print(EVfileName)
  
    # create COM connection between R and Echoview
    EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")
    EVApp$Minimize()  #Minimize EV file to run in background
  
    # open EV file
    EVfile <- EVApp$OpenFile(EVfileName)
  
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
  
    # get .ecs filename
    calPath <- filesetObj$GetCalibrationFileName()
    calName <- sub(".*\\\\|.*/","",calPath)
    calFile <- filesetObj$GetCalibrationFileName()
  
    #check it is correct .ecs file
  
  #  for(EVfile.list[44:74]){
       if(!calFile == DirTableSurvey$Cal_File){
          stop(".ecs file is wrong or missing", call. = FALSE)
        }
  
  
    # Set fileset object for EK80
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
    
    # get .ecs filename
    calPath_EK80 <- filesetObj$GetCalibrationFileName()
    calName_EK80 <- sub(".*\\\\|.*/","",calPath)
    calFile_EK80 <- filesetObj$GetCalibrationFileName()
    
    #check it is correct .ecs file for EK60
    
    #  for(EVfile.list[44:74]){
    if(!calFile == DirTableSurvey$Cal_File){
      stop("EK60 .ecs file is wrong or missing", call. = FALSE)
    }
    
    #  for(EVfile.list[44:74]){
    if(!calFile_EK80 == DirTableSurvey$EK80_Cal_File){
      stop("EK80 .ecs file is wrong or missing", call. = FALSE)
    }
    
    # export .evr file
    # filename
    regionfilename <-  file.path(Regdir, paste(name, "evr", sep="."))
    # export
    EVfile[["Regions"]]$ExportDefinitionsAll(regionfilename)
  
    # export bottom line
      name_sub=sub('\\.','_',name) #to remove the "." in the first three transect names so bottom line gets properly named and exported...
      if(EVbottom == TRUE){
      linesObj <- EVfile[["Lines"]]
      bottom <- linesObj$FindbyName(EVbottomname)
      bottomfilename <- file.path(Linedir, paste(name_sub, "bottom", "evl", sep="."))
      #bottom$Export(bottomfilename)
      varObj <- EVfile[["Variables"]]
      LineExportVar = varObj$FindbyName(LineExportVarName)
      LineExportVar$ExportLine(bottom,bottomfilename,-1,-1)
    }
  
    #Get transducer depths
    #Not all transducer depths were set correctly in original EV files - use
    #38 kHz transducer depth
    TransObj<-EVfile[["Transducers"]] 
    XD38<-TransObj$FindByName("38 kHz Transducer")
    Dducer<-XD38$VerticalOffset()   
    # Dducer<-list()
    nducers<-TransObj$Count()
    # for (t in 1:nducers){
    #   Dducer[t]<-TransObj$Item(t-1)$VerticalOffset()    
    #   #Not all transducers were set correctly in original EV file.  Go off of first transducer
    # }
    #quit echoview
    EVApp$Quit()
  
  
  
  
  #####################################
  #          Make EV file             #
  #####################################
  
  # create COM connection between R and Echoview
  EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")
  
  # Open template EV file
  EVfile <- EVApp$OpenFile(file.path(template))
  
  # Set fileset object for EK60
  filesetObj <- EVfile[["Filesets"]]$Item(0)
  
  # Set calibration file
  
  if(!calPath == ""){
  
  #    if(calName == ecsfile_44_77){
      add.calibration <- filesetObj$SetCalibrationFile(calFile)
        } else {
          stop(".ecs file is wrong or missing", call. = FALSE)
  #      }
  }
  
  
  # Add raw files, either with .raw or .ek60 extension; if no raw data located, error
  
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
  
  # Set fileset object for EK80
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
  bottom <- linesObj$FindbyName(paste0("Line",linenum+1))
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
  
  #Set transducer depths
  TransObj<-EVfile[["Transducers"]] 
  for (t in 1:nducers){
    TOItem<-TransObj$Item(t-1)
    #TOItem[["VerticalOffset"]]<-Dducer[[t]]
    TOItem[["VerticalOffset"]]<-Dducer
    #set transducer offset
  }
  
  #Update export parameters
  Obj_ExpVar<-EVfile[['Properties']][['Export']][['Variables']]  #set export variables object
  SetBiomassExpParamsFun(Obj_ExpVar)
  #source("c:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_EK80_conv/SetBiomassExpParams.R")
  Obj<-Obj_ExpVar$Item("Depth_mean")
  Obj[["Enabled"]]<-TRUE
  Obj<-Obj_ExpVar$Item("Height_mean")
  Obj[["Enabled"]]<-TRUE
  Obj<-Obj_ExpVar$Item("Range_mean")
  Obj[["Enabled"]]<-TRUE
  
  # Save EV file
  EVfile$SaveAS(file.path(CONV_new_template,i))
  
  # Close EV file
  EVApp$CloseFile(EVfile)
  
  # Quit echoview
  EVApp$Quit()
  
    }
}
