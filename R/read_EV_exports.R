#' read_EV_exports:  Read in a directory of .csv files exported from Echoview
#'
#' @description Read in a directory of .csv files (can be of several variables) 
#' exported from Echoview.   Uses
#' @param var - variables to read in (subdirectories need to be named as variables)
#' @param SurveyName - name of survey
#' @param DirNameFile excel file name of paths
#' 
#'
#' @examples
#' test example
#'
#' @export

read_EV_exports<-function(var, SurveyName, DirNameFile){
  ####################################################################################################################
  
  ### This is code to read in exported EK60/EK80 data as a function (.csv file format) from DFO and US surveys from same year, add some metadata, 
  # and then combine together to one .csv for analysis. This script preserves the 10-m depth cells
  ##NOTE: Not fully workable yet - only testing for US 2019
  ### Adapted from code by Beth Phillips
  ### Last edited 8/31/2020 by Rebecca Thomas
  
  #Inputs: 
  #DirNameFile - files with directory and file locations
  #Survey - survey to be loaded (e.g 2019_US)
  
  ###################################################################################################################
 

# library(readxl)
# library(readr)
# library(purrr)
# library(dplyr)

####################################################################################################################

#Things to change (mostly the year of interest)
#year<-2019
#var <- "Sv_corrected (scenario 1) On transect only" # required to navigate to folder with filtered, exported data

#### DOUBLE CHECK ABOVE, THEN PROCEED ####
DirNameFile="C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_Ek80_conv/Directory Structure EK60 EK80 conversion.xlsx"

DirTable <- readxl::read_excel(DirNameFile)
DirTable_yr<- subset(DirTable, Survey ==SurveyName) #just year of interest
DirTable_yr[] <- lapply(DirTable_yr, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

#FilePaths <- read.csv("E:/Survey.Acoustics/Projects & Analysis/Euphausiids/EUPH documents/EUPH_paths.csv") #This file should have most updated file.paths for all directories
#FilePaths_yr <- subset(FilePaths, Year == year) #just year of interest
#FilePaths_yr[] <- lapply(FilePaths_yr, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

#Pull out both surveys (CAN and US) for each year
#Survey <- DirTable_yr$Survey

#setup folders to read in data
#BaseYearPath<-as.character(DirTable_yr$BaseYearPath[1]) #same for both surveys within year, select first one
#BaseProjPath<-as.character(DirTable_yr$BaseProjPath[1]) #same for both surveys within year, select first one

#setup folders to read in data
BaseYearPath<-as.character(DirTable_yr$Base_Path[1]) #same for both surveys within year, select first one
BaseProjPath<-as.character(DirTable_yr$Base_Path[1]) #same for both surveys within year, select first one

#Function to read in .csv files and append filename into new column

# read_csv_addflnm <- function(flnm) {
#   temp<-suppressMessages(read_csv(flnm)) %>% 
#     mutate(filename = flnm)
#    #print(nrow(temp))
#    #temp<-mutate(temp,num_rows=nrow(temp))
# }

 #for (j in c(1)){

#for (j in 1:length(Survey)){
  show(var)
  CONV_exportbase <- file.path(DirTable_yr$Export_Dir, var) 
  setwd(CONV_exportbase)
  NASC.list.to.combine <-list.files(file.path(CONV_exportbase), pattern = "*.csv") #be sure to verify correct number of files loaded
  CONV_10m_combine <- NASC.list.to.combine %>% purrr::map_df(~read_csv_addflnm(.))
  
  #load in analysis sheets
  NASC.list.to.combine.analysis <-list.files(file.path(CONV_exportbase), pattern = "*analysis).csv") #be sure to verify correct number of files loaded
  CONV_10m_combine_analysis <- NASC.list.to.combine.analysis %>% purrr::map_df(~read_csv_addflnm(.))
  
  #load in cell sheets
  NASC.list.to.combine.cells <-list.files(file.path(CONV_exportbase), pattern = "*cells).csv") #be sure to verify correct number of files loaded
  CONV_10m_combine_cells <- NASC.list.to.combine.cells %>% purrr::map_df(~read_csv_addflnm(.))
  #print(NASC.list.to.combine.cells)
  
  #load in interval sheets
  NASC.list.to.combine.intervals <-list.files(file.path(CONV_exportbase), pattern = "*intervals).csv") #be sure to verify correct number of files loaded
  CONV_10m_combine_intervals <- NASC.list.to.combine.intervals %>% purrr::map_df(~read_csv_addflnm(.))
  
  #load in layers sheets
  NASC.list.to.combine.layers <-list.files(file.path(CONV_exportbase), pattern = "*layers).csv") #be sure to verify correct number of files loaded
  CONV_10m_combine_layers <- NASC.list.to.combine.layers %>% purrr::map_df(~read_csv_addflnm(.))
  
  #do join
  temp <- dplyr::inner_join(CONV_10m_combine_cells, CONV_10m_combine_intervals, by=c("Process_ID","Interval"))
  temp <- dplyr::inner_join(temp, CONV_10m_combine_layers, by=c("Process_ID","Layer"))  
  CONV_10m_join<- dplyr::inner_join(temp, CONV_10m_combine_analysis, by="Process_ID")
  CONV_10m_join$Transect <- sapply(strsplit(CONV_10m_join$filename.x,"_", fixed = TRUE),`[`,1)
  CONV_10m_join$ID <- paste(CONV_10m_join$Transect,CONV_10m_join$Interval, sep="_") 
  CONV_10m_join$Survey <- paste(SurveyName) 
  CONV_10m_join$Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", CONV_10m_join$Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
#} 
 
  CONV_10m_join
   # #Add other identifiers
  # CONV_10m_combine$Transect <- sapply(strsplit(CONV_10m_combine$filename,"_", fixed = TRUE),`[`,1)
  # CONV_10m_combine$ID <- paste(CONV_10m_combine$Transect,CONV_10m_combine$Interval, sep="_") 
  # CONV_10m_combine$Survey <- paste(Survey[j]) 
  
  #write separate .csv for each survey, just in case needed for future
  # DatAnalSumDir<-file.path(DirTable_yr$Data_Compile_Dir[j],Survey[j])
  # suppressWarnings(dir.create(DatAnalSumDir))
  # write.csv(CONV_10m_join, file.path(DatAnalSumDir,"_10m_combine.csv"), row.names = F)
  
}

# #combine both .csv's into one giant file for analysis (DFO and US 10-m exports by year)
# CONV_10m.list <- c(paste(Survey[],"_10m_combine.csv",sep=""))
# setwd(paste(BaseProjPath,"/Data for Analyses/",sep=""))
# CONV_10m_all <- lapply(CONV_10m.list, read_csv) %>% bind_rows()
# write.csv(CONV_10m_all, paste(year,"_10m_CombineAll.csv", sep=""), row.names = F) 
# 
# # 
# range(CONV_10m_all$ID)
# 
# #Sum NASC through water column (WC) grouped by ID (transect_interval) and write .csv out
# 
# #Fix date first...
# CONV_10m_all$Date_M <- as.POSIXct(as.character(CONV_10m_all$Date_M), format="%Y%m%d")
# 
# head(CONV_10m_all)
# 
# CONV_120_WC_NASC <- CONV_10m_all %>%
#   group_by(ID) %>% 
#   summarise(Transect=first(Transect), Interval=first(Interval), Year=year, Survey=first(Survey), NASC.80=sum(NASC.80), Lat=mean(Lat_M), Lon=mean(Lon_M), 
#             Dist=median(Dist_M), Date=median(Date_M), DepthBin="WC")
# 
# #WC.dir <- "E:/Survey.Acoustics/Projects & Analysis/Euphausiids/EUPH/Data for Analyses/NASC_0.5nmi_WC"
# WC.dir<-DirTable_yr$Data_Compile_Dir  #Directory for data to be saved into
# write.csv(CONV_120_WC_NASC, paste(WC.dir[j],"/",year,"_0.5nmi_WC_NASC.csv", sep=""), row.names = F)  
# 
# 
# #######################################################################
#   
# ### Old Code ###
# 
# # Create new columns for survey, transect, and ship ID
# CONV_combine$Transect <- gsub(pattern = "\\_120_0.5nmi_10m_cells_50to300m_80dB.csv$", "", EUPH_120_combine$filename)
# 
# CONV_combine$ShipID <- noquote(strsplit(CONV_exportbase, split = "_")[[1]][2]) #add in ShipID column to track (had to do it in two steps)
# CONV_combine$ShipID <- noquote(strsplit(CONV_combine$ShipID, split = "/")[[1]][1])
