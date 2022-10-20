#' read_EV_exports:  Read in a directory of .csv files exported from Echoview
#'
#' @description Read in a directory of .csv files (can be of several variables) 
#' exported from Echoview.   The function preserves the cells by depth. 
#' Uses packages readxl, readr, purrr, and dplyr.
#' @param var - variables to read in (subdirectories need to be named as variables)
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files
#' 
#'
#' @examples
#' temp<-read_EV_exports(variables[1], SurveyName, DirNameFile)
#'
#' @export

read_EV_exports<-function(var, SurveyName, DirNameFile){

DirTable <- readxl::read_excel(DirNameFile)  #load in directories/paths/locations
DirTable_yr<- subset(DirTable, Survey ==SurveyName) #just year of interest
DirTable_yr[] <- lapply(DirTable_yr, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

#setup folders to read in data
BaseYearPath<-as.character(DirTable_yr$Base_Path[1]) #same for both surveys within year, select first one
BaseProjPath<-as.character(DirTable_yr$Base_Path[1]) #same for both surveys within year, select first one

show(var)  #display the variable to be imported
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

#do joins
temp <- dplyr::inner_join(CONV_10m_combine_cells, CONV_10m_combine_intervals, by=c("Process_ID","Interval"))
temp <- dplyr::inner_join(temp, CONV_10m_combine_layers, by=c("Process_ID","Layer"))  
CONV_10m_join<- dplyr::inner_join(temp, CONV_10m_combine_analysis, by="Process_ID")
CONV_10m_join$Transect <- sapply(strsplit(CONV_10m_join$filename.x,"_", fixed = TRUE),`[`,1)
CONV_10m_join$ID <- paste(CONV_10m_join$Transect,CONV_10m_join$Interval, sep="_") 
CONV_10m_join$Survey <- paste(SurveyName) 
CONV_10m_join$Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", CONV_10m_join$Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
CONV_10m_join  #return result
}


