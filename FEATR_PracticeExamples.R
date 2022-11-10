#devtools::install_github("NOAA-FEAT/FEATR")
library(FEATR)
#dirstruct <- read_excel("E:/GitHub/FEATR/R/Directory Structure EK60 EK80 conversion updated.xlsx")
DirNameFile <- "E:/GitHub/FEATR/R/Directory Structure EK60 EK80 conversion updated.xlsx"
SurveyName <- "2019_US"
repath_EVdir(SurveyName,DirNameFile, ni=1)

SetBiomassExpParamsFun(Obj_ExpVar, ToExport,NoExport)