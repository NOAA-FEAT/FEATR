% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_EV_exports_compile.R
\name{read_EV_exports_compile}
\alias{read_EV_exports_compile}
\title{read_EV_exports_compile: Read and append certain columns from multiple
variable Echoview exports}
\usage{
read_EV_exports_compile(
  variables,
  CompCols,
  SurveyName,
  DirNameFile,
  database = 1,
  exportloc = 1,
  ...
)
}
\arguments{
\item{variables}{- variables to read in (subdirectories need to be named as variables)}

\item{SurveyName}{- name of survey}

\item{DirNameFile}{excel file - contains paths and locations of files}

\item{database}{is an optional argument for format of the .csv files to input.  Default is database (1).  Spreadsheet =0}

\item{exportloc}{is an optional argument for using the Export_Dir or the Region_Export_dir column
entry in the DirNameFile spreadsheet.  1 is Export_Dir, and 0 is Region_Export_dir}

\item{ComCols}{- the columns to be kept from the non-primary variable and appended 
to the end of the data.}
}
\description{
Reads variables from multiple EV files.  The columns from the 
first variable are completely kep.  The requested columns (CompCols) from the rest of the
variables are appended onto the end of the dataframe.  Usually for exports from
multiple EV files.  Reads in multiple 
variables exported  from each EV file.   
Example columns are Sv_mean and PRC_NASC.
}
