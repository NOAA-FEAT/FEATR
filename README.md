
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FEATR

<!-- badges: start -->

<!-- badges: end -->

The goal of FEATR is to provide a tidy package of frequently used R
files for Echoview processing. This package is in its initial state and
will be undergoing considerable updates.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NOAA-FEAT/FEATR")
```

## Examples

1)  To update the paths of all EV files in a directory (update .raw
    files, calibration files, etc), use
    repath\_EVdir(SurveyName,DirNameFile, ni=1) Where SurveyName is the
    name of the survey referenced as a row in the excel file
    DirNameFile. This excel file has a list of paths or filenames in
    named columns, with each row being a “survey.” ni is the number of
    the EVfile in the directory list to start the processing at. The
    default is ni=1.

2)  To set the export parameters for within an EV file, use
    SetBiomassExpParamsFun(Obj\_ExpVar, ToExport,NoExport), where
    Obj\_ExpVar is the Echoview object of the acoustic variable,
    ToExport is the list of parameters to have be exported, and NoExport
    is the list of parameters to not have exported. It’s handy to keep
    those in separate columns of an excel file
