
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FEATR

<!-- badges: start -->

<!-- badges: end -->

The goal of FEATR is to provide a tidy package of frequently used R
files for Echoview processing.

## Installation

You can install the released version of FEATR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("FEATR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NOAA-FEAT/FEATR")
```

## Example

To set the export parameters for within an EV file, use
SetBiomassExpParamsFun(Obj\_ExpVar, ToExport,NoExport), where
Obj\_ExpVar is the Echoview object of the acoustic variable, ToExport is
the list of parameters to have be exported, and NoExport is the list of
parameters to not have exported. It’s handy to keep those in separate
columns of an excel file
