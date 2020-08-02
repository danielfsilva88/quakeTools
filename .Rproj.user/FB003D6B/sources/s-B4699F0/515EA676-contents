# quakeTools

The goal of visTools is to clean and plot earthquake data from **The 
Significant Database Earthquake** of [NOAA](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

## Installation

You can install the released version of visTools from [GitHub](https://github.com/) with:

``` r
devtools::install_github("danielfsilva88/quakeTools")
```

## Example

This is a basic example which shows you how to use this package:

``` r
library(quakeTools)
earthquakes %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")
```
