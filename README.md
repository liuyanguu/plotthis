# plotthis

The goal of 'plotthis' is to wrap up some frequently used ggplot functions

## Installation

Please install from github:
```r
devtools::install_github("liuyanguu/plotthis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(plotthis)

scatter.plot.simple(data = datam1_terra, x = "col_water_cm", y = "Column_WV")

scatter.plot.diagonal(data = datam1_terra, x = "col_water_cm", y = "Column_WV", add_abline = T)

```

