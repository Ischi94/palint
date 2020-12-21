
# palint

The goal of `palint` is to have a package with helper functions to
calculate palaeoclimate interactions. It is currently used privately but
the goal is to add all functions I have used in the past in here.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Ischi94/palint")
```

## Example

The package currently contains one function: `perc_change`. It
calculates the percentage change between two values:

``` r
library(palint)
perc_change(2, 4)
#> [1] 1
```

The default returns the raw value, but by changing the `print.result`
parameter we can get the raw value expressed as percentage or in a text
description:

``` r
perc_change(2, 4, "percentage")
#> [1] 100
```

``` r
perc_change(2, 4, "text")
#> [1] "The percentage change from 2 to 4 is 100%"
```
