# SALURhelper


## Overview

The *Salud Urbana en America Latina* (SALURBAL) research project and
it’s successor, SALURBAL-Climate, are focused understanding health in
urban Latin America. Many of these papers use similar statistical
methods. This package provides a centralized location for common helper
functions used in such research.

## Installation

``` r
devtools::install_github("SALURBAL-Climate/SALURhelper")
library(SALURhelper)
```

## Usage

To get the QAIC of a quasi-Poisson model, use the `QAIC` function on the
model output.

``` r
fit <- glm(hp ~ mpg + disp + wt, family = "quasipoisson", data = mtcars)
stats::AIC(fit)
```

    [1] NA

``` r
SALURhelper::QAIC(fit)
```

    [1] 505.2654

## How to Contribute

This package is meant to serve SALURBAL researchers, and we want to
incorporate any feedback to make the package better.

- If you have ideas for functions that you would like to add to
  `SALURhelper`, please file an
  [issue](https://github.com/SALURBAL-Climate/SALURhelper/issues).
- If you encounter any bugs, or suspect the code is not functioning
  properly, please file an
  [issue](https://github.com/SALURBAL-Climate/SALURhelper/issues) with a
  [minimum reproducible
  example](https://stackoverflow.com/help/minimal-reproducible-example).
