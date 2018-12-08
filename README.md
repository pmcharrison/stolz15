
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stolz15 - Implementation of Stolzenburg’s (2015) Periodicity Algorithm

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Periodicity, also known as harmonicity, is an important predictor of a
chord’s consonance. This package implements Stolzenburg’s (2015)
algorithm for quantifying a chord’s periodicity.

## Installation

You can install this package from GitHub:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/stolz15")
```

## Example usage

Higher model outputs correspond to greater period lengths and hence
lower periodicity. A major chord therefore produces a lower score than a
diminished chord:

``` r
library(stolz15)

smooth_log_periodicity(c(60, 64, 67))
#> [1] 2
smooth_log_periodicity(c(60, 63, 66))
#> [1] 3.786034
```

## References

Stolzenburg, F. (2015). Harmony perception by periodicity detection.
Journal of Mathematics and Music, 9(3), 215–238.
<https://doi.org/10.1080/17459737.2015.1033024>
