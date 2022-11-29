# miaSimShiny

The goal of miaSimShiny is to facilitate the use of [miaSim (an R package for microbial community simulations)](https://github.com/microbiome/miaSim) in [a local shiny app](https://shiny.rstudio.com).

## Installation

You can install the development version of miaSimShiny like so:

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("miaSim")

# install the latest version of miaSim via github
devtools::install_github("microbiome/miaSim")

if (!require("devtools", quietly = TRUE))
  install.packages("devtools")
devtools::install_github("gaoyu19920914/miaSimShiny")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(miaSimShiny)
## run the shiny app for miaSim
miaSimShiny::run_app()
```

## Online version

Follow [this link](https://gaoyu.shinyapps.io/shiny_rep/) to try the online version, which is limited in usage and function.
