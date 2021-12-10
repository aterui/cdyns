README
================

# Installation

The `cdyns` package can be installed with the following script:

``` r
#install.packages("remotes")
remotes::install_github("aterui/cdyns")
library(cdyns)
```

# Overview

The R package `cdyns` is a collection of functions to perform community
dynamics simulations with stock enhancement. The current version of the
package includes the following functions:

-   `cdynsim` : Community dynamics simulation with stock enhancement

# Instruction

## Basic usage

The key arguments are `n_timestep` (the number of time step to be
saved), `n_warmup` (warm-up period during which seeding happens),
`n_burnin` (burn-in period for eliminating initial condition effects),
and the number of species in a community (`n_species`). The community
dynamics are simulated using either a Ricker equation
(`model = "ricker"`) or a Beverton-Holt equation (`model = "bh"`).

Sample script:

``` r
library(cdyns)
sim <- cdynsim(n_timestep = 1000,
               n_warmup = 200,
               n_burnin = 200,
               n_species = 10)
```

This script returns the following:

-   `df_dyn`: dataframe for dynamics. Columns include time step
    (`timestep`), species id (`species`), and density (`density`)

-   `df_community`: dataframe for the whole community. Columns include a
    temporal mean (`mean_density`) and sd (`sd_density`) of the whole
    community density.

-   `df_species`: dataframe for species density and traits. Columns
    include species id (species), temporal mean (`mean_density`) and sd
    of species density (`sd_density`), carrying capacity (`k`),
    intrinsic population growth rate (`r`), and interspecific
    competition coefficient with the stocked species (`alpha_j1`).

-   `interaction_matrix`: matrix of intra- and interspecific
    interactions.

``` r
print(sim)
```

    ## $df_dyn
    ## # A tibble: 10,000 x 3
    ##    timestep species density
    ##       <dbl>   <dbl>   <dbl>
    ##  1        1       1    22.0
    ##  2        1       2    17.2
    ##  3        1       3    20.7
    ##  4        1       4    16.3
    ##  5        1       5    11.3
    ##  6        1       6    20.5
    ##  7        1       7    19.8
    ##  8        1       8    17.6
    ##  9        1       9    16.2
    ## 10        1      10    15.7
    ## # ... with 9,990 more rows
    ## 
    ## $df_community
    ## # A tibble: 1 x 2
    ##   mean_density sd_density
    ##          <dbl>      <dbl>
    ## 1         182.       6.84
    ## 
    ## $df_species
    ## # A tibble: 10 x 6
    ##    species mean_density sd_density     k     r alpha_j1
    ##      <int>        <dbl>      <dbl> <dbl> <dbl>    <dbl>
    ##  1       1         18.2       3.63   100   1.5      1  
    ##  2       2         18.2       3.73   100   1.5      0.5
    ##  3       3         18.4       3.51   100   1.5      0.5
    ##  4       4         18.1       3.31   100   1.5      0.5
    ##  5       5         18.5       3.29   100   1.5      0.5
    ##  6       6         18.0       3.50   100   1.5      0.5
    ##  7       7         17.7       3.76   100   1.5      0.5
    ##  8       8         18.1       3.62   100   1.5      0.5
    ##  9       9         18.2       3.41   100   1.5      0.5
    ## 10      10         18.4       3.34   100   1.5      0.5
    ## 
    ## $interaction_matrix
    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]  1.0  0.5  0.5  0.5  0.5  0.5  0.5  0.5  0.5   0.5
    ##  [2,]  0.5  1.0  0.5  0.5  0.5  0.5  0.5  0.5  0.5   0.5
    ##  [3,]  0.5  0.5  1.0  0.5  0.5  0.5  0.5  0.5  0.5   0.5
    ##  [4,]  0.5  0.5  0.5  1.0  0.5  0.5  0.5  0.5  0.5   0.5
    ##  [5,]  0.5  0.5  0.5  0.5  1.0  0.5  0.5  0.5  0.5   0.5
    ##  [6,]  0.5  0.5  0.5  0.5  0.5  1.0  0.5  0.5  0.5   0.5
    ##  [7,]  0.5  0.5  0.5  0.5  0.5  0.5  1.0  0.5  0.5   0.5
    ##  [8,]  0.5  0.5  0.5  0.5  0.5  0.5  0.5  1.0  0.5   0.5
    ##  [9,]  0.5  0.5  0.5  0.5  0.5  0.5  0.5  0.5  1.0   0.5
    ## [10,]  0.5  0.5  0.5  0.5  0.5  0.5  0.5  0.5  0.5   1.0

## Stock enhancement

Stock enhancement can be added using `stock` argument. For example,
`stock = 100` adds 100 individuals to species 1 every time step:

``` r
sim <- cdynsim(n_timestep = 1000,
               n_warmup = 200,
               n_burnin = 200,
               n_species = 10,
               stock = 100)
```
