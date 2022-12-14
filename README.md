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

- `cdynsim` : Community dynamics simulation with stock enhancement

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

- `df_dyn`: dataframe for dynamics. Columns include time step
  (`timestep`), species id (`species`), and density (`density`)

- `df_community`: dataframe for the whole community. Columns include a
  temporal mean (`mean_density`) and sd (`sd_density`) of the whole
  community density.

- `df_species`: dataframe for species density and traits. Columns
  include species id (species), temporal mean (`mean_density`) and sd of
  species density (`sd_density`), carrying capacity (`k`), intrinsic
  population growth rate (`r`), and interspecific competition
  coefficient with the stocked species (`alpha_j1`).

- `interaction_matrix`: matrix of intra- and interspecific interactions.

``` r
print(sim)
```

    ## $df_dyn
    ## # A tibble: 10,000 × 3
    ##    timestep species density
    ##       <dbl>   <dbl>   <dbl>
    ##  1        1       1    19.5
    ##  2        1       2    18.2
    ##  3        1       3    23.5
    ##  4        1       4    14.8
    ##  5        1       5    14.5
    ##  6        1       6    22.2
    ##  7        1       7    23.9
    ##  8        1       8    19.8
    ##  9        1       9    23.6
    ## 10        1      10    18.3
    ## # … with 9,990 more rows
    ## 
    ## $df_community
    ## # A tibble: 1 × 2
    ##   mean_density sd_density
    ##          <dbl>      <dbl>
    ## 1         182.       7.11
    ## 
    ## $df_species
    ## # A tibble: 10 × 6
    ##    species mean_density sd_density     k     r alpha_j1
    ##      <int>        <dbl>      <dbl> <dbl> <dbl>    <dbl>
    ##  1       1         18.4       3.44   100   1.5      1  
    ##  2       2         18.3       3.39   100   1.5      0.5
    ##  3       3         18.2       3.49   100   1.5      0.5
    ##  4       4         17.9       3.31   100   1.5      0.5
    ##  5       5         17.7       3.49   100   1.5      0.5
    ##  6       6         18.4       3.12   100   1.5      0.5
    ##  7       7         17.7       3.26   100   1.5      0.5
    ##  8       8         18.2       3.67   100   1.5      0.5
    ##  9       9         19.1       3.49   100   1.5      0.5
    ## 10      10         17.9       3.47   100   1.5      0.5
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
    ## 
    ## $vcov_matrix
    ##             species1    species2   species3    species4    species5   species6
    ## species1  11.8466874 -0.98283589 -0.6426082 -0.98435486 -0.97300095 -1.1876555
    ## species2  -0.9828359 11.46050451  0.4921862 -0.66278165  0.03436199 -0.5331024
    ## species3  -0.6426082  0.49218617 12.1709428 -0.81415025 -2.98860059 -1.0439455
    ## species4  -0.9843549 -0.66278165 -0.8141503 10.92351262  0.52519723 -0.3643399
    ## species5  -0.9730010  0.03436199 -2.9886006  0.52519723 12.15038681 -0.8875066
    ## species6  -1.1876555 -0.53310235 -1.0439455 -0.36433992 -0.88750657  9.7377165
    ## species7  -0.4893666 -0.48286153 -0.3145561 -1.24858001  0.12996592 -1.2049932
    ## species8   0.3616652 -1.73654262 -0.4331405 -1.96524746 -0.35562874 -0.6246875
    ## species9   0.4535407 -3.02322596 -1.1720978 -1.64968511 -0.60696496  0.2094594
    ## species10 -2.3316904  1.26362679  0.7584752  0.05749195 -2.23765300  0.6636385
    ##             species7   species8   species9   species10
    ## species1  -0.4893666  0.3616652  0.4535407 -2.33169042
    ## species2  -0.4828615 -1.7365426 -3.0232260  1.26362679
    ## species3  -0.3145561 -0.4331405 -1.1720978  0.75847518
    ## species4  -1.2485800 -1.9652475 -1.6496851  0.05749195
    ## species5   0.1299659 -0.3556287 -0.6069650 -2.23765300
    ## species6  -1.2049932 -0.6246875  0.2094594  0.66363845
    ## species7  10.6122320 -0.7016609  0.5623412 -2.26619852
    ## species8  -0.7016609 13.4780745 -1.0874900 -1.61854029
    ## species9   0.5623412 -1.0874900 12.1766311 -0.89465394
    ## species10 -2.2661985 -1.6185403 -0.8946539 12.05063040

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
