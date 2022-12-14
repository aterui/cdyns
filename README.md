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
    ##  1        1       1    16.0
    ##  2        1       2    21.8
    ##  3        1       3    20.0
    ##  4        1       4    24.5
    ##  5        1       5    14.1
    ##  6        1       6    20.2
    ##  7        1       7    20.8
    ##  8        1       8    16.2
    ##  9        1       9    19.9
    ## 10        1      10    14.6
    ## # … with 9,990 more rows
    ## 
    ## $df_community
    ## # A tibble: 1 × 2
    ##   mean_density sd_density
    ##          <dbl>      <dbl>
    ## 1         182.       6.76
    ## 
    ## $df_species
    ## # A tibble: 10 × 6
    ##    species mean_density sd_density     k     r alpha_j1
    ##      <int>        <dbl>      <dbl> <dbl> <dbl>    <dbl>
    ##  1       1         18.3       3.41   100   1.5      1  
    ##  2       2         17.9       3.61   100   1.5      0.5
    ##  3       3         18.3       3.49   100   1.5      0.5
    ##  4       4         18.0       3.38   100   1.5      0.5
    ##  5       5         18.2       3.43   100   1.5      0.5
    ##  6       6         18.1       3.43   100   1.5      0.5
    ##  7       7         17.9       3.33   100   1.5      0.5
    ##  8       8         18.6       3.49   100   1.5      0.5
    ##  9       9         18.5       3.28   100   1.5      0.5
    ## 10      10         18.1       3.33   100   1.5      0.5
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
    ##              species1   species2   species3   species4   species5    species6
    ## species1  11.64802866 -1.7038982 -1.4740907 -1.1130980 -0.6672343 -0.04444368
    ## species2  -1.70389816 13.0265790 -1.4947944 -1.2865694 -0.8272281 -1.45325147
    ## species3  -1.47409073 -1.4947944 12.2086851 -1.0842118  0.1159128  0.62925744
    ## species4  -1.11309801 -1.2865694 -1.0842118 11.4234922 -0.9715645 -0.75601922
    ## species5  -0.66723428 -0.8272281  0.1159128 -0.9715645 11.7657396 -1.64855365
    ## species6  -0.04444368 -1.4532515  0.6292574 -0.7560192 -1.6485537 11.76768074
    ## species7  -1.20254580  0.1270377 -0.2007672 -0.7199614 -1.6027865 -0.73613316
    ## species8  -0.46812400 -1.5757900 -0.1001612 -0.4401668  0.3222239 -1.93580936
    ## species9  -1.69436531  0.5526940 -1.1378852 -1.1959470 -1.6485844 -0.58164199
    ## species10  0.88830657 -0.7458556 -2.3854508 -0.1383661 -0.4176972 -0.17558458
    ##             species7   species8   species9  species10
    ## species1  -1.2025458 -0.4681240 -1.6943653  0.8883066
    ## species2   0.1270377 -1.5757900  0.5526940 -0.7458556
    ## species3  -0.2007672 -0.1001612 -1.1378852 -2.3854508
    ## species4  -0.7199614 -0.4401668 -1.1959470 -0.1383661
    ## species5  -1.6027865  0.3222239 -1.6485844 -0.4176972
    ## species6  -0.7361332 -1.9358094 -0.5816420 -0.1755846
    ## species7  11.0910564 -1.5351145  0.7550421 -2.2499270
    ## species8  -1.5351145 12.1517215 -0.8478887 -1.1512771
    ## species9   0.7550421 -0.8478887 10.7730985  0.3769068
    ## species10 -2.2499270 -1.1512771  0.3769068 11.1192567

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
