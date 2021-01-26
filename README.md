# tidySpectR

## Introduction

The aim of this package is to provide tidy methods for the 
analysis of spectral dataset in order to improve 
accessibility and reproducibility of chemometric analysis.
No more 'Analysis was performed using a collection of 
undocumented MATLAB scripts available upon request'.

This package is meant to fill the gap between data acquisition 
and preprocessing (phasing, baseline correction) and modelling
with the tidymodels metapackage.

This package provides an S3 class `collection` storing spectra informations
and methods to process the data. It also includes `step_` methods specific
to chemometrics to use with the [tidymodels recipe package](https://github.com/tidymodels/recipes). 

```r
library(tidySpectR)
library(tidymodels)

data(fa_nmr)

# H1-NMR data of egg yolk fatty acids
print(fa_nmr)
#> 
#> Spectra collection containing 10 entries.
#> Number of bins: 47500
#> Normalized: FALSE
#> Bucketted: FALSE
#> Labels: control treatment

processed <- fa_nmr %>% 
             # Trimming edges
             extract(from = 0.5, to = 7.2) %>% 
             # Masking water peak
             mask(from = 4.58, to = 4.68) %>% 
             # Masking MeOD peak
             mask(from = 3.33, to = 3.38) %>% 
             # Bucketting with a bucket size of 0.04 ppm
             bucket_uniform(width = 0.04) %>% 
             # Normalizing with the intensity of the methylen group of phospahtidylcholin
             normalize_internalStandard(from = 3.58, to = 3.64) %>%
             tidy()

print(processed[,1:5]) 
#> 
#> # A tibble: 10 x 5
#> # Groups:   id [10]
#>    id          label     `0.519600000000001` `0.559600000000001` `0.5996`
#>    <fct>       <fct>                   <dbl>               <dbl>    <dbl>
#>  1 20198124123 control                0.0173              0.0270   0.0236
#>  2 20198124124 control                0.0186              0.0271   0.0253
#>  3 20198124125 control                0.0138              0.0218   0.0191
#>  4 20198124469 control                0.0176              0.0269   0.0244
#>  5 20198124538 control                0.0161              0.0246   0.0219
#>  6 20198124540 treatment              0.0159              0.0238   0.0219
#>  7 20198124553 treatment              0.0152              0.0233   0.0209
#>  8 20198124554 treatment              0.0154              0.0249   0.0217
#>  9 20198124555 treatment              0.0177              0.0263   0.0240
#> 10 20198125029 treatment              0.0171              0.0251   0.0232

# Pre-processing for data analysis using the `recipe` package
nmr_preprocessing <- recipe(processed, label ~.) %>%
                     update_role(id, new_role = "ID") %>% 
                     # Pareto scaling
                     step_pareto(all_predictors()) %>%
                     # Adding VAST normalization
                     step_vast(all_predictors()) %>%
                     # PCA transformation
                     step_pca(all_predictors(), num_comp = 5)

# Check the data 
nmr_preprocessing %>% prep() %>% juice()
#> 
#> # A tibble: 10 x 7
#>    id          label           PC1       PC2       PC3       PC4       PC5
#>    <fct>       <fct>         <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 20198124123 control   -3.83e-15 -2.61e-15  8.82e-15 -3.34e-15 -3.28e-15
#>  2 20198124124 control    5.20e-15 -9.61e-15 -4.15e-15 -2.77e-15 -4.27e-16
#>  3 20198124125 control   -3.34e-15  6.99e-15  3.39e-15 -1.82e-15  2.48e-15
#>  4 20198124469 control   -2.18e-14 -4.47e-15  1.57e-16  3.33e-15  2.87e-15
#>  5 20198124538 control   -6.77e-15  5.62e-15 -3.04e-15 -1.35e-15  2.16e-16
#>  6 20198124540 treatment -6.06e-16  2.00e-15 -6.70e-15 -3.50e-15  3.22e-16
#>  7 20198124553 treatment  1.84e-14  1.46e-15  3.55e-16  3.57e-15  6.50e-16
#>  8 20198124554 treatment  1.25e-14 -2.07e-15  3.19e-15 -1.10e-16  4.24e-15
#>  9 20198124555 treatment -1.46e-15  4.75e-17 -7.82e-16  5.55e-15 -2.85e-15
#> 10 20198125029 treatment  1.69e-15  2.64e-15 -1.22e-15  4.35e-16 -4.22e-15
```

## Installation

This package is not yet on CRAN, install from github:

```r
require("devtools")
install_github("CVUA-RRW/tidySpectR")
```

## Contributions

Contributions are very welcome!
If you would like to see new functionnalities submit an issue directly 
on github or start a PR.