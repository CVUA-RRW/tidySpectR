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
                     # Pareto-VAST scaling
                     step_vast(all_predictors(), scaling = 'pareto') %>%
                     # PCA transformation
                     step_pca(all_predictors(), num_comp = 5)

# Check the data 
nmr_preprocessing %>% prep() %>% juice()
#> 
#> # A tibble: 10 x 7
#>    id          label        PC1    PC2     PC3     PC4    PC5
#>    <fct>       <fct>      <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
#>  1 20198124123 control    -9.02  -6.24 -22.3    11.7   -5.69
#>  2 20198124124 control   -57.6   46.9    8.19    6.24  -2.53
#>  3 20198124125 control    60.2  -41.6    1.55   10.2    0.529
#>  4 20198124469 control   -73.9  -32.4   -2.93    0.531 10.1
#>  5 20198124538 control     4.72 -28.7    8.96   -7.51  -2.76
#>  6 20198124540 treatment  -3.25  -5.80  21.4     6.83  -5.20
#>  7 20198124553 treatment  60.9   33.3    2.02   -7.28   6.52
#>  8 20198124554 treatment  26.2   32.2   -5.78   10.4    4.21
#>  9 20198124555 treatment  -6.60  -2.26   0.285 -14.1    0.927
#> 10 20198125029 treatment  -1.59   4.62 -11.5   -17.0   -6.08
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