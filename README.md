# tidySpectR

![R-CMD-check](https://github.com/CVUA-RRW/tidySpectR/workflows/R-CMD-check/badge.svg)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


## Introduction

The aim of this package is to provide tidy methods for the 
analysis of spectral dataset in order to improve 
accessibility and reproducibility of chemometric analysis.

This package is meant to fill the gap between data acquisition 
and preprocessing (phasing, baseline correction) and modelling
with the tidymodels metapackage.

It provides a framework for processing and analysis of spectral data:
 * A `collection` class storing spectra informations and providing methods for 
spectral region selection, normalization and bucketting.
 * A `processing_template` class storing processing steps and allowing to repeat
these steps on other `collection` objects.
 * A few `step_` methods implementing some data preprocessing methods for modelling and/or 
multivariate data analysis, to use with the [tidymodels recipe package](https://github.com/tidymodels/recipes). 

## Installation

This package is not yet on CRAN, install from github:

```r
require("devtools")
install_github("CVUA-RRW/tidySpectR")
```

## Quickstart

### Processing spectral data

```r
library(tidyverse)
library(tidySpectR)
library(tidymodels)

data(fa_nmr)

print(fa_nmr)
#> Spectra collection containing 6 entries.
#> Number of bins: 2250
#> Limits: -2.123166e-05 8.999979
#> Labels: conventional organic
#> 
#> Processing:

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
             normalize_internalStandard(from = 3.58, to = 3.64) 

print(processed)
#> Spectra collection containing 6 entries.
#> Number of bins: 166
#> Limits: 0.4999788 7.203979
#> Labels: conventional organic
#> 
#> Processing:
#> Step 1 / 5 : extract
#> Step 2 / 5 : mask
#> Step 3 / 5 : mask
#> Step 4 / 5 : uniform_binning
#> Step 5 / 5 : internalStandard_normalization
```

### Reproducing processing on new data

```recipe
# Export processor
processor <- export_processor(processed)

# Apply processor on new data
new_data <- process(processor, fa_nmr)

print(new_data)
#> Spectra collection containing 6 entries.
#> Number of bins: 166
#> Limits: 0.4999788 7.203979
#> Labels: conventional organic
#> 
#> Processing:
#> Step 1 / 5 : extract
#> Step 2 / 5 : mask
#> Step 3 / 5 : mask
#> Step 4 / 5 : cutsom_bucketting
#> Step 5 / 5 : factor_normalization
```

### Streamlining to tidymodels

```r
# Exporting processed spectra as a data matrix
df <- tidy(processed)

print(df[,1:5]) 
#> # A tibble: 6 x 5
#>   id         label      `0.51997876833874… `0.55997876833874… `0.59997876833874…
#>   <chr>      <fct>                   <dbl>              <dbl>              <dbl>
#> 1 201981241… organic                 0.676              0.658               4.63
#> 2 201981241… organic                 0.722              0.711               5.20
#> 3 201981241… organic                 0.588              0.544               3.90
#> 4 201993059… conventio…              0.631              0.657               4.74
#> 5 201993062… conventio…              0.565              0.540               4.11
#> 6 201995046… conventio…              0.689              0.671               5.31

# Pre-processing for data analysis using the `recipe` package
nmr_preprocessing <- recipe(df, label ~.) %>%
                     update_role(id, new_role = "ID") %>% 
                     # Pareto-VAST scaling - Implemented in this package
                     step_vast(all_predictors(), scaling = 'pareto') %>%
                     # PCA transformation (from the recipes package)
                     step_pca(all_predictors(), num_comp = 5)

# Check the data 
nmr_preprocessing %>% prep() %>% juice()
#> # A tibble: 6 x 7
#>   id          label            PC1    PC2    PC3    PC4    PC5
#>   <fct>       <fct>          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 20198124123 organic       -0.234   8.81 10.4   -0.197  4.37
#> 2 20198124124 organic      -39.6     9.30 -9.34  -2.50   0.787
#> 3 20198124125 organic       32.6    -1.86  0.172 -9.63  -2.44
#> 4 20199305928 conventional   5.01    6.60  2.28   6.68  -5.72
#> 5 20199306281 conventional  37.7    -5.63 -6.97   5.22   3.38
#> 6 20199504645 conventional -35.4   -17.2   3.43   0.440 -0.371
```

## Contributions

Contributions are very welcome!
If you would like to see new functionnalities submit an issue directly 
on github or start a PR.