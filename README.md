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
#> Number of bins: 2250
#> Normalized: FALSE
#> Bucketted: FALSE
#> Labels: conventional organic

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

# Just checking the first five columns
print(processed[,1:5]) 
#> 
#> # A tibble: 6 x 5
#>   id         label      `0.51997876833874… `0.55997876833874… `0.59997876833874…
#>   <fct>      <fct>                   <dbl>              <dbl>              <dbl>
#> 1 201993059… conventio…             0.0353             0.0385             0.0442
#> 2 201993062… conventio…             0.0388             0.0414             0.0481
#> 3 201995046… conventio…             0.0259             0.0307             0.0348
#> 4 201981241… organic                0.0381             0.0398             0.0451
#> 5 201981241… organic                0.0311             0.0329             0.0425
#> 6 201981241… organic                0.0385             0.0403             0.0470


# Pre-processing for data analysis using the `recipe` package
nmr_preprocessing <- recipe(processed, label ~.) %>%
                     update_role(id, new_role = "ID") %>% 
                     # Pareto-VAST scaling
                     step_vast(all_predictors(), scaling = 'pareto') %>%
                     # PCA transformation (from the recipes package)
                     step_pca(all_predictors(), num_comp = 5)

# Check the data 
nmr_preprocessing %>% prep() %>% juice()
#> 
#> # A tibble: 6 x 7
#>   id          label          PC1   PC2   PC3    PC4     PC5
#>   <fct>       <fct>        <dbl> <dbl> <dbl>  <dbl>   <dbl>
#> 1 20199305928 conventional -52.1  38.3  50.2  29.4    1.77
#> 2 20199306281 conventional -81.3 -35.1 -28.7   3.52 -28.6
#> 3 20199504645 conventional  25.5  76.9 -37.8 -15.9   -2.13
#> 4 20198124123 organic       23.3 -21.0  52.6 -34.6   -2.98
#> 5 20198124124 organic      146.  -28.8 -10.6  19.9   -0.771
#> 6 20198124125 organic      -61.1 -30.3 -25.7  -2.23  32.8
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