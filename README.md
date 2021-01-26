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

```
library(tidySpectR)
library(tidymodels)

data(fa_nmr)

# H1-NMR of egg yolk faty acids
print(fa_nmr)

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

print(processed)

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
```

## Installation

This package is not yet on CRAN, install from github:

```
require("devtools")
install_github("CVUA-RRW/tidySpectR")
```

## Contributions

Contributions are very welcome!
If you would like to see new functionnalities submit an issue directly 
on github or start a PR.