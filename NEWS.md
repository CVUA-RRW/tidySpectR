# tidySpectR (development version)

## Version 0.2.0
 * Added the `processing_step` class and associated methods to store 
function calls to processing steps of `collection` objects.

## Version 0.1.4

 * Fixed an issue caused by large slackness in `bucket_optimized`
 * Added probabilistic quotient normalization method
 * `add_labels` now only add labels to existing IDs
 * Added a SNR argument to `bucket_aibin` to eliminate over-bucketting of noise in the spectral regions
 * Supressed printing of QC results inherited from the `opls`function in the `step_opls_denoise` method
 * Added `median_spectrum` method

## Version 0.1.3

 * Ported bucket_aibin code to C++ for performance increase

## Version 0.1.2

 * Added a `subsample´ method to extract a subset of spectra from a collection
 * Added the `pull_limits`getter to retrieve X-axis limits
 * Added `step_opls_denoise` to filter structural noise

## Version 0.1.1

 * Added `add_labels` to reference
 * Fixed `normalize_aibin`
 * All `bucket_` methods now support multiprocessing. 
 * In order to better integrate multiprocessing, Cluster definition has been removed from ther methods body. Cluster now need to be defined manually be the user (see for example `bucket_aibin` documentation.

## Version 0.1.0

First release!