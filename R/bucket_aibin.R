#' Applies Adaptive Inteligent Binning on a collection
#' 
#' Adaptive Inteligent Binning (aibin) recursively finds bin edges 
#'   in existing bins by maximizing the information contained in each 
#'   newly created bin.
#' @aliases bucket_aibin bucket_aibin.collection
#' @export
#' @details 
#' AI binning will first consider the entire spectra as a single bin.
#'   It will then consider every position in the spectra as a potential
#'   splitting candidate and calulate a quality value for each splitting candidate.
#'   If the best splitting candidates fulfills quality requirements, the bin 
#'   will be split and the process will be repeated on the newly created bins.
#'
#' In order to limit splitting in local minima of noisy regions a minimal quality
#'   value is computed from the user-provided noise-region.
#' 
#' @references
#' De Meyer T, Sinnaeve D, Van Gasse B, Tsiporkova E, Rietzschel ER, 
#' De Buyzere ML, Gillebert TC, Bekaert S, Martins JC, Van Criekinge W. 
#' NMR-based characterization of metabolic alterations in hypertension 
#' using an adaptive, intelligent binning algorithm. Anal Chem. 2008 
#' May 15;80(10):3783-90. doi: 10.1021/ac7025964. 
#' \url{https://pubmed.ncbi.nlm.nih.gov/18419139/}
bucket_aibin <- function(x, ...)
    UseMethod("bucket_aibin")

#' @rdname bucket_aibin
#' @param x A`collection` object to bucket
#' @param R resolution value, strictly positive and typically in the interval
#'   0 > R >= 1. 
#' @param noise_region A`collection` object containing a noise_region
#' @param snr Signal to noise ratio for bucketting. snr should a real >=1.
#'   Factor by which to multiply the noise bin value. Can be useful to limit overbucketing 
#'   of high-resolution data or in case of very smooth and low noise.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @returns An updated version of x
#' @note The amount of created bins is not directly depending of a high or low R.
#'   If a maximal splitting is looked for, you should try several different values 
#'   in the possible rangeof R (see references). However the differences in binning should
#'   limited to bins that are borderline noise and most likely has limited effect in 
#'   the downstream analysis.
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' # First we normalize the dataset and extract noise and sepctra regions
#' normalized <- fa_nmr %>% 
#'                   normalize_internalStandard(3.58, 3.64)
#' spectra <- normalized %>% extract(0.5, 7.2)
#' noise <- normalized %>% extract(7.5, Inf)
#' 
#' bucketted <- bucket_aibin(spectra, 0.5, noise)
#' bucketted
#'
#' @importFrom dplyr arrange
bucket_aibin.collection <- function(x, R, noise_region, snr = 1,...){
    # Convert data to proper format
    noise_region <- noise_region$data %>%
                    data2wide() %>%
                    arrange(bins) %>%
                    as.matrix()
    
    spectra <- x$data %>%
               data2wide() %>%
               arrange(bins) %>%
               as.matrix()
    
    # Call Cpp function
    breaks <- aibin_cpp(spectra, noise_region, R)
    
    # Convert indices to values
    breaks <- spectra[breaks+1, 'bin_end'] # C++ indices are 0-based
    
    # Applying buckets
    new_obj <- bucket_from_breaks(x, breaks)
    new_obj$bucketted <- "Adaptive inteligent binning"

    return(new_obj)
}