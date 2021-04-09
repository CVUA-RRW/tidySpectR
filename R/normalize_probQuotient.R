#' Probabilistic Quotient normalization
#' 
#' Probabilistic Quoteint Normalization estimates for each spectra the 
#'   the most likely quotient derived form the ratio of the signal distribution
#'   and that of a reference spectrum.
#'
#' @aliases normalize_probQuotient normalize_ProbQuotient.collection
#' @export
#' @references
#' Dieterle, F., Ross, A., Schlotterbeck, G., & Senn, H. (2006). Probabilistic 
#' Quotient Normalization as Robust Method to Account for Dilution of Complex 
#' Biological Mixtures. Application in1H NMR Metabonomics. Analytical Chemistry, 
#' 78(13), 4281–4290. doi:10.1021/ac051632c 
#' \url{https://pubmed.ncbi.nlm.nih.gov/16808434/}
#' @param x A`collection` object to normalize
#' @param ... further arguments passed to or from other methods(not
#'   currently used).
normalize_probQuotient <- function(x, ...)
    UseMethod("normalize_probQuotient")

#' @rdname normalize_probQuotient
#' @param x A`collection` object
#' @param ref A reference spectra. Will use the median spectrum of the 
#'   collection if set to `NULL`.
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @export
#' @examples
#' library(tidySpectR)
#'
#' normalize_probQuotient(fa_nmr)
#'
#' # Provifding a reference spectrum
#' med <- median_spectrum(fa_nmr)
#' normalize_probQuotient(fa_nmr, ref = med)
#'
#' @importFrom dplyr select summarise starts_with across everything
#' @importFrom tidyr pivot_longer
normalize_probQuotient.collection <- function(x, ref = NULL, skip = FALSE, ...){
    # Perform integral normalization
    norm <- normalize_totalSpectrum(x)
    
    # Calculate ref (median) spectrum
    if (is.null(ref)){
        ref <- median_spectrum(norm) 
    }
    
    # Calculate median quotient of all variables
    norm_mat <- data2wide(norm$data) %>%
                arrange(bins)
    quotients <- norm_mat %>% 
                 select(!starts_with("bin")) %>%
                 summarise(across(everything(), ~ 1/ median(.x  / ref$values, na.rm = TRUE))) %>%
                 pivot_longer(everything(), names_to = "id", values_to = "factors")

    # Normalize
    new_obj <- normalize_factor(x, quotients, skip = TRUE)
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(normalize_factor, 
                                      list(factors = quotients), 
                                      name = "probabilisticQuotient_normalization")
    }
    
    return(new_obj)
}

