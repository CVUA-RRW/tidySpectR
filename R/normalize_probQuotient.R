#' Probabilistic Quotient normalization
#' 
#' Probabilistic Quoteint Normalization estimates for each spectra the 
#'   the most likely quotient derived form the ratio of the signal distribution
#'   and that oif a reference (median) spectrum.
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
#' @export
#' @examples
#' library(tidySpectR)
#'
#' normalize_probQuotient(fa_nmr)
#' @importFrom dplyr select summarise starts_with across everything
#' @importFrom tidyr pivot_longer
normalize_probQuotient.collection <- function(x, ...){
    # Perform integral normalization
    norm <- normalize_totalSpectrum(x)
    
    # Calculate ref (median) spectrum
    ref <- median_spec(norm$data) %>%
           arrange("bins")
    
    # Calculate median quotient of all variables
    norm_mat <- data2wide(norm$data) %>%
                arrange(bins)
    quotients <- norm_mat %>% 
                 select(!starts_with("bin")) %>%
                 summarise(across(everything(), ~ 1/ median(.x  / ref$values, na.rm = TRUE))) %>%
                 pivot_longer(everything(), names_to = "id", values_to = "factors")

    # Normalize
    new_obj <- normalize_factor(x, quotients)
    new_obj$normalized <- "Probabilistic quotient"
    
    return(new_obj)
}

