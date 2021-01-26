#' Normalizes spectra to their integrals
#'
#' Sums up the signal over the whole spectrum and use it as a 
#'   normalization factor. This assumes that the total signal 
#'   varies proportionally to the sample concentration
#'
#' @aliases normalize_totalSpectrum normalize_totalSpectrum.collection
#' @export
normalize_totalSpectrum <- function(x, ...)
    UseMethod("normalize_totalSpectrum")

#' @rdname normalize_totalSpectrum
#' @param x A`collection` object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @importFrom dplyr group_by summarise
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' normalize_totalSpectrum(fa_nmr)
normalize_totalSpectrum.collection <- function(x, ...){
    factors <- x$data %>%
                group_by(id) %>%
                summarise(factors = 1 / sum(values))
                
    new_obj <- x %>% normalize_factor(factors)
    
    new_obj$normalized <- "Total spectrum"
    
    return(new_obj)
}
