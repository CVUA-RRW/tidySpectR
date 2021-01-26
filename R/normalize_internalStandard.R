#' Normalizes spectra to an internal standard
#'
#' The integral of the spectrum between the given limits
#'   will be used as a normalization factor. This assumes that
#'   the selected peak intensity in independent of the experimental conditions.
#'
#' @aliases normalize_internalStandard normalize_internalStandard.collection
#' @export
normalize_internalStandard <- function(x, ...)
    UseMethod("normalize_internalStandard")

#' @rdname normalize_internalStandard
#' @param x A`collection` object
#' @param from,to Coordinates of the region to use as standard. 
#' @param ... arguments to be passed to `extract`
#' @return An updated version of `collection`.
#' @importFrom dplyr group_by summarise
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' normalize_internalStandard(fa_nmr, from = 3, to = 3.5)
normalize_internalStandard.collection <- function(x, from, to, ...){
    factors <- x %>% 
               extract(from, to, ...) %>%
               .$data %>%
               group_by(id) %>%
               summarise(factors = 1 / sum(values))
               
    new_obj <- x %>% normalize_factor(factors)
    
    new_obj$normalized <- paste0("Internal standard (", from, "-", to, ")")
    
    return(new_obj)
}