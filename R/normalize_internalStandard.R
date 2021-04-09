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
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
#' @param ... arguments to be passed to `extract`
#' @return An updated version of `collection`.
#' @importFrom dplyr group_by summarise
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' normalize_internalStandard(fa_nmr, from = 3, to = 3.5)
normalize_internalStandard.collection <- function(x, from, to, skip = FALSE, ...){
    factors <- x %>% 
               extract(from, to, ...) %>%
               .$data %>%
               group_by(id) %>%
               summarise(factors = 1 / sum(values))
               
    new_obj <- normalize_factor(x, factors, skip = TRUE)
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(normalize_factor, 
                                      list(factors = factors), 
                                      name = "internalStandard_normalization")
    }
    
    return(new_obj)
}