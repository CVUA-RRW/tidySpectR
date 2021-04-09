#' Normalizes spectra to a given factor
#'
#' Intensities will be multiplied by the given factor.
#'
#' @aliases normalize_factor normalize_factor.collection
#' @export
normalize_factor <- function(x, ...)
    UseMethod("normalize_factor")

#' @rdname normalize_factor
#' @param x A`collection` object
#' @param factors A tibble with columns `id` and `factors`
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details Will mutliply the spectra by the factors given 
#'   as arguments.
#'
#' Missing ids will not be normalized
#'
#' This method should be used to bucket new data based on the results from 
#'   a previous dataset.
#' @importFrom dplyr group_by group_modify filter pull mutate
#' @export
#' @examples
#' library(tidySpectR)
#' library(tibble)
#' 
#' ids <- pull_ids(fa_nmr)
#' norm <- tibble(id = ids, factors = runif(length(ids)))
#'
#' normalize_factor(fa_nmr, norm)
normalize_factor.collection <- function(x, factors, skip = FALSE, ...) {
    new_obj <- x
    
    new_obj$data <- x$data %>%
                    group_by(id) %>%
                    group_modify(function(x, y, factors) {
                            f <- factors %>% 
                                 filter(id == pull(y, id)) %>%
                                 pull(factors)

                            x %>% mutate(values = x$values * f)
                        }, factors) %>% 
                    ungroup()
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(normalize_factor, 
                                      list(factors = factors), 
                                      name = "factor_normalization")
    }
    
    return(new_obj)
}
