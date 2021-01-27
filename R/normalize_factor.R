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
#' @param factors A tibble with columns `id` and `factor`
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
#' norm <- tibble(id = pull_ids(fa_nmr), factors = runif(10))
#'
#' normalize_factor(fa_nmr, norm)
normalize_factor.collection <- function(x, factors, ...) {
    new_obj <- x
    
    new_obj$data <- x$data %>%
                    group_by(id) %>%
                    group_modify(function(x, y, factors) {
                            f <- factors %>% 
                                 filter(id == pull(y, id)) %>%
                                 pull(factors)

                            x %>% mutate(values = x$values * f)
                        }, factors)
    
    new_obj$normalized <- "Custom factors"
    
    return(new_obj)
}
