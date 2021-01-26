#' Normalizes spectra to a given factor
#'
#' Intensities will be multiplies by the given factor.
#'
#' @aliases normalize_factor normalize_factor.collection
#' @export
normalize_factor <- function(...)
    UseMethod("normalize_factor")

#' @rdname normalize_factor
#' @param factors A tibble with columns `id` and `factor`
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details Will mutliply the spectra by the factors given 
#'   as arguments.
#'
#' Missing ids will not be normalized
#'
#' Direct use is not recommended, see other normalization methods.
#' @importFrom dplyr group_by group_modify filter pull mutate
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' norm <- tibble(id = pull_ids(fa_nmr), factors = runif(10))
#'
#' normalize_factor(fa_nmr, norm)
normalize_factor.collection <- function(obj, factors, ...) {
    new_obj <- obj
    
    new_obj$data <- obj$data %>%
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
