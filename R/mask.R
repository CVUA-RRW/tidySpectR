#' Masks a spectral region
#'
#' `mask` discards a spectral region
#'
#' @aliases mask mask.collection
#' @export
mask <- function(...)
    UseMethod("mask")

#' @param obj A`collection` object
#' @param from,to Coordinates of the region to mask. 
#'   Use -Inf or Inf to shorten the spectra.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The upper limit in not included: [min, max). 
#'   Bins that overlap on the limit will be removed.
#' @examples
#' sample <- nmr_spectra %>% filter(ID == "20198124123")
#' 
#' coll <- collection() %>%
#'      add_spectrum(sample$values, 
#'                   c(min(sample$bins), max(sample$bins)),
#'                   id = "20198124123")
#'
#' coll %>% mask(-2, 7.2)
#'
#' # The order is not important
#' coll %>% mask(7.2, -2)
#'
#' # Cropping edges
#' cropped <- coll %>%
#'              mask(-Inf, 0) %>%
#'              mask(Inf, 7.5)
#'
#' @rdname mask
#' @export
mask.collection <- function(obj, from, to, ...){
    lower <- min(from, to)
    higher <- max(from, to)
    
    if (lower == higher){
        rlang::abort("The given limits will results in an empty dataset")
    }
    
    new_obj <- obj
    
    new_obj$data <- obj$data %>%
            filter(bin_end < lower | bin_start >= higher)
    
    return(new_obj)
}