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
#' @param overlaps What to do with the bins ovelapping the edge of the mask.
#'   Use either `keep` of `remove`.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The upper limit in not included: [min, max).
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
#' @importFrom dplyr filter
#' @export
mask.collection <- function(obj, from, to, overlaps = 'keep', ...){

    if (!overlaps %in% c('keep', 'remove')){
        rlang::abort("Invalid value for `overlaps`")
    }
    
    lower <- min(from, to)
    higher <- max(from, to)
    
    new_obj <- obj
    
    if (overlaps == 'remove'){
        new_obj$data <- obj$data %>%
            filter(bin_end < lower | bin_start >= higher)
    } else if (overlaps == 'keep'){
        new_obj$data <- obj$data %>%
            filter(bin_start < lower | bin_end >= higher)
    }
    
    if (length(new_obj$data) == 0){
        rlang::warn("Returning an empty dataset")
    }
    
    return(new_obj)
}

#' Extracts a spectral region
#'
#' `extract` extracts a spectral region
#'
#' @aliases extract extract.collection
#' @export
extract <- function(...)
    UseMethod("extract")

#' @param obj A`collection` object
#' @param from,to Coordinates of the region to extract. 
#'   Use -Inf or Inf to extract up to an end of the spectra.
#' @param overlaps What to do with the bins ovelapping the edge of the mask.
#'   Use either `keep` of `remove`.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The upper limit in not included: [min, max).
#' @examples
#' sample <- nmr_spectra %>% filter(ID == "20198124123")
#' 
#' coll <- collection() %>%
#'      add_spectrum(sample$values, 
#'                   c(min(sample$bins), max(sample$bins)),
#'                   id = "20198124123")
#'
#' coll %>% extract(-2, 7.2)
#'
#' # The order is not important
#' coll %>% extract(7.2, -2)
#'
#' @rdname extract
#' @importFrom dplyr filter
#' @export
extract.collection <- function(obj, from, to, overlaps = 'keep', ...){

    if (!overlaps %in% c('keep', 'remove')){
        rlang::abort("Invalid value for `overlaps`")
    }
    
    lower <- min(from, to)
    higher <- max(from, to)
    
    new_obj <- obj
    
    if (overlaps == 'remove'){
        new_obj$data <- obj$data %>%
            filter(bin_end < higher & bin_start > lower)
    } else if (overlaps == 'keep'){
        new_obj$data <- obj$data %>%
            filter(bin_start < higher & bin_end > lower)
    }
    
    if (length(new_obj$data) == 0){
        rlang::warn("Returning an empty dataset")
    }
    
    return(new_obj)
}

