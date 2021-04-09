#' Masks a spectral region
#'
#' `mask` discards a spectral region
#'
#' @aliases mask mask.collection
#' @export
mask <- function(x, ...)
    UseMethod("mask")

#' @rdname mask
#' @param x A`collection` object
#' @param from,to Coordinates of the region to mask. 
#'   Use -Inf or Inf to shorten the spectra.
#' @param overlaps What to do with the bins ovelapping the edge of the mask.
#'   Use either `keep` of `remove`.
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The upper limit in not included: [min, max).
#'
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' # Trimming one side
#' mask(fa_nmr, from = Inf, to = 7.2)
#'
#' # Masking region
#' mask(fa_nmr, from = 3.5, to = 3, overlaps = 'remove')
mask.collection <- function(x, from, to, overlaps = 'keep', skip = FALSE, ...){

    if (!overlaps %in% c('keep', 'remove')){
        rlang::abort("Invalid value for `overlaps`")
    }
    
    lower <- min(from, to)
    higher <- max(from, to)
    
    new_obj <- x
    
    if (overlaps == 'remove'){
        new_obj$data <- x$data %>%
            filter(bin_end < lower | bin_start >= higher)
    } else if (overlaps == 'keep'){
        new_obj$data <- x$data %>%
            filter(bin_start < lower | bin_end >= higher)
    }
    
    if (length(new_obj$data) == 0){
        rlang::warn("Returning an empty dataset")
    }
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(mask, 
                                      list(from = from,
                                           to = to,
                                           overlaps = overlaps), 
                                      name = "mask")
    }
    
    return(new_obj)
}

#' Extracts a spectral region
#'
#' `extract` extracts a spectral region
#'
#' @aliases extract extract.collection
#' @export
extract <- function(x, ...)
    UseMethod("extract")

#' @rdname extract
#' @param x A`collection` object
#' @param from,to Coordinates of the region to extract. 
#'   Use -Inf or Inf to extract up to an end of the spectra.
#' @param overlaps What to do with the bins ovelapping the edge of the mask.
#'   Use either `keep` of `remove`.
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The upper limit in not included: [min, max).
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(tidySpectR)
#'
#' # Removing edges
#' extract(fa_nmr, from = 7.2, to = -0.5)
extract.collection <- function(x, from, to, overlaps = 'keep', skip = FALSE, ...){

    if (!overlaps %in% c('keep', 'remove')){
        rlang::abort("Invalid value for `overlaps`")
    }
    
    lower <- min(from, to)
    higher <- max(from, to)
    
    new_obj <- x
    
    if (overlaps == 'remove'){
        new_obj$data <- x$data %>%
            filter(bin_end < higher & bin_start > lower)
    } else if (overlaps == 'keep'){
        new_obj$data <- x$data %>%
            filter(bin_start < higher & bin_end > lower)
    }
    
    if (length(new_obj$data) == 0){
        rlang::warn("Returning an empty dataset")
    }
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(extract, 
                                      list(from = from,
                                           to = to,
                                           overlaps = overlaps), 
                                      name = "extract")
    }
    
    return(new_obj)
}

