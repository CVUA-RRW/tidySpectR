#' Performs uniform bucketting
#'
#' Bucket a collection of spectra to a given bin width or
#'   number of bins
#'
#' @aliases bucket_uniform bucket_uniform.collection
#' @export
bucket_uniform <- function(...)
    UseMethod("bucket_uniform")

#' @rdname bucket_uniform
#' @param obj A`collection` object
#' @param width Bin width
#' @param N Number of bins to create
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details Provide either `width` or `N`
#' 
#' @importFrom dplyr arrange
#' @importFrom purrr map2_dfr
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' bucket_uniform(fa_nmr, width= 0.04)
#'
#' bucket_uniform(fa_nmr, N = 1500)
bucket_uniform.collection <- function(obj, width = NULL, N = NULL, ...){
    # getting spectra limits
    lowest <- obj %>% 
                pull_breaks() %>%
                min()
                
    highest <- obj %>% 
                pull_breaks() %>%
                max()
    
    # Calculating breaks
    if (!is.null(width) & !is.null(N)){
        rlang::abort("Provide either `width`or `N`: you provided both")
    } else if (is.null(width) & is.null(N)){
        rlang::abort("Provide either `width`or `N`: you provided neither")
    } else if (!is.null(width)){
        breaks <- seq(lowest, highest, by = width)
        bucketting <- paste0("Uniform (width=", as.character(width), ")")
    } else if (!is.null(N)){
        breaks <- seq(lowest, highest, length.out = N + 1)
        bucketting <- paste0("Uniform (number=", as.character(N), ")")
    }
    
    new_obj <- obj %>%
               bucket_from_breaks(breaks)
    
    new_obj$bucketted <- bucketting
    
    return(new_obj)
}