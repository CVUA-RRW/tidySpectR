#' Performs uniform bucketting
#'
#' Bucket a collection of spectra to a given bin width or
#'   number of bins
#'
#' @aliases bucket_uniform bucket_uniform.collection
#' @export
bucket_uniform <- function(x, ...)
    UseMethod("bucket_uniform")

#' @rdname bucket_uniform
#' @param x A`collection` object
#' @param width Bin width
#' @param N Number of bins to create
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.
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
#' bucket_uniform(fa_nmr, width= 0.1)
#'
#' bucket_uniform(fa_nmr, N = 100)
bucket_uniform.collection <- function(x, width = NULL, N = NULL, skip = FALSE, ...){
    # getting spectra limits
    lowest <- x %>% 
                pull_breaks() %>%
                min()
                
    highest <- x %>% 
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
    
    new_obj <- bucket_from_breaks(x, breaks, skip = TRUE)
    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(bucket_from_breaks, 
                                      list(breaks = breaks), 
                                      name = "uniform_binning")
    }
    
    return(new_obj)
}