#' Bucket a collection form a list of breaks
#'
#' Creates new bins delimited by the input breaks and sums up the intensity
#'   values within each bin.
#'
#' @aliases bucket_from_breaks bucket_from_breaks.collection
#' @export
bucket_from_breaks <- function(x, ...)
    UseMethod("bucket_from_breaks")

#' @rdname bucket_from_breaks
#' @param x A`collection` object
#' @param breaks A vector of breaking values
#' @param skip Skip the creation of of processor step. If TRUE, this step will not be added to
#'   the list of processing steps. Typically reserved for nested function calls.

#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details If omitted, the spectra limits will be added to the breaks.
#'
#' Bins will be grouped considering their center, inclusion criteria is
#'   (lower_limit, higher_limit].
#' 
#' @importFrom dplyr mutate group_by summarise rowwise across
#' @importFrom tidyr separate
#' @export
#' @examples
#' library(tidySpectR)
#'
#' breaks = c(10, 8, 7.5, 7, 5, 0, -1)
#' bucket_from_breaks(fa_nmr, breaks)
bucket_from_breaks.collection <- function(x, breaks, skip = FALSE, ...){
    # tol <- options()$digits
    # Adding limits if nescessary
    x_breaks <- pull_breaks(x)
    x_min <- min(x_breaks)
    x_max <- max(x_breaks)
    
    breaks <- sort(breaks)
    
    if (min(x_breaks) != first(breaks)){
        breaks <- append(breaks, min(x_breaks))
    }
    
    if (max(x_breaks) != last(breaks)){
        breaks <- append(breaks, max(x_breaks))
    }
    
    breaks <- sort(breaks[breaks >= x_min & breaks <= x_max],
                   decreasing = FALSE) %>% 
              unique()
    
    # Bucket
    new_obj <- x
    
    dat <- x$data %>%
            data2wide()
            
    bucketted <- dat %>% 
                rowwise() %>%
                mutate(bin_start = max(breaks[breaks <= bins]),
                       bin_end = min(breaks[breaks > bins])) %>%
                mutate(bins = mean(c(bin_start, bin_end))) %>%
                group_by(bins, bin_start, bin_end) %>%
                summarise(across(-starts_with("bin"), ~sum(.x, na.rm = TRUE)), .groups = "drop")
           
    new_obj$data <- wide2long(bucketted)
                    
    # Add processing step 
    if (!skip){
        new_obj$processor <- new_obj$processor %>%
                             new_step(bucket_from_breaks, 
                                      list(breaks = breaks), 
                                      name = "cutsom_bucketting")
    }
    
    return(new_obj)
}