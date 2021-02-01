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
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details If omitted, the spectra limits will be added to the breaks.
#'
#' Buckets will include the bin overlapping on the lower limit and exclude the bin 
#'   overlapping on the higher limit. This can results in a slight left-shift of the spectra. 
#'   Use with care.
#' 
#' @importFrom dplyr arrange
#' @importFrom purrr map2_dfr
#' @importFrom furrr future_map2_dfr
#' @export
#' @examples
#' library(tidySpectR)
#'
#' breaks = c(10, 8, 7.5, 7, 5, 0, -1)
#' bucket_from_breaks(fa_nmr, breaks)
bucket_from_breaks.collection <- function(x, breaks, ...){
    # Adding limits if nescessary
    lowest <- x %>% 
                pull_breaks() %>%
                min()
                
    highest <- x %>% 
                pull_breaks() %>%
                max()
                
    if (!lowest %in% breaks){
        breaks <- append(breaks, lowest)
    }
    
    if (!highest %in% breaks){
        breaks <- append(breaks, highest)
    }
    
    breaks <- sort(breaks)
    
    # Bucket
    bin_start <- breaks[-length(breaks)]
    bin_end <- breaks[2:length(breaks)]
    
    new_obj <- x
    new_obj$data <- future_map2_dfr(bin_start, 
                                     bin_end,
                                     bin_sum,
                                     x$data) %>% 
                            arrange(id, bins)
    
    # Set bucketing flag
    new_obj$bucketted <- 'custom'
    
    return(new_obj)
}

#' @importFrom dplyr filter group_by summarise
#' @importFrom tibble add_column
bin_sum <- function(lower, higher, data){
    data %>% 
    filter(bin_end >= lower & bin_end < higher) %>%
    group_by(id) %>%
    summarise(values = sum(values), .groups = "drop") %>%
    add_column(bins = mean(c(lower,higher)), .after = 1) %>%
    add_column(bin_start = lower, .after = 2) %>%
    add_column(bin_end = higher, .after = 3) 
}
