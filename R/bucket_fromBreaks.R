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
#' @importFrom dplyr mutate group_by summarise rowwise across
#' @importFrom tidyr separate
#' @export
#' @examples
#' library(tidySpectR)
#'
#' breaks = c(10, 8, 7.5, 7, 5, 0, -1)
#' bucket_from_breaks(fa_nmr, breaks)
bucket_from_breaks.collection <- function(x, breaks, ...){
# Adding limits if nescessary
    lowest <- x %>% 
                pull_limits() %>%
                min()
                
    highest <- x %>% 
                pull_limits() %>%
                max()
                
    if (!lowest %in% breaks){
        breaks <- append(breaks, lowest)
    }
    
    if (!highest %in% breaks){
        breaks <- append(breaks, highest)
    }
    
    breaks <- sort(breaks) %>% unique()
    
    # Bucket
    
    new_obj <- x
    
    dat <- x$data %>%
            data2wide()
            
    ### cut cuts the floats ! bugged bugged bugged FIXME
    bucketted <- dat %>%
                 mutate(bin_index = cut(bin_end, breaks, include.lowest = TRUE, right = FALSE, dig.lab = 12)) %>% 
                 group_by(bin_index) %>% 
                 summarise(across(-starts_with("bin"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
                 separate(bin_index, c(NA, "bin_start", "bin_end", NA), 
                          sep = "([,\\[\\]\\(\\)])",
                          remove = TRUE, 
                          convert = TRUE) %>%
                 rowwise() %>%
                 mutate(bins = mean(c(bin_start, bin_end)), .before = 1)

    new_obj$data <- wide2long(bucketted)
                    
    # Set bucketing flag
    new_obj$bucketted <- 'custom'
    
    return(new_obj)
}