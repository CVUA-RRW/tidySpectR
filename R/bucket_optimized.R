#' Performs optimized bucketting
#'
#' Bucket a collection of spectra using the Optimized
#'   Bucketting Algorithm
#'
#' @aliases bucket_optimized bucket_optimized.collection
#' @references
#' Sousa SAA, Magalhaes A, Ferreira MMC. Optimized bucketing for NMR spectra: 
#' Three case studies. Chemometr Intell Lab. 2013;122:93–102. 
#' https://doi.org/10.1016/j.chemolab.2013.01.006
#' \url{https://doi.org/10.1016/j.chemolab.2013.01.006}
#' 
#' @export
bucket_optimized <- function(x, ...)
    UseMethod("bucket_optimized")

#' @rdname bucket_optimized
#' @param x A`collection` object
#' @param initial_width Initial bin width to optimize
#' @param slackness fraction of initial width that defines 
#'   how far the boundary can move while searching for a local minimum
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The Optimized Bucketting Algorithm optimizes bucket size by 
#'   searching local minima in the average spectrum.
#' 
#' @importFrom dplyr last first pull
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' \dontrun{
#' library(tidySpectR)
#'
#' bucket_optimized(fa_nmr, initial_width = 0.01, slackness = 0.5)
#' }
bucket_optimized.collection <- function(x, initial_width, slackness, ...){
    average <- x %>% 
               average_spectrum(group = "all")
    
    # J is the number of points
    J <- average$data %>% nrow()
    
    # distance between points
    sampling_interval <- (last(x$data$bins) -first(x$data$bins)) / (J - 1) 
    
    # N is the number of points per initial bucket
    N <- as.integer(initial_width / sampling_interval)
    
    # Calculating slackness
    s <- as.integer(N * slackness)
    
    # T is the bucket counter
    T <- 1:(J/N-1) 
    
    # Finding local minima
    breaks <- map_dfr(T, find_local_min, x$data, N, s) %>%
              pull(bins)
    
    new_obj <- x %>%
               bucket_from_breaks(breaks)
    
    new_obj$bucketted <- paste0("Optimized Bucketting (width=", initial_width, ", slackness=", slackness, ")")
    
    return(new_obj)
}

#' Find local minimum between two boundaries
#'
#' For internal use only.
#'
#' @param i A counter
#' @param data A dataframe with columns values
#' @param N number of point per bin (initial)
#' @param s slackness
#' @importFrom dplyr slice slice_min
find_local_min <- function(i, data, N, s){
    start <- N*i-s
    end <- N*i+s-1 # correcting for R indexing
    # Return local minimum position 
    data %>% 
    slice(start:end) %>%
    slice_min(order_by = values, n=1, with_ties = FALSE)
}
