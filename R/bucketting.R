#' Bucket a collection form a list of breaks (bucket limits).
#'   For internal use
#'
#' @aliases bucket_from_breaks bucket_from_breaks.collection
bucket_from_breaks <- function(...)
    UseMethod("bucket_from_breaks")
    
#' @param obj A`collection` object
#' @param breaks A vector of breaking values
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details If omitted the spectra limits will be added o the breaks
#'
#' Buckets will include the bin overlapping on the lower limit and exclude the bin 
#'   overlapping on the higher limit.
#' 
#' @rdname bucket_from_breaks
#' @importFrom dplyr arrange
#' @importFrom purrr map2_dfr
bucket_from_breaks.collection <- function(obj, breaks, ...){
    # Adding limits if nescessary
    lowest <- obj %>% 
                pull_breaks() %>%
                min()
                
    highest <- obj %>% 
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
    
    new_obj <- obj
    new_obj$data <- map2_dfr(bin_start, 
                             bin_end,
                             bin_sum,
                             obj$data) %>% 
                    arrange(id, bins)
    
    # Set bucketing flag
    new_obj$bucketted <- 'custom'
    
    return(new_obj)
}

#' Sums values over a single bin
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

#' Performs uniform bucketting
#'
#' Bucket a colleciton of spectra to a given bin width or
#'   number of bins
#'
#' @aliases bucket_uniform bucket_uniform.collection
#' @export
bucket_uniform <- function(...)
    UseMethod("bucket_uniform")

#' @param obj A`collection` object
#' @param width Bin width
#' @param N Number of bins
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details Provide either `width` or `N`
#' 
#' @rdname bucket_uniform
#' @importFrom dplyr arrange
#' @importFrom purrr map2_dfr
#' @export
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
        breaks <- seq(lowest, highest, length.out = N)
        bucketting <- paste0("Uniform (number=", as.character(N), ")")
    }
    
    new_obj <- obj %>%
               bucket_from_breaks(breaks)
    
    new_obj$bucketted <- bucketting
    
    return(new_obj)
}

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

bucket_optimized <- function(...)
    UseMethod("bucket_uniform")

#' @param obj A`collection` object
#' @param initial_width Initial bin width to optimize
#' @param slackness fraction of initial width that defines 
#'   how far the boundary can move while searching for a local minimum
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details The Optimized Bucketting Algorithm optimized bucket size by 
#'   searching local minima in the average spectrum.
#' 
#' @rdname bucket_optimized
#' @importFrom dplyr last first
#' @importFrom purrr map_dbl
#' @export
bucket_optimized.collection <- function(obj, initial_width, slackness, ...){
    average <- obj %>% 
               average_spectrum(group = "all")
    
    # J is the number of points
    J <- average$data %>% nrow()
    
    # distance between points
    sampling_interval <- (last(data$bins) -first(data$bins)) / (J - 1) 
    
    # N is the number of points per initial bucket
    N <- as.integer(initial_width / sampling_interval)
    
    # Calculating slackness
    s <- as.integer(N * slackness)
    
    # T is the bucket counter
    T <- 1:(J/N-1) 
    
    # Finding local minima
    breaks <- map_dbl(T, find_local_min, data, N, s)
    
    new_obj <- obj %>%
               bucket_from_breaks(breaks)
    
    new_obj$bucketted <- paste0("Optimized Bucketting (width=", initial_width, ", slackness=", slackness, ")")
    
    return(new_obj)
}

#' Find local minimum between two boundaries
#' @importFrom dplyr slice 
find_local_min <- function(i, data, N, s){
    start <- N*i-s
    end <- N*i+s-1 # correcting for R indexing
    # Return local minimum position 
    data %>% 
    slice(start:end) %>%
    slice(which.min(values)) %>%
    .$bins
}
