#' Pivot_wider on collection$data
#' @param x data tibble
#' @param ... not currently used
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_wider
#' @keywords internal
data2wide <- function(x, names = 'id', values = 'values', ...) {
    pivot_wider(x, names_from = all_of(names), values_from = all_of(values))
} 

#' Pivot_longer on collection$data wide format
#' @param x wide data tibble
#' @param ... not currently used
#' @importFrom dplyr starts_with
#' @importFrom tidyr pivot_longer
#' @keywords internal
wide2long <- function(x, ...){
    pivot_longer(x,
        cols = -starts_with("bin"),
        names_to = "id",
        values_to = "values")
}

#' Median spectrum
#' @param x data tibble (long form)
#' @param ... not currently used
#' @keywords internal
#' @importFrom stats median
median_spec <- function(x, ...){
    x %>% 
    group_by(bins, bin_start, bin_end) %>%
    summarise(values = median(values), .groups= 'drop') %>%
    add_column(id = "median", .before = 1) %>% 
    mutate(id = as.factor(id)) %>%
    arrange("bins")
}