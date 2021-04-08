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

#' Generate random id
#' @param id A base string to expand with random digits
#' @param ... currently unused
#' @importFrom stringr str_pad
#' @importFrom stats runif
#' @keywords internal
rand_id <- function(id, ...){
    paste0(
        id, 
        "_", 
        str_pad(
            floor(
                runif(1, min = 0, max = 999999)), 
        6, pad = "0"))
}