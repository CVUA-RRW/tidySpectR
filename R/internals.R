#' Pivot_wider on collection$data
#' @param x data tibble
#' @param ... not currently used
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_wider
#' @keywords internal
data2wide <- function(x, ids = 'id', ...) {
    pivot_wider(x, names_from = all_of(ids), values_from = values)
} 

#' Pivot_wider on collection$data
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