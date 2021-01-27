#' Tidy a spectra collection
#'
#' `tidy` will return a dataframe containing the spectral 
#'   information in its current state.
#'
#' @name tidy.collection
#'
#' @param x A `collection` object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble with a variable number columns
#'   depending on the binning of the data:
#'   \item{id}{Unique sample identifier}
#'   \item{label}{Label for the sample}
#'   \item{bins}{Bin centers}
#' @importFrom dplyr inner_join relocate select mutate
#' @importFrom tidyr pivot_wider
#' @examples
#' library(tidySpectR)
#'
#' tidy(fa_nmr)
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname tidy.collection
#' @export
tidy.collection <- function(x, ...){
    x$data %>%
        select(id, bins, values) %>%
        mutate(bins = as.factor(bins)) %>%
        pivot_wider(names_from = bins, values_from = values) %>%
        inner_join(x$labels, by = 'id') %>%
        relocate(label, .after = 1)
}

