#' Calculate the average spectrum 
#'
#' Averages spectra 
#' 
#' @aliases average_spectrum average_spectrum.collection
#' @export
average_spectrum <- function(...)
    UseMethod("average_spectrum")

#' @rdname average_spectrum
#' @param obj A`collection` object
#' @param group How to perform grouping. Either `all`or `labels`
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#'
#' @importFrom dplyr group_by summarise mutate select arrange inner_join
#' @importFrom tibble add_column tibble
#' @export
#' @examples
#' library(tidySpectR)
#'
#' average_spectrum(fa_nmr)
#'
#' average_spectrum(fa_nmr, group = 'labels')
average_spectrum <- function(obj, group = 'all', ...){
    new_obj <- obj
    if (group == 'all'){
        new_obj$data <- obj$dat %>% 
                        group_by(bins, bin_start, bin_end) %>%
                        summarise(values = mean(values), .groups= 'drop') %>%
                        add_column(id = "all", .before = 1) %>% 
                        mutate(id = as.factor(id))
                        
        new_obj$labels <- tibble(id = "all", label = NA) %>%
                          mutate(id = as.factor(id),
                                 label = as.factor(label))
                                 
    } else if (group == 'labels'){
        new_obj$data <- obj$dat %>% 
                        inner_join(obj$labels, by = 'id') %>%
                        group_by(bins, bin_start, bin_end, label) %>%
                        summarise(values = mean(values), .groups= 'drop') %>%
                        mutate(id = label, .before = 1) %>%
                        select(-label) %>%
                        arrange(id, bins)
        
        new_obj$labels <- tibble(id = unique(new_obj$data$id),
                                 label = NA) %>%
                          mutate(id = as.factor(id),
                                 label = as.factor(label))
    
    } else{
        rlang::abort("Invalid value for `group`, use either `all` or `labels`")
    }
    
    return(new_obj)
}

#' Pull the bin (or bucket) number of a spectra collection
#'
#' For a collection, retrieve the number of bins.
#' 
#' @aliases pull_numbin pull_numbin.collection
#' @export
pull_numbin <- function( ...)
    UseMethod("pull_numbin")

#' @rdname pull_numbin
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble.
#' @importFrom dplyr group_by summarise
#' @export
#' @examples
#' library(tidySpectR)
#' pull_numbin(fa_nmr)
pull_numbin.collection <- function(obj, ...){
    obj$data %>% 
        group_by(id) %>%
        summarise("numbin"=n(), .groups = "drop")
}

#' Pull the breaking points (or bin limits) of a spectra collection
#'
#' For a collection, retrieve the bin limits, including the edges 
#'   (min and max limits of the spectra).
#'
#' @aliases pull_breaks pull_breaks.collection
#' @export
pull_breaks <- function(obj, ...)
    UseMethod("pull_breaks")

#' @rdname pull_breaks
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A vector of dbl.
#' @importFrom dplyr pull
#' @export
#' @examples
#' library(tidySpectR)
#' pull_breaks(fa_nmr) %>% head()
pull_breaks.collection <- function(obj, ...){
    breaks_low <- obj$data %>%
                pull(bin_start) %>% 
                sort() %>%
                unique()
    highest <- obj$data %>%
                pull(bin_end) %>%
                unique() %>%
                sort() %>%
                last()
    breaks_low %>% 
        append(highest) 
}

#' Pull the ids of a spectra collection
#'
#' For a collection, retrieve the sample identifiers 
#' 
#' @aliases pull_ids pull_ids.collection
#' @export
pull_ids <- function(obj, ...)
    UseMethod("pull_ids")

#' @rdname pull_ids
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A vector of chr.
#' @export
#' @examples
#' library(tidySpectR)
#' pull_ids(fa_nmr)
pull_ids.collection <- function(obj, ...){
    obj$data$id %>% unique() %>% as.character()
}

#' Adds labels 
#' 
#' Add labels to an existing collection by providing a 
#'   dat frame or tibble associating sample ids to labels
#'
#' @aliases add_labels add_labels.collection
#' @export
add_labels <- function(...)
    UseMethod("add_labels")

#' @rdname add_labels
#' @param obj A spectra collection
#' @param labels A dataframe or tibble associating sample ids to labels
#' @param ids_from Name of the id column
#' @param labels_from Name of the label column
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#' @details Pre-existing labels will be overwritten.
#' @importFrom tibble tibble
#' @importFrom dplyr pull mutate
#' @export
#' @examples
#' library(tidySpectR)
#'
#' newlabs <- tibble(names = pull_ids(fa_nmr),
#'                   conditions = c("organic", "organic", "organic", "organic", "organic",
#'                                  "conventional", "conventional", "conventional", "conventional", "conventional"))
#'
#' add_labels(fa_nmr, newlabs, ids_from= "names", labels_from= "conditions")
add_labels <- function(obj, labels, ids_from, labels_from, ...){
    new_obj <- obj
    new_obj$labels <- tibble(id = pull(labels, var = ids_from),
                         label = pull(labels, var = labels_from)) %>%
                  mutate(id = as.factor(id),
                         label = as.factor(label))
    
    return(new_obj)
}
