#' Calculate the average spectrum 
#'
#' Averages spectra 
#' 
#' @aliases average_spectrum average_spectrum.collection
#' @export
average_spectrum <- function(...)
    UseMethod("average_spectrum")

#' @param obj A`collection` object
#' @param group How to perform grouping. Either `all`or `labels`
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `collection`.
#'
#' @rdname average_spectrum
#' @importFrom dplyr group_by summarise mutate select arrange inner_join
#' @importFrom tibble add_column tibble
#' @export
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

#' @aliases pull_numbin pull_numbin.collection
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @export
pull_numbin <- function(obj, ...)
    UseMethod("pull_numbin")

#' Pull the bin (or bucket) number of a spectra collection
#'
#' For a collection, retrieve the number of bins.
#' 
#' @return A tibble.
#' @rdname pull_numbin
#' @importFrom dplyr group_by summarise
#' @export
pull_numbin.collection <- function(obj, ...){
    obj$data %>% 
        group_by(id) %>%
        summarise("numbin"=n(), .groups = "drop")
}

#' @aliases pull_breaks pull_breaks.collection
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @export
pull_breaks <- function(obj, ...)
    UseMethod("pull_breaks")

#' Pull the breaking points (or bin limits) of a spectra collection
#'
#' For a collection, retrieve the bin limits, including the edges 
#'   (min and max limits of the spectra).
#' 
#' @return A vector of dbl.
#' @rdname pull_breaks
#' @importFrom dplyr pull
#' @export
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

#' @aliases pull_ids pull_ids.collection
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @export
pull_ids <- function(obj, ...)
    UseMethod("pull_ids")

#' Pull the ids of a spectra collection
#'
#' For a collection, retrieve the sample identifiers 
#' 
#' @return A vector of chr.
#' @rdname pull_ids
#' @export
pull_ids.collection <- function(obj, ...){
    obj$data$id %>% unique() %>% as.character()
}
