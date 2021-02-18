# Misc functions for the handling of collection objects

#' Reduce the resolution of Spectra
#'
#' @aliases reduce_resolution reduce_resolution.collection
#' @param x A`collection` object to normalize
#' @param ... further arguments passed to or from other methods(not
#'   currently used).
reduce_resolution <- function(x, ...)
    UseMethod("reduce_resolution")

#' @rdname reduce_resolution
#' @param k an integer expressing the reduction in resolution.
#'   For example `k=2`will keep every second point, `k=3` every third.
#' @note Reducing resolution of the spectra before computation intensive steps 
#'   like AI binning can significantly improve their speed, while minimally affecting the output.
#' You should however be careful with the choice of k and in the caase of subsequent binning 
#'   make sure to remove the outter limits of the returned results as these will not match
#'   the outter limits of the full resolution spectra.
#' @examples
#' library(tidySpectR)
#' # Speeding up AI binning
#' reduced <- reduce_resolution(fa_nmr, k=2)
#' binned <- bucket_aibin(reduced)
#' breaks <- pull_breaks(binned)
#' 
#' # last break does not match spectra limit, leaving it so would create a bin at the ends of the spectra
#' breaks[1]
#' breaks[length(breaks)]
#' pull_limits(fa_nmr)
#'
#' breaks <- breaks[-1] # remove first break
#' breaks <- breaks[-length(breaks)] # remove last break
#' processed <- bucket_from_breaks(breaks)
#'
#' # Full resolution bucketted spectra
#' processed
#' @importFrom dplyr slice row_number
reduce_resolution.collection <- function(x, k, ...){
    new_obj <- x
    new_obj$data <- data2wide(x$data) %>%
           slice(which(row_number() %% k == 1)) %>%
           wide2long()
    return(new_obj)
}

#' Calculate the average spectrum 
#'
#' Averages spectra 
#' 
#' @aliases average_spectrum average_spectrum.collection
#' @export
average_spectrum <- function(x, ...)
    UseMethod("average_spectrum")

#' @rdname average_spectrum
#' @param x A`collection` object
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
average_spectrum.collection <- function(x, group = 'all', ...){
    new_obj <- x
    if (group == 'all'){
        new_obj$data <- x$dat %>% 
                        group_by(bins, bin_start, bin_end) %>%
                        summarise(values = mean(values), .groups= 'drop') %>%
                        add_column(id = "all", .before = 1) %>% 
                        mutate(id = as.factor(id))
                        
        new_obj$labels <- tibble(id = "all", label = NA) %>%
                          mutate(id = as.factor(id),
                                 label = as.factor(label))
                                 
    } else if (group == 'labels'){
        new_obj$data <- x$dat %>% 
                        inner_join(x$labels, by = 'id') %>%
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
pull_numbin <- function(x, ...)
    UseMethod("pull_numbin")

#' @rdname pull_numbin
#' @param x A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble.
#' @importFrom dplyr group_by summarise n
#' @export
#' @examples
#' library(tidySpectR)
#' pull_numbin(fa_nmr)
pull_numbin.collection <- function(x, ...){
    x$data %>% 
        group_by(id) %>%
        summarise("numbin"= n(), .groups = "drop")
}

#' Pull the breaking points (or bin limits) of a spectra collection
#'
#' For a collection, retrieve the bin limits, including the edges 
#'   (min and max limits of the spectra).
#'
#' @aliases pull_breaks pull_breaks.collection
#' @export
pull_breaks <- function(x, ...)
    UseMethod("pull_breaks")

#' @rdname pull_breaks
#' @param x A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A vector of dbl.
#' @importFrom dplyr pull
#' @export
#' @examples
#' library(tidySpectR)
#' pull_breaks(fa_nmr) %>% head()
pull_breaks.collection <- function(x, ...){
    breaks_low <- x$data %>%
                pull(bin_start) %>% 
                sort() %>%
                unique()
    highest <- x$data %>%
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
pull_ids <- function(x, ...)
    UseMethod("pull_ids")

#' @rdname pull_ids
#' @param x A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A vector of chr.
#' @export
#' @examples
#' library(tidySpectR)
#' pull_ids(fa_nmr)
pull_ids.collection <- function(x, ...){
    x$data$id %>% unique() %>% as.character()
}

#' Pull the limits of a spectra collection
#'
#' For a collection, the upper and lower limits of the x-axis
#' 
#' @aliases pull_limits pull_limits.collection
#' @export
pull_limits <- function(x, ...)
    UseMethod("pull_limits")

#' @rdname pull_limits
#' @param x A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A vector of int.
#' @export
#' @examples
#' library(tidySpectR)
#' pull_limits(fa_nmr)
pull_limits.collection <- function(x, ...){
    breaks <- pull_breaks(x) 
    return(c(first(breaks), last(breaks)))
}
    
#' Adds labels 
#' 
#' Add labels to an existing collection by providing a 
#'   dat frame or tibble associating sample ids to labels
#'
#' @aliases add_labels add_labels.collection
#' @export
add_labels <- function(x, ...)
    UseMethod("add_labels")

#' @rdname add_labels
#' @param x A `collection` object 
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
#' library(tibble)
#'
#' newlabs <- tibble(names = pull_ids(fa_nmr),
#'                   conditions = c("control", "control", "control", 
#'                                  "treated", "treated", "treated"))
#'
#' add_labels(fa_nmr, newlabs, ids_from= "names", labels_from= "conditions")
add_labels.collection <- function(x, labels, ids_from, labels_from, ...){
    new_obj <- x
    new_obj$labels <- tibble(id = pull(labels, var = ids_from),
                         label = pull(labels, var = labels_from)) %>%
                  mutate(id = as.factor(id),
                         label = as.factor(label))
    
    return(new_obj)
}
