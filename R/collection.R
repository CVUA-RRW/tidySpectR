#' Creates a Spectra collection
#'
#' A collection is a matrix of spectral data.
#'
#' @aliases collection collection.default
#' @export
collection <- function(...)
    UseMethod("collection")

#' rdname collection
#' 
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An object of class `collection` with sub-objects:
#'   \item{data}{A tibble that contains the spectral data}
#'   \item{normalized}{Information about the normalization of the dataset}
#'   \item{bucketted}{Information about the bucketting of the dataset}
#'   \item{labels}{Information about the labelling of the dataset}
#'
#' @details The collection is empty on creation, use `add_spectrum` to 
#'   populate it.
#' @export
collection.default <- function(...){
    x <- list(data = tibble(), 
              normalized= FALSE, 
              bucketted = FALSE, 
              labels = NULL)
    class(x) <- "collection"
    return(x)
}

#' @aliases add_spctrum add_spectrum.collection
#' @param obj A spectra collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @export
add_spectrum <- function(obj, ...)
    UseMethod("add_spectrum")

#' Add a spectrum to a collection
#'
#' @param obj A `collection` object.
#' @param values A vector of intensity values.
#' @param id A unique identifier for the sample.
#' @param limits Upper and lower limits of the measurement space.
#' @param label A label for the sample.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `obj`.
#' @details It is assumed that the binning is uniform and correspond 
#'   to point measurments.
#'   
#' `labels` should be used to provide meanigful labels to the samples, e.g.
#'    'treated' and 'control'.
#'
#' @examples
#' library(tidyverse)
#' library(tidySpectR)
#' 
#' # Load one sample
#' sample <- nmr_spectra %>% filter(ID == "20198124123")
#' 
#' coll <- collection() %>%
#'      add_spectrum(sample$values, 
#'                   c(min(sample$bins), max(sample$bins)),
#'                   id = "20198124123")
#'
#' # Load a bunch of samples from a tibble (longform):
#' coll <- collection()
#' nmr_spectra %>% 
#'     group_by(ID) %>%
#'     group_walk(~ {coll <<- coll %>% 
#'                    add_spectrum(.x$values, 
#'                                 c(min(.x$bins), max(.x$bins)),
#'                                 id = .y$ID)})
#'
#' @rdname add_spectrum
#' @importFrom tibble tibble
#' @export
add_spectrum.collection <- function(obj, 
                                    values, 
                                    limits,
                                    id = deparse(substitute(values)), 
                                    label = NA,
                                    ...){
    # Check that id is unique
    if (length(obj$data) != 0) {
        if (id %in% obj$data$id){
        rlang::abort("`id` is already used in this collection")
        }
    }
        
    # get bins from limits
    bins <- seq(min(limits),
                max(limits),
                length.out = length(values))
    
    # Build the data tibble
    binsize = (max(limits) - min(limits)) / length(values)
    
    newdat <- tibble("id" = id,
                     "bins" = bins,
                     bin_start = bins - (binsize/2),
                     bin_end = bins + (binsize/2),
                     values = values) %>% 
              mutate(id = as.factor(id))
    obj$data <- bind_rows(obj$data, newdat)
    
    # Build the labels tibble
    newlab <- tibble("id" = id,
                     "label" = label) %>%
              mutate(id = as.factor(id),
                     label = as.factor(label))
    obj$labels <- bind_rows(obj$labels, newlab)
    
    return(obj)
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
#' @importFrom dplyr filter pull
#' @export
pull_breaks.collection <- function(obj, ...){
    if (!obj$bucketted) {
        rlang::abort("This collection was not yet bucketted. Thus breaking points may not be consistent.")
    }
    
    breaks_low <- obj$data %>%
                filter(id == obj$data$id[1]) %>%
                pull(bin_start)
    highest <- obj$data %>%
                filter(id == obj$data$id[1]) %>%
                pull(bin_end) %>%
                last()
    breaks_low %>% 
        append(highest) %>%
        sort()
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

#' Tidy a spectra collection
#'
#' `tidy` will return a dataframe containing the spectral 
#'   information in its current state.
#' @name tidy.collection
#' @param obj A `collection` object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble with a variable number columns
#'   depending on the binning of the data:
#'   \item{id}{Unique sample identifier}
#'   \item{label}{Label for the sample}
#'   \item{bins}{Bin centers}
#' @importFrom dplyr inner_join relocate
#' @importFrom tidyr pivot_wider
#' @export
tidy.collection <- function(obj, ...){
    obj$data %>%
        select(id, bins, values) %>%
        mutate(bins = as.factor(bins)) %>%
        pivot_wider(names_from = bins, values_from = values) %>%
        inner_join(obj$labels, by = 'id') %>%
        relocate(label, .after = 1)
}

#' Print a spectra collection
#'
#' @aliases print.collection
#' @param obj A `collection` object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return The original object (invisibly)
#' importFrom dplyr pull select
#' @export
print.collection <- function(obj, ...){
    # Sample number
    entries <- pull_ids(obj) %>% 
                length()
    
    # Processing bin_number
    min_numbin <- pull_numbin(obj) %>%
        select(numbin) %>% 
        min()
    
    max_numbin <- pull_numbin(obj) %>%
        select(numbin) %>% 
        max()
    
    if (min_numbin == max_numbin){
        num_bin <- as.character(min_numbin)
    } else {
        num_bin <- paste0(as.character(min_numbin),
                    " - ",
                    as.character(max_numbin))
    }
        
    cat("Spectra collection containing", entries, "entries.\n")
    cat("Number of bins:", num_bin, "\n")
    cat("Normalized:", obj$normalized, "\n")
    cat("Bucketted:", obj$bucketted, "\n")
    cat("Labels:", levels(obj$labels$label), "\n")
    
    invisible(obj)
}