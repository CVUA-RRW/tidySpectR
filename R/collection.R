#' Creates a spectra collection
#'
#' A collection is a matrix of spectral data and associated labels.
#'
#' @aliases collection collection.default
#' @export
collection <- function(...)
    UseMethod("collection")

#' @rdname collection
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
#'   populate it. It is recommended to populate a collection using `purrr::walk`
#'   on a list of files (see `add_spectrum`).
#' @export
collection.default <- function(...){
    x <- list(data = tibble(), 
              normalized= FALSE, 
              bucketted = FALSE, 
              labels = NULL)
    class(x) <- "collection"
    return(x)
}

#' Add a single spectrum to a collection object
#' 
#' Adds an new entry to an existing colleciton object.
#'
#' @aliases add_spctrum add_spectrum.collection
#' @export
add_spectrum <- function(x, ...)
    UseMethod("add_spectrum")

#' @rdname add_spectrum
#' @param x A `collection` object.
#' @param values A vector of intensity values.
#' @param id A unique identifier for the sample.
#' @param limits A vector of length two with the upper and lower 
#'   limits of the measurement space.
#' @param label A label for the sample.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `x`.
#' @details It is assumed that the binning is uniform and correspond 
#'   to point measurments.
#'   
#' `labels` should be used to provide meanigful labels to the samples, e.g.
#'    'treated' and 'control'.
#'
#' It is recommended to populate a collection using `purrr::walk`
#'   on a list of files (see example).
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_rows
#' @export
#' @examples 
#'
#' ####################################################################
#' 
#' # A simple example: adding a spectra from a vector of values:
#' library(tidySpectR)
#' 
#' # Genearating some values
#' values = runif(50)
#' limits = c(0,49)
#' 
#' # Collection is empty on creation
#' coll = collection()
#' 
#' # Adding the data
#' coll %>% 
#'     add_spectrum(values, limits, id = "basic_example", label = "test")
#' 
#' ####################################################################
#' \dontrun{
#' # Real life example: parsing data form file and adding them to a 
#' # collection on the fly
#' library(purrr)
#'
#' # Generate a list of file paths for a folder containing spectra data files
#' folder_path <- "path/to/data/folder"
#' files <- file.path(folder_path, list.files(folder_path))
#' 
#' # Create a collection and add the spectra
#' coll <- collection()
#' purrr::walk(files,
#'               function(x){
#'                   # Parse your files to extract values, limits and ids
#'                   # ...
#'                   coll <<- coll %>% 
#'                            add_spectrum(values, limits, id)
#'               })
#' }
add_spectrum.collection <- function(x, 
                                    values, 
                                    limits,
                                    id = deparse(substitute(values)), 
                                    label = NA,
                                    ...){
    # Check that id is unique
    if (length(x$data) != 0) {
        if (id %in% x$data$id){
        rlang::abort("`id` is already used in this collection")
        }
    }
        
    # get bins from limits
    bins <- seq(min(limits),
                max(limits),
                length.out = length(values))
    
    new_obj <- x
    
    # Build the data tibble
    binsize = (max(limits) - min(limits)) / (length(values) - 1)
    
    newdat <- tibble(id = id,
                     bins = bins,
                     bin_start = bins - (binsize/2),
                     bin_end = bins + (binsize/2),
                     values = values) %>% 
              mutate(id = as.factor(id))
    new_obj$data <- bind_rows(x$data, newdat)
    
    # Build the labels tibble
    newlab <- tibble(id = id,
                     label = label) %>%
              mutate(id = as.factor(id),
                     label = as.factor(label))
    new_obj$labels <- bind_rows(x$labels, newlab)
    
    return(new_obj)
}

#' Print a spectra collection
#'
#' @aliases print.collection
#' @param x A `collection` object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return The original object (invisibly)
#' @export
#' @examples
#' library(tidySpectR)
#' print(fa_nmr)
print.collection <- function(x, ...){
    # Sample number
    entries <- pull_ids(x) %>% 
                length()
    
    # Processing bin_number
    min_numbin <- pull_numbin(x) %>%
        select(numbin) %>% 
        min()
    
    max_numbin <- pull_numbin(x) %>%
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
    cat("Normalized:", x$normalized, "\n")
    cat("Bucketted:", x$bucketted, "\n")
    cat("Labels:", levels(x$labels$label), "\n")
    
    invisible(x)
}