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
#' @importFrom tibble tibble
#' @export
collection.default <- function(...){
    
    x <- list(data = tibble(), 
              labels = NULL,
              processor = processing_template())
    class(x) <- "collection"
    return(x)
}

#' Exports a processor object
#'
#' @aliases export_processor export_processor.collection
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @export
export_processor <- function(...)
    UseMethod("export_processor")

#' @rdname export_processor
#'
#' @param x A `collection` object
#' @return A `processing_template` object
#' @examples
#' library(tidySpectR)
#' 
#' fa_nmr %>% 
#'   mask(from = 5, to = Inf) %>%
#'   bucket_uniform(N = 10) %>%
#'   export_processor() %>%
#'   tidy()
#' @export
export_processor.collection <- function(x, ...){
    return(x$processor)
}
    
#' Add a single spectrum to a collection object
#' 
#' Adds an new entry to an existing colleciton object.
#'
#' @aliases add_spectrum add_spectrum.collection
#' @export
add_spectrum <- function(x, ...)
    UseMethod("add_spectrum")

#' @rdname add_spectrum
#' @param x A `collection` object.
#' @param values A vector of intensity values.
#' @param id A unique identifier for the sample.
#' @param left A dbl, left limit of the spectra, corresponds to the
#'   first element of `values`
#' @param right A dbl, right limit of the spectra, corresponds to the
#'   last element of `values`
#' @param label A label for the sample.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An updated version of `x`.
#' @details It is assumed that the binning is uniform and correspond 
#'   to point measurments.
#'
#' Be careful with the values of `left`and `right`! THis is important
#'   for example for NMR data which are usually given with a reversed 
#'   y-axis. 
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
#' left = 0
#' right = 49
#' 
#' # Collection is empty on creation
#' coll = collection()
#' 
#' # Adding the data
#' coll %>% 
#'     add_spectrum(values, left, right, id = "basic_example", label = "test")
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
#'                            add_spectrum(values, left, right, id)
#'               })
#' }
add_spectrum.collection <- function(x, 
                                    values, 
                                    left,
                                    right,
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
    bins <- seq(left,
                right,
                length.out = length(values))
    
    new_obj <- x
    
    # Build the data tibble
    limits <- c(left, right)
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
    cat("Limits:", pull_limits(x), "\n")
    cat("Labels:", levels(x$labels$label), "\n")
    cat("\nProcessing:\n")
    print(x$processor)
    
    invisible(x)
}

#' @rdname collection
#'
#' @param x A `collection` object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble with a variable number columns
#'   depending on the binning of the data:
#'   \item{id}{Unique sample identifier}
#'   \item{label}{Label for the sample}
#'   \item{bins}{Bin centers}
#' @importFrom dplyr inner_join relocate select mutate ungroup
#' @importFrom tidyr pivot_wider
#' @examples
#' library(tidySpectR)
#'
#' tidy(fa_nmr)
#' @export
tidy.collection <- function(x, ...){
    x$data %>%
        select(id, bins, values) %>%
        mutate(bins = as.factor(bins)) %>%
        pivot_wider(names_from = bins, values_from = values) %>%
        inner_join(x$labels, by = 'id') %>%
        relocate(label, .after = 1) %>%
        ungroup()
}