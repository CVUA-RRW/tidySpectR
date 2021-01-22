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
#' @param ... currently unused
#' @return An object of class `collection` with sub-objects:
#'   \item{data}{A tibble that contains the spectral data}
#'   \item{normalized}{Information about the normalization of the dataset}
#'   \item{bucketted}{Information about the bucketting of the dataset}
#'   \item{labels}{Information about the labelling of the dataset}
#'
#' @details The collection is empty on creation, use specific methods to add
#'   entries.
#' @export
collection.default <- function(...){
    x <- list(data = tibble(), 
              normalized= FALSE, 
              bucketted = FALSE, 
              labels = NULL)
    class(x) <- "collection"
    return(x)
}

#' @aliases pull_limits pull_limits.collection
#' @param obj A spectra collection
#' @export
pull_limits <- function(obj)
    UseMethod("pull_limits")

#' Pull the limits a spectra collection
#'
#' For a collection, retrieve the limits of the measurment space (x-axis)
#' 
#' @return A vector of dbl in the form c(min, max).
#' @rdname pull_limits
#' @export
pull_limits.collection <- function(obj){
    NULL
}

#' @aliases pull_numbin pull_numbin.collection
#' @param obj A spectra collection
#' @export
pull_numbin <- function(obj)
    UseMethod("pull_numbin")

#' Pull the bin (or bucket) number of a spectra collection
#'
#' For a collection, retrieve the number of bins.
#' 
#' @return A dbl.
#' @rdname pull_numbin
#' @export
pull_numbin.collection <- function(obj){
    NULL
}

#' @aliases pull_breaks pull_breaks.collection
#' @param obj A spectra collection
#' @export
pull_breaks <- function(obj)
    UseMethod("pull_breaks")

#' Pull the breaking points (or bin limits) of a spectra collection
#'
#' For a collection, retrieve the bin limits, including the edges (min and max limits of the spectra).
#' 
#' @return A vector of dbl.
#' @rdname pull_breaks
#' @export
pull_numbin.pull_breaks <- function(obj){
    NULL
}

# tidy.collection

#' Print a spectra collection
#'
#' @aliases print.collection`#'
#' @param obj A `collection` object.
#' @return The original object (invisibly)
#' @export
print.collection <- function(obj){
    cat("Spectra collection containing", length(obj$data), "entries.\n")
    cat("Limits:", pull_limits(obj), "\n")
    cat("Number of bins:", pull_numbin(obj), "\n")
    cat("Normalized:", obj$normalized, "\n")
    cat("Bucketted:", obj$bucketted, "\n")
    cat("Labelled:", !(nrow(obj$labels)==0), "\n")
    
    invisible(obj)
}