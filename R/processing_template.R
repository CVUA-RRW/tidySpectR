#' Stores a template for processing of `collection` objects
#'
#' Processing steps specification are stored in a template 
#' object. This will be typically handled internally.
#' 
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @aliases processing_template processing_template.default
#' @export
processing_template <- function(...)
    UseMethod("processing_template")

#' @rdname processing_template
#' @return A list-like object storing processing steps.
#' @importFrom tibble tibble
#' @export
processing_template.default <- function(...){
    x <- list()
    x$steps <- list()
    
    class(x) <- "processing_template"
    return(x)
}

#' Add a new `processing_step`
#'
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @aliases new_step new_step.processing_template
#' @export
new_step <- function(...)
    UseMethod("new_step")

#' @rdname new_step
#'
#' @param x A `processing_template` object
#' @param fun either a function or a non-empty character string naming the function to be called.
#' @param arglist a list of arguments to the function call. The names attribute of args gives the argument names.
#' @param name A string description of the step
#' @param id An id associated with the step. Will be converted to a unique id.
#' @return A list-like object storing processing steps.
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' template <- processing_template() %>%
#'             new_step(mask, list(from = 5, to = Inf), name = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), name = "bucket")
#' template
new_step.processing_template <- function(x, fun, arglist, name = "none", id = NULL , ...){
    
    if (is.null(id)){
        id <- name
    }
    
    if (!is.character(fun)){
        fun = as.character(substitute(fun))
    }
    
    stp <- processing_step(fun, arglist, name = name, id = id)
    
    x$steps <- append(x$steps, list(stp))
    
    return(x)
}

#' Process a `collection` object
#' @export
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
process <- function(...)
    UseMethod("process")
  
#' Applies a `processing_template` on a `collection` object
#' @rdname process
#' @param x A `processing_template` object
#' @param collection a `collection` to process
#' @param verbose Set to TRUE to show progress
#' @examples
#' library(tidySpectR)
#' 
#' template <- processing_template() %>%
#'             new_step(mask, list(from = 5, to = Inf), name = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), name = "bucket") 
#' process(template, fa_nmr)
#' @export
process.processing_template <- function(x, collection, verbose = FALSE, ...) {
    
    i <- 1
    
    for (stp in x$steps){
        
        if (verbose){
            cat("Step", i , "/", length(x$steps),":", stp$name, "\n")
            i <- i+1
        }
        
        collection <- process(stp, collection)
    }
    return(collection)
}

#' @rdname processing_template
#' @param x A `processing_step`object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble representation of x
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a masking step
#' template <- processing_template() %>%
#'             new_step(mask, list(from = 5, to = Inf), name = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), name = "bucket") 
#' 
#' tidy(template)
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble rowid_to_column
tidy.processing_template <- function(x, ...){
    lapply(x$steps, tidy) %>% 
    lapply(select, name, method, id) %>%
    bind_rows() %>%
    rowid_to_column(var= "num")
}

#' Print processing_template
#' @aliases print.processing_template
#' @param x A `processing_step`object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return The original object (invisibly)
#' @export
print.processing_template <- function(x, ...){
    i <- 1
    for (stp in x$steps){
        cat("Step", i , "/", length(x$steps),":", stp$name, "\n")
        i <- i+1
    }
}