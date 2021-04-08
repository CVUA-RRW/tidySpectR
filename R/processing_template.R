#' Stores a template for processing of `collection` objects
#'
#'#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @aliases processing_template processing_template.default
#' @export
processing_template <- function(...)
    UseMethod("processing_template")

#' @rdname processing_template
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
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
#' @param type A string description of the step
#' @param name A name associated with the step. Will be converted to a unique id.
#' @return A list-like object storing processing steps.
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' template <- processing_template() %>%
#'             new_step(mask, list(from = 5, to = Inf), type = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), type = "bucket")
#' template
new_step.processing_template <- function(x, fun, arglist, type = "none", name = NULL , ...){
    
    if (is.null(name)){
        name <- type
    }
    
    if (!is.character(fun)){
        fun = as.character(substitute(fun))
    }
    
    stp <- processing_step(fun, arglist, type = type, name = name)
    
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
#'             new_step(mask, list(from = 5, to = Inf), type = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), type = "bucket") 
#' process(template, fa_nmr)
#' @export
process.processing_template <- function(x, collection, verbose = FALSE, ...) {
    
    i <- 1
    
    for (stp in x$steps){
        
        if (verbose){
            cat("Step ", i , "/", length(x$steps)," : ")
            print(stp)
        i <- i+1
        }
        
        collection <- process(stp, collection)
    }
    return(collection)
}

#' Tidy a `processing_template`
#' 
#' @name tidy.processing_template
#' @param x A `processing_step`object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble representation of x
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a masking step
#' template <- processing_template() %>%
#'             new_step(mask, list(from = 5, to = Inf), type = "mask") %>%
#'             new_step(bucket_uniform, list(N = 10), type = "bucket") 
#' 
#' tidy(template)
#' @rdname tidy.processing_template
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble rowid_to_column
tidy.processing_template <- function(x, ...){
    lapply(x$steps, tidy) %>% 
    lapply(select, type, method, id) %>%
    bind_rows() %>%
    rowid_to_column(var= "num")
}

# print.processing_template
