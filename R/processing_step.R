#' Stores specifications of a `collection` processing step
#'
#' Processing steps specification are stored in a step 
#' object. This will be typically handled internally.
#' 
#' A `processing_step` stores the function call and associated
#'   arguments to reproduce a processing step at a later time
#'   point a a different collection object.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @aliases processing_step processing_step.default
#' @export
processing_step <- function(...)
    UseMethod("processing_step")

#' @rdname processing_step
#' 
#' @param fun either a function or a non-empty character string naming the function to be called.
#' @param arglist a list of arguments to the function call. The names attribute of args gives the argument names.
#' @param type A string description of the step
#' @param name A name associated with the step. Will be converted to a unique id.
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An object of class `processing_step`
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a masking step
#' stp <- 
#'    processing_step(
#'      fun = mask, 
#'      arglist = list(from = 5, to = Inf),
#'      type = "masking", 
#'      name = "masking")
#' 
#' stp
#' @importFrom rlang is_callable
processing_step.default <- function(fun, arglist, type= "none", name = "none", ...){
    
    if (!is.character(fun)){
        fun = as.character(substitute(fun))
    }
    
    process <- 
        list(id = rand_id(name),
             type = type,
             fun = fun,
             arglist = arglist)
    
    class(process) <- "processing_step"
    return(process)
}

#' Applies a `processing_step` on a `collection` object
#' @rdname process
#' @param x A `processing_step` object
#' @param collection a `collection` to process
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a masking step
#' stp <- 
#'    processing_step(
#'      fun = mask, 
#'      arglist = list(from = 5, to = Inf),
#'      type = "masking", 
#'      name = "masking")
#' 
#' process(stp, fa_nmr)
#' @export
process.processing_step <- function(x, collection, ...){
    arglist <- append(list(collection), x$arglist)
    do.call(x$fun, arglist)
}

#' @rdname processing_step
#' @param x A `processing_step`object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble representation of x
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a masking step
#' stp <- 
#'    processing_step(
#'      fun = mask, 
#'      arglist = list(from = 5, to = Inf),
#'      type = "masking", 
#'      name = "masking")
#' 
#' tidy(stp)
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
tidy.processing_step <- function(x, ...){
    tibble(type = x$type,
           method = x$fun,
           args = list(x$arglist),
           id = x$id) %>% 
    unnest_wider(args)
} 

#' @export
#' @importFrom rlang call2
print.processing_step <- function(x, ...){
    funcall <-
        do.call(
            rlang::call2, 
            append(x$fun, x$arglist)
        )
    print(funcall)
    invisible(x)
}