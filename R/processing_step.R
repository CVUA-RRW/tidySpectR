#' Stores specifications of a `collection` processing step
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
#' @param fun_call A function call created with as.list(match.call())
#' @param type A string description of the step
#' @param id A unique id associated with the step
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return An object of calss `processing_step`
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' do_something <- function(x, y, z){
#'      print("doing something")
#'      as.list(match.call())
#' }
#'
#' fun_call <- do_something(1,2,3)
#' # Remove unwanted arguments
#' fun_call$x <- NULL
#' fun_call
#' 
#' processing_step(fun_call, "test", "test_01")
#' @importFrom rlang is_callable
processing_step.default <- function(fun_call, type= "none", id = rand_id("none"), ...){
    
    if (!is_callable(fun_call[[1]])){
        rlang::abort("`fun_call` must be a function call.")
    }
    
    process <- 
        list(id = id,
             type = type,
             fun = fun_call[[1]],
             fun_call = fun_call,
             arglist = fun_call[-1] )
    
    class(process) <- "processing_step"
    return(process)
}

#' Tidy a `processing_step`
#' 
#' @name tidy.processing_step
#' @param x A `processing_step`object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return A tibble representation of x
#' @examples
#' library(tidySpectR)
#' 
#' do_something <- function(x, y, z){
#'      print("doing something")
#'      as.list(match.call())
#' }
#'
#' fun_call <- do_something(1,2,3)
#' # Remove unwanted arguments
#' fun_call$x <- NULL
#' fun_call
#' 
#' stp <- processing_step(fun_call, "test", "test_01")
#' tidy(stp)
#' @rdname tidy.processing_step
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
tidy.processing_step <- function(x, ...){
    tibble(type = x$type,
           method = deparse(x$fun),
           args = list(x$arglist),
           id = x$id) %>% 
    unnest_wider(args)
} 

print.processing_step <- function(x, ...){
    cat(
        deparse(
            as.call(x$fun_call))
    )
    cat("\n")
    invisible(x)
}

#' Applies a processing step on a `collection` object
#'
#' @aliases process process.processing_step
#' @export
process <- function(x, ...)
    UseMethod("process")

#' @rdname process
#' @param x A `processing_step` object to apply
#' @param collection A `collection` object to process
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @return The retrun value of the processing step call
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' # Creating a function call to `mask`
#' # Note that the `collection` argument is omitted
#' fun_call <- as.list(match.call(mask, call("mask", from = 5, to = Inf)))
#' 
#' stp <- processing_step(fun_call, "masking", "masking_01")
#' processed <- process(stp, fa_nmr)
#' processed
process.processing_step <- function(x, collection, ...){
    arglist <- append(list(collection), x$arglist)
    do.call(eval(x$fun), arglist)
}

#' Generate random id
#' @param id A base string to expand with random digits
#' @ param ... currently unused
#' @importFrom stringr str_pad
#' @importFrom stats runif
#' @keywords internal
rand_id <- function(id, ...){
    paste0(
        id, 
        "_", 
        str_pad(
            floor(
                runif(1, min = 0, max = 999999)), 
        6, pad = "0"))
}