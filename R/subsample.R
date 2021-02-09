#' Sub-sample a spectra collection
#' 
#' Pick a subsample form a spectra collection
#'
#' @param x A`collection` object
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @aliases subsample subsample.collection
#' @export
subsample <- function(x, ...)
    UseMethod("subsample")

#' @rdname subsample
#' @param size Size of the sub-sampling
#' @param balance_labels A boolean, wether to pick spectra equally between labels.
#'   If the subsampling size is below the number of labels, not all labels will be chosen from.
#' @param ids A vector of ids to extract, this will override the other parameters.
#' @return a subsample of x
#' @export
#' @importFrom dplyr filter pull
#' @examples
#' library(tidySpectR)
#' library(dplyr)
#'
#' # Randomly sample
#' subsample(fa_nmr, size = 3)
#'
#' # Sample one spectra for each labels
#' subsample(fa_nmr, size= 2, balance_labels = TRUE)
#'
#' # Subsample specific IDs
#' subsample(fa_nmr, ids = c("20199305928", "20199306281", "20199504645"))
#'
#' # Sample alll spectra of specific labels
#' organic <- fa_nmr$labels %>% 
#'            filter(label == 'organic') %>%
#'            pull(id)
#' subsample(fa_nmr, ids = organic)
#'
#' # Chain subsamplings to randomly select a spectra with a specific label
#' subsample(fa_nmr, ids = organic) %>%
#'   subsample(size=1)
subsample.collection <- function(x, size = 2, balance_labels = FALSE, ids = NULL, ...) {
    new_obj <- x
    
    if (is.null(ids)) {
    
        if (balance_labels){
        
            nlab <- length(levels(x$labels$label))
            labsample <- round(size/nlab)
            
            for (lab in levels(x$labels$label)){
                if (size == 0) {
                    break
                } else if (labsample == 0){
                    picksize <- size
                } else {
                    picksize <- labsample
                }
                
                newids <- x$labels %>% 
                       filter(label == lab) %>%
                       pull(id) %>%
                       as.character() %>%
                       sample(size = picksize) 
                       
                ids <- append(ids, newids)
                
                size <- size - picksize
            }
            
        } else {
            ids <- x$labels %>% 
                   pull(id) %>%
                   sample(size = size)
        }
        
    }
    
    new_obj$data <- x$data %>% filter(id %in% ids)
    new_obj$labels <- x$labels %>% filter(id %in% ids)
    
    return(new_obj)
}