#' @useDynLib tidySpectR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' @importFrom utils globalVariables
utils::globalVariables(
    c(
        # collection variables
        ".", "id", "bins", "bin_start", "bin_end", 
        "label", "values", "numbin",
        
        # autoplot variables
        "x_offset", "y_offset", "average", "min_val",
        "max_val",
        
        # aibin
        "product", "vbsum", "candidate",
        
        # bucket from breaks
        "bin_index"
        
    )
)