#' @importFrom utils globalVariables

utils::globalVariables(
    c(
        # collection variables
        ".", "id", "bins", "bin_start", "bin_end", 
        "label", "values", "numbin",
        
        # autoplot variables
        "x_offset", "y_offset", "average", "min_val",
        "max_val"
    )
)