#' Applies Adaptive Inteligent Binning on a collection
#' 
#' Adaptive Inteligent Binning (aibin) recursively finds bin edges 
#'   in existing bins by maximizing the information contained in each 
#'   newly created bin.
#' @aliases bucket_aibin bucket_aibin.collection
#' @export
#' @details 
#' AI binning will first consider the entire spectra as a single bin.
#'   It will then consider every position in the spectra as a potential
#'   splitting candidate and calulate a quality value for each splitting candidate.
#'   If the best splitting candidates fulfills quality requirements, the bin 
#'   will be split and the process will be repeated on the newly created bins.
#'
#' In order to limit splitting in local minima of noisy regions a minimal quality
#'   value is computed from the user-provided noise-region.
#' 
#' @references
#' De Meyer T, Sinnaeve D, Van Gasse B, Tsiporkova E, Rietzschel ER, 
#' De Buyzere ML, Gillebert TC, Bekaert S, Martins JC, Van Criekinge W. 
#' NMR-based characterization of metabolic alterations in hypertension 
#' using an adaptive, intelligent binning algorithm. Anal Chem. 2008 
#' May 15;80(10):3783-90. doi: 10.1021/ac7025964. 
#' \url{https://pubmed.ncbi.nlm.nih.gov/18419139/}
bucket_aibin <- function(x, ...)
    UseMethod("bucket_aibin")

#' @rdname bucket_aibin
#' @param x A`collection` object to bucket
#' @param R resolution value, strictly positive and typically in the interval
#'   0 > R >= 1. 
#' @param noise_region A`collection` object containing a noise_region
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @returns An updated version of x
#' @note The amount of created bins is not directly dpeending of a high or low R.
#'   If a maximal splitting is looked for, you should try several different values 
#'   in the possible rangeof R (see references). However the differences in binning should
#'   limited to bins that are borderline noise and most likely has limited effect in 
#'   the downstream analysis.
#' @export
#' @examples
#' \dontrun{
#' library(tidySpectR)
#' 
#' # First we normalize the dataset and extract noise and sepctra regions
#' normalized <- fa_nmr %>% 
#'                   normalize_internalStandard(3.58, 3.64)
#' spectra <- normalized %>% extract(0, 7.2)
#' noise <- normalized %>% mask(0, 7.2, overlaps = 'remove')
#' 
#' 
#' bucketted <- bucket_aibin(spectra, 0.2, noise)
#' 
#' # Multiprocessing example
#' library(future)
#' cl <- makeClusterPSOCK(6)
#' plan(cluster, workers = cl)
#'
#' bucketted <- bucket_aibin(spectra, 0.2, noise)
#'
#' stopCluster(cl)
#'}
#' @importFrom dplyr arrange
bucket_aibin.collection <- function(x, R, noise_region,...){
    # Speed up : convert tibble to data.tables internally?
    # Determine Vnoise
    noise_region <- noise_region$data %>%
                    data2wide() %>%
                    arrange(bins) %>%
                    as.matrix()
                    
    vnoise <- find_vnoise(noise_region, 
                          R)
                          
    # Run the algorithm on the spectra region using the Vnoise value determined above
    spectra <- x$data %>%
                    data2wide() %>%
                    arrange(bins) %>%
                    as.matrix()
                    
    splits <- recursive_split(spectra, 
                              R, 
                              vnoise)
    
    # Applying buckets
    new_obj <- bucket_from_breaks(x, splits)
    new_obj$bucketted <- "Adaptive inteligent binning"

    return(new_obj)
}

#' Find the max bin value in the noise region
#' 
#' @param noise_region A matrix containing the noise region
#' @param R resolution
#' @returns A dbl, maximum bin value in the noise region
#' @keywords internal
#' @importFrom purrr map2_dbl
find_vnoise <- function(noise_region, R){
    noise_bins <- recursive_split(noise_region, R, vnoise = 0) %>% 
                  sort()
    
    left_bins <- noise_bins[-length(noise_bins)]
    right_bins <- noise_bins[-1]
    
    # Calculate bin values for all buckets
    vnoise <- 
        map2_dbl(
            left_bins, 
            right_bins,
            function(.x, .y, R){
                bucket <- noise_region[noise_region[,'bin_start'] >= .x & noise_region[,'bin_end'] <= .y,]
                bin_value(bucket, R)
            },
            R) %>%
        max()
              
    return(vnoise)
}

#' Recursively split the spectra with the adaptive inteligent binning algorithm
#' 
#' @param current_bin A matrix containing the current bin
#' @param R resolution
#' @param vnoise Maximal bin value for the noise region.
#' @param breaks A vector of double that will be populated with the 
#'   binning positions
#' @returns A vector of dbl contianing the split positions
#' @keywords internal
recursive_split <- function(current_bin, R, vnoise, breaks = NULL){
    while(TRUE){
        # Try to split the bin
        new_split <- divide_bin(current_bin, R, vnoise)
        
        if (!is.numeric(new_split)) {
            # If the bin cannot be split, append the right limit of the bin to 
            # the binning point and return the whole vector
            # this will be a splitting binning point
            breaks <- current_bin[,'bin_end'] %>%
                        max() %>%
                        append(breaks)
            return(breaks)
            
        } else {
            # Extract the left-most bin of the split
            sub_bin <- current_bin[current_bin[, 'bin_end'] <= new_split, ]
            
             # Keep splittin' 
            # recursive call, will return when unsplitable bin is found
            breaks <- recursive_split(sub_bin, R, vnoise, breaks)
            
            # Moving on to the next bin
            current_bin <- current_bin[current_bin[, 'bin_start'] >= max(breaks), ]
        }
    }
}

#' Find a new division splitting position for the current bin
#' 
#' @param current_bin A matrix containing the current bin
#' @param R resolution
#' @param vnoise Maximal bin value for the noise region.
#' @return A dbl, the new optimal splitting position or FALSE if not splitting is possible
#' @keywords internal
#' @importFrom dplyr arrange 
#' @importFrom furrr future_imap furrr_options
divide_bin <- function(current_bin, R, vnoise){
    vb <- bin_value(current_bin, R)
    
    # Get candidate bin limits
    # Consider all limits in the interval minus the first and last
    candidates <- current_bin[, 'bin_start'][-1]
    
    # Get bin values for each candidates
    split_values <- future_imap(candidates, 
                                ~ sub_bins_values(.x, .y,
                                current_bin, 
                                R),
                                .options = furrr_options(packages= "matrixStats"))
                                
    split_values <- do.call(rbind, split_values)
                           
    # Find candidate which maximizes Vbsum
    new_split <- split_values[order(split_values[,'vbsum'], 
                                    split_values[,'candidate'], 
                                    decreasing = TRUE), ][1, , drop = FALSE]

    # Is it worth spliting?
    if (bin_eval_test(vbmax = new_split[, 'vbsum'], 
                  vb = vb, 
                  vb1max = new_split[, 'vb1'], 
                  vb2max = new_split[, 'vb2'],
                  vnoise = vnoise)
                  ){
        return(new_split[,'candidate'])
    } else {
        return(FALSE)
    }
}

#' Calculate the bin values for each splits of the current bin
#'
#' @param x spliting point
#' @param i splitting point index
#' @param current_bin A matrix containing the current bin
#' @param R resolution
#' @return A Matrix with bin values:
#' \describe{
#'   \item{candidate}{position of the new splitting point}
#'   \item{vb1}{left-bin value}
#'   \item{vb2}{right-bin value}
#'   \item{vbsum}{sum of the left and righ bin values}
#' }
#' @keywords internal
sub_bins_values <- function(x, i, current_bin, R){
    left_bin <- current_bin[1:i, , drop = FALSE]
    right_bin <- current_bin[(i+1):nrow(current_bin), , drop = FALSE]
    vb1 <- bin_value(left_bin, R)
    vb2 <- bin_value(right_bin, R)

    matrix(c(x, vb1, vb2, vb1 + vb2), 
           ncol = 4,
           dimnames = list(NULL, c('candidate', 'vb1', 'vb2', 'vbsum')))
}

#' Calculate bin value (Vb) 
#'
#' Computes the inner product for each sample,
#'   then the bin value over all samples.
#'
#' @param dat A matrix containing the current bin
#' @param R resolution
#' @returns A dbl, the bin value
#' @importFrom matrixStats colMaxs
#' @keywords internal
bin_value <-function(dat, R){
    vals <- dat[, 4:ncol(dat), drop = FALSE]
    first <- vals[1, ]
    last <- vals[NROW(vals), ]
    maximum <- colMaxs(vals)
    
    inner_product <- ((maximum - first) * (maximum - last)) ^ R
    vb <- mean(inner_product)
}

#' Bin Evaluation Criterion
#'
#' Evaluates whether a bin can be split or not
#'
#' @param vbmax sum of maximums of left and right bin values 
#' @param vb original bin value
#' @param vb1max max bin value of left bin
#' @param vb2max max bin value of right bin
#' @param vnoise minimal bin value
#' @return TRUE if splitting improves bin value, FALSE otherwise
#' @keywords internal
bin_eval_test <- function(vbmax, vb, vb1max, vb2max, vnoise){
    all(vbmax > vb,
        vb1max > vnoise,
        vb2max > vnoise)
}