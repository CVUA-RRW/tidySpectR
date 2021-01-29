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
    UseMethod(bucket_aibin)

#' @rdname bucket_aibin
#' @param x A`collection` object to bucket
#' @param R resolution value, strictly positive and typically in the interval
#'   0 > R >= 1. 
#' @param noise_region A`collection` object containing a noise_region
#' @param cores Number of cores to allocate to the process for multprocessing
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @returns An updated version of x
#' @notes The amount of created bins is not directly dpeending of a high or low R.
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
#' # Sit back and relax
#' bucketted <- bucket_aibin(spectra, 0.2, noise, cores = 2)
#' }
#' @importFrom future plan makeClusterPSOCK cluster
#' @importFrom parallel stopCluster
bucket_aibin.collection <- function(x, R, noise_region, cores = 1,...){
    cl <- makeClusterPSOCK(cores)
    plan(cluster, workers = cl)
    
    # Determine Vnoise
    vnoise <- find_vnoise(noise_region, R)
    
    # Run the algorithm on the spectra region using the Vnoise value determined above
    splits <- recursive_split(x, R, vnoise)
    
    new_obj <- bucket_from_breaks(x, splits)
    new_obj$bucketted <- "Adaptive inteligent binning"
    
    stopCluster(cl)
    return(new_obj)
}

#' Find the max bin value in the noise region
#' 
#' @param noise_region A `collection` object containing the noise region
#' @param R resolution
#' @returns A dbl, maximum bin value in the noise region
#' @keywords internal
#' @importFrom purrr map2_dbl
find_vnoise <- function(noise_region, R){
    noise_bins <- recursive_split(noise_region, R, vnoise = 0)
    bin_start <- noise_bins[-length(noise_bins)]
    bin_end <- noise_bins[-1]
    
    vnoise <- map2_dbl(bin_start, bin_end,
                    ~ {bucket <- extract(noise_region, bin_start, bin_end)
                    bin_value(bucket$data, R)}) %>%
              max()
              
    return(vnoise)
}

#' Recursively split the spectra with the adaptive inteligent binning algorithm
#' 
#' @param current_bin A `collection` object containing the current bin
#' @param R resolution
#' @param vnoise Maximal bin value for the noise region.
#' @param breaks A vector of double that will be populated with the 
#'   binning positions
#' @returns A vector of dbl contianing the split positions
#' @keywords internal
#' @importFrom dplyr last
recursive_split <- function(current_bin, R, vnoise, breaks = NULL){
    while(TRUE){
        # Try to split the bin
        new_split <- divide_bin(current_bin, R, vnoise)
        
        if (!is.numeric(new_split)) {
            # If the bin cannot be split, append the right limit of the bin to 
            # the binning point and return the whole vector
            # this will be a splitting binning point
            breaks <- pull_breaks(current_bin) %>% 
                        last() %>%
                        append(breaks) %>%
                        sort()
            return(breaks)
        } else {
            # Extract the left-most bin of the split
            sub_bin <- extract(current_bin, -Inf, new_split)
            
            # Keep splittin' 
            # recursive call, will return when unsplitable bin is found
            breaks <- recursive_split(sub_bin, R, vnoise, breaks)
            
            # Moving on to the next bin
            current_bin <- extract(current_bin, last(breaks), Inf)
        }
    }
}

#' Find a new division splitting position for the current bin
#' 
#' @param current_bin A `collection` object containing the current bin
#' @param R resolution
#' @param vnoise Maximal bin value for the noise region.
#' @return A dbl, the new optimal splitting position or FALSE if not splitting is possible
#' @keywords internal
#' @importFrom purrr map_dfr
#' @importFrom dplyr slice_max
#' @importFrom furrr future_map_dfr furrr_options
divide_bin <- function(current_bin, R, vnoise){
    vb <- bin_value(current_bin$data, R)
    
    # Get candidate bin limits
    # Consider all limits in the interval minus the first and last
    candidates <- pull_breaks(current_bin)
    candidates <- candidates[-1]
    candidates <- candidates[-length(candidates)]
    
    # Get bin values for each candidates
    split_values <- future_map_dfr(candidates, 
                                    ~ sub_bins_values(.x,
                                    current_bin, 
                                    R),
                                    .options = furrr_options(packages= c("tibble", 
                                                                         "dplyr")))
    
    # Find candidate which maximizes Vbsum
    new_split <- slice_max(split_values, vbsum) %>%
                 slice_min(candidate) # in case of mutliple occurences of max(vbsum)
    
    # Is it worth spliting?
    if ( bin_eval_test(vbmax = new_split$vbsum, 
                  vb = vb, 
                  vb1max = new_split$vb1, 
                  vb2max = new_split$vb2,
                  vnoise = vnoise)
                  ){
        return(pull(new_split, candidate))
    } else {
        return(FALSE)
    }
}

#' Calculate the bin values for each splits of the current bin
#'
#' @param x spliting point
#' @param current_bin A `collection` object containing the current bin
#' @param R resolution
#' @return A tibble with bin values:
#' \describe{
#'   \item{candidate}{position of the new splitting point}
#'   \item{vb1}{left-bin value}
#'   \item{vb2}{right-bin value}
#'   \item{vbsum}{sum of the left and righ bin values}
#' }
#' @importFrom tibble tibble
#' @keywords internal
sub_bins_values <- function(x, current_bin, R){
    left_bin <- extract(current_bin, -Inf, x)
    right_bin <- extract(current_bin, x, Inf)
    vb1 <- bin_value(left_bin$data, R)
    vb2 <- bin_value(right_bin$data, R)
    tibble(candidate = x,
           vb1 = vb1,
           vb2 = vb2,
           vbsum = vb1 + vb2)
}

#' Calculate bin value (Vb) 
#'
#' Computes the inner product for each sample,
#'   then the bin value over all samples.
#'
#' @param dat a date frame or tibble with the columns:
#' \describe{
#'   \item{id}{unique identifier}
#'   \item{bin_start}{left border of the bin}
#'   \item{bin_end}{right border of the bin}
#'   \item{values}{bin intensity}
#' }
#' @param R resolution
#' @returns A dbl, the bin value
#' @importFrom dplyr group_by group_modify ungroup pull n
#' @keywords internal
bin_value <- function(dat, R) {
    dat %>% 
        group_by(id) %>%
        group_modify(~ bin_value_inner_product(.x, .y, R)) %>%
        ungroup() %>%
        summarise(bin_value = sum(product) / n()) %>%
        pull(bin_value)
}

#' Computes the inner product of the bin value equation 
#'   for a dataset with a single sample
#'
#' @param dat A tibble or dat frame with columns
#' \describe{
#'   \item{bin_start}{left border of the bin}
#'   \item{bin_end}{right border of the bin}
#'   \item{values}{bin intensity}
#' }
#' @param id will be ignored
#' @param R resolution
#' @returns a tibble with column product containing 
#'   the inner product of the bin value equation for 1 sample
#' @importFrom dplyr slice_min pull
#' @importFrom tibble tibble
#' @keywords internal
bin_value_inner_product <- function(dat, id, R){
    int_left <- dat %>%
                slice_min(bin_start) %>%
                pull(values)
    
    int_right <- dat %>%
                slice_max(bin_end) %>%
                pull(values)
                
    int_max <- dat %>% 
               pull(values) %>%
               max()
               
    inner_product <- ((int_max - int_left)*(int_max - int_right))^R
    return(tibble(product = inner_product))
}

#' Bin Evaluation Criterion
#'
#' Evaluates whether a bin can be split or not
#'
#' @param vbmax maximum of left and right bin values sum
#' @param vb original bin value
#' @param vb1max max bin value of left bin
#' @param vb2max max bin value of right bin
#' @param vnoise minimal bin value
#' @return TRUE if splitting improves bin value, FALSE otherwise
#' @keywords internal
bin_eval_test <- function(vbmax, vb, vb1max, vb2max, vnoise){
    if ((vbmax > vb) & (vb1max > vnoise) & (vb2max > vnoise)){
        TRUE
    } else {
        FALSE
    }
}