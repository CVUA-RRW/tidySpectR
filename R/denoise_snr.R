#' Remove noise bins 
#'
#' Average maximal intensity is calculated form the noise region 
#'   and each bin in the spectra. Bins whose average maximal intensity 
#'   are not above the signal to noise ratio are discarded.
#' @aliases filter_noise filter_noise.collection
#' @export
filter_noise <- function(x, ...)
    UseMethod("filter_noise")

#' @rdname filter_noise
#' @param x A`collection` object to bucket
#' @param noise_region A`collection` object containing a noise_region
#' @param snr A dbl, signal to noise ratio
#' @param ... further arguments passed to or from other methods(not
#'   currenctly used).
#' @returns An updated version of x
#' @export
#' @examples
#' library(tidySpectR)
#' 
#' # First we normalize the dataset and extract noise and sepctra regions
#' normalized <- fa_nmr %>% 
#'                   normalize_internalStandard(3.58, 3.64)
#' spectra <- normalized %>% extract(0, 7.2)
#' noise <- normalized %>% mask(0, 7.2, overlaps = 'remove')
#'
#' filtered <- filter_noise(spectra, noise, 3)
#' @importFrom dplyr group_by summarise pull filter right_join
filter_noise.collection <- function(x, noise_region, snr, ...){
    new_obj <- x
    
    noise_level <- noise_region$data %>%
                   group_by(id) %>%
                   summarise(values = max(values), .groups = "drop") %>%
                   pull(values) %>%
                   mean()
    
    signal_level <- x$data %>%
                    group_by(id, bins) %>%
                    summarise(values = max(values), .groups = "drop") %>%
                    group_by(bins) %>%
                    summarise(values = mean(values), .groups = "drop")
                    
    keep <- signal_level %>%
            filter(bins > (snr * noise_level))
            
    new_obj$data <- right_join(x$data, select(keep, bins))
    
    return(new_obj)
}
