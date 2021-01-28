#' H1-NMR spectra of Egg yolk fatty-acids
#' 
#' A dataset containing H1-NMR fatty-acid spectra for 6 egg yolk samples.
#'   
#' The spectra were pre-trimmed to the 0-9 ppm space and bucketted with a bin
#'   width of 0.004 ppm, resulting in 2 250 bins.
#'   
#' The spectra are divided in two categories with 3 samples each:
#'   eggs from conventional hatching and eggs from organic hatching.
#'
#' @format A collection object
#' \describe{
#'   \item{data}{Tibble containing the spectral data}
#'   \item{normalized}{Normalization method flag}
#'   \item{bucketted}{Bucketting method flag}
#'   \item{labels}{Tibble associating each sample to a condition}
#' }
"fa_nmr"