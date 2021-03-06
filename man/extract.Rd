% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mask_extract.R
\name{extract}
\alias{extract}
\alias{extract.collection}
\title{Extracts a spectral region}
\usage{
extract(x, ...)

\method{extract}{collection}(x, from, to, overlaps = "keep", skip = FALSE, ...)
}
\arguments{
\item{x}{A\code{collection} object}

\item{...}{further arguments passed to or from other methods(not
currenctly used).}

\item{from, to}{Coordinates of the region to extract.
Use -Inf or Inf to extract up to an end of the spectra.}

\item{overlaps}{What to do with the bins ovelapping the edge of the mask.
Use either \code{keep} of \code{remove}.}

\item{skip}{Skip the creation of of processor step. If TRUE, this step will not be added to
the list of processing steps. Typically reserved for nested function calls.}
}
\value{
An updated version of \code{collection}.
}
\description{
\code{extract} extracts a spectral region
}
\details{
The upper limit in not included: [min, max).
}
\examples{
library(tidySpectR)

# Removing edges
extract(fa_nmr, from = 7.2, to = -0.5)
}
