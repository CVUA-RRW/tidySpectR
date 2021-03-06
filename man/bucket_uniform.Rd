% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bucket_uniform.R
\name{bucket_uniform}
\alias{bucket_uniform}
\alias{bucket_uniform.collection}
\title{Performs uniform bucketting}
\usage{
bucket_uniform(x, ...)

\method{bucket_uniform}{collection}(x, width = NULL, N = NULL, skip = FALSE, ...)
}
\arguments{
\item{x}{A\code{collection} object}

\item{...}{further arguments passed to or from other methods(not
currenctly used).}

\item{width}{Bin width}

\item{N}{Number of bins to create}

\item{skip}{Skip the creation of of processor step. If TRUE, this step will not be added to
the list of processing steps. Typically reserved for nested function calls.}
}
\value{
An updated version of \code{collection}.
}
\description{
Bucket a collection of spectra to a given bin width or
number of bins
}
\details{
Provide either \code{width} or \code{N}
}
\examples{
library(tidySpectR)

bucket_uniform(fa_nmr, width= 0.1)

bucket_uniform(fa_nmr, N = 100)
}
