% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_weights.R
\name{st_kernel_weight}
\alias{st_kernel_weight}
\title{Calculate Kernel Weights}
\usage{
st_kernel_weight(x, nb, kernel = "uniform")
}
\arguments{
\item{x}{Spatial points. Typically the \code{geometry} column of an sf object.}

\item{nb}{A nb neighbor list such as created from \code{st_knn(x, k = 1)}.}

\item{kernel}{One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic".}
}
\description{
Calculate Kernel Weights
}
\seealso{
Other weights: 
\code{\link{st_inverse_weights}()},
\code{\link{st_weights}()}
}
\concept{weights}
