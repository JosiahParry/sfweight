% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_nb_band.R
\name{st_nb_band}
\alias{st_nb_band}
\title{Neighbors from a distance band}
\usage{
st_nb_band(x, lower = 0, upper = 0.01, ...)
}
\arguments{
\item{x}{An sf or sfc object.}

\item{lower}{The lower threshold of the distance band. It is recommended to keep this as 0.}

\item{upper}{The upper threshold of the distance band.}

\item{...}{Passed to \code{spdep::dnearneigh()}.}
}
\description{
Creates neighbors based on a distance band.
}
\seealso{
Other neighbors: 
\code{\link{st_contiguity}()},
\code{\link{st_knn}()}
}
\concept{neighbors}
