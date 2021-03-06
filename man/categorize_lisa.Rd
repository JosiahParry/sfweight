% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moran_test.R
\name{categorize_lisa}
\alias{categorize_lisa}
\title{Categorize LISA}
\usage{
categorize_lisa(x, x_lag, scale = TRUE)
}
\arguments{
\item{x}{Numeric vector.}

\item{x_lag}{The spatial lag of x as calculated by \code{st_lag()}.}

\item{scale}{Whether or not to standardize \code{x}. Defaults to \code{TRUE}.}
}
\description{
Categorize LISA
}
\seealso{
Other other: 
\code{\link{st_cardinalties}()},
\code{\link{st_nb_lag_cumul}()},
\code{\link{st_nb_lag}()}
}
\concept{other}
