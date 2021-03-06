% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous.R
\name{estimate_density}
\alias{estimate_density}
\title{Estimate continuous density}
\usage{
estimate_density(x, min, max, n_points = 100, w, ...)
}
\arguments{
\item{x}{numeric vector}

\item{min}{scalar, lower bound of evaluation points}

\item{max}{scalar, upper bound of evaluation points}

\item{n_points}{number of evaluation points (estimates)}

\item{w}{vector of weights}

\item{...}{further arguments to pass to \link[ks]{kde}}
}
\description{
Run \link[ks]{kde} to estimate the
weighted density of \code{x} at \code{n_points}
evenly spaced points between \code{min} and {max}.
}
