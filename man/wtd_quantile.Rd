% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous.R
\name{wtd_quantile}
\alias{wtd_quantile}
\title{Weighted quantiles ' ' This function calls \link[collapse]{fnth} repeatedly
over a vector of probabilities to produce output like \link[stats]{quantile}.
This function is fast, with minimal dependencies, but does not accept negative weights.
Importantly, this function does not call Hmisc::wtd.quantile, which fails for non-integer weights.}
\usage{
wtd_quantile(x, probs = seq(0, 1, 0.25), weight, na.rm = FALSE, ...)
}
\arguments{
\item{x}{numeric vector.}

\item{probs}{numeric vector of probabilities.}

\item{weight}{numeric vector of non-negative weights.}

\item{na.rm}{logical whether to remove missing values}

\item{...}{further arguments passed to \link[collapse]{fnth}.}
}
\description{
Weighted quantiles ' ' This function calls \link[collapse]{fnth} repeatedly
over a vector of probabilities to produce output like \link[stats]{quantile}.
This function is fast, with minimal dependencies, but does not accept negative weights.
Importantly, this function does not call Hmisc::wtd.quantile, which fails for non-integer weights.
}
