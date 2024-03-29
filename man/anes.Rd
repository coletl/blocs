% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{anes}
\alias{anes}
\title{Sample of 2020 ANES cumulative data file}
\format{
A data frame with 68,224 rows and 13 columns:
\describe{
    \item{year}{election year}
    \item{respid}{respondent identifier}
    \item{weight}{survey weight}
    \item{race}{respondent race}
    \item{gender}{respondent gender}
    \item{educ}{respondent education level}
    \item{age}{respondent age}
    \item{voted}{respondent's voter turnout}
    \item{vote_pres}{respondent's presidential vote}
    \item{vote_pres_dem}{flag indicating Democratic presidential vote choice}
    \item{vote_pres_rep}{flag indicating Republican presidential vote choice}
    \item{vote_pres3}{Three-valued voting behavior DV coded as follows: -1 for Democrat vote choice, 0 for third-party vote, 1 for Republican vote choice, and NA for no vote.
    }
}
}
\source{
\url{https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/}
}
\usage{
anes
}
\description{
Selected columns from the American National Election Studies' 2020 cumulative data file.
The final column is an example of the three-valued variable for voting behavior,
to be passed to the `dv_vote3` argument,
}
\keyword{datasets}
