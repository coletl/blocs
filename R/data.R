#' Sample of 2020 ANES cumulative data file
#'
#' Several columns from the American National Election Studies' 2020 cumulative data file.
#' The dataset includes a 10% random sample of respondents, stratified by election year.
#'
#' @format A data frame with 6,822 rows and 12 columns:
#' \describe{
#'     \item
#'     \item
#'     \item{year}{election year}
#'     \item{respid}{respondent identifier}
#'     \item{weight}{survey weight}
#'     \item{race}{respondent race}
#'     \item{gender}{respondent gender}
#'     \item{educ}{respondent education level}
#'     \item{age}{respondent age}
#'     \item{racialres}{respondent racial resentment score}
#' }
#'
#' @source \url{https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/}
"anes_sample"
