# vbdf class ----

get_bloc_var <- function(x) attr(x, "bloc_var")
get_var_type <- function(x) attr(x, "var_type")

#' Constructor for class vbdf
#'
#' @param x a data.frame
#' @param bloc_var character vector naming the variables to define voting blocs
#' @param var_type string, the type, discrete or continuous
#'
new_vbdf <- function(x, bloc_var = character(),
                     var_type = c("discrete", "continuous")){

    stopifnot(is.data.frame(x))
    # tibble protects attributes from dplyr verbs
    out <-
        tibble::new_tibble(x, nrow = nrow(x), class = "vbdf",
                          bloc_var = bloc_var, var_type = var_type)

    tibble::validate_tibble(out)

    return(out)
}

#' Validator for class vbdf
#'
#' @param x object to check
#' @param tolerance tolerance used when checking range of probability estimates

check_vbdf <- function(x, tolerance = sqrt(.Machine$double.eps)){

    stopifnot(is.data.frame(x))
    stopifnot("bloc_var" %in% names(attributes((x))))
    stopifnot("var_type" %in% names(attributes((x))))
    stopifnot(attr(x, "var_type") %in% c("discrete", "continuous"))


    stopifnot(rlang::has_name(x, get_bloc_var(x)))

    tol <- tolerance

    if(
        isFALSE(
            dplyr::summarize(x,
                             dplyr::across(dplyr::matches("^prob"),
                                           ~ all(.x > 0 - tol,
                                                 .x < 1 + tol)
                             )
            ) %>%
            all(na.rm = TRUE)
        )
    ) stop("Found a density estimate outside of normal range.")

    # Allow modeled probabilities out of normal range
    pr_lowbound <- ifelse(get_var_type(x) == "continuous", -2, 0)
    pr_uppbound <- ifelse(get_var_type(x) == "continuous",  3, 1)
    if(
        isFALSE(
            dplyr::summarize(x,
                             dplyr::across(dplyr::matches("^pr_turnout"),
                                           ~ all(.x > pr_lowbound - tol,
                                                 .x < pr_uppbound + tol)
                             )
            ) %>%
            all(na.rm = TRUE)
        )
    ) stop("Found a turnout probability outside of expected range.")

    if(
        isFALSE(
            dplyr::summarize(x,
                             dplyr::across(dplyr::matches("^pr_vote"),
                                           ~ all(.x > pr_lowbound - tol,
                                                 .x < pr_uppbound + tol)
                             )
            ) %>%
            all(na.rm = TRUE)
        )
    ) stop("Found a vote choice probability outside of expected range.")

    if(
        isFALSE(
            dplyr::summarize(x,
                             dplyr::across(dplyr::matches("^net_rep"),
                                           ~ all(
                                               .x > -1 - tol,
                                               .x <  1 + tol
                                           )
                             )
            ) %>%
            all(na.rm = TRUE)
        )
    ) stop("Found a value of net Republican votes outside of expected range.")

    return(TRUE)
}

#' Create a vbdf object
#'
#' Create a vbdf object holding bloc-level estimates of composition, turnout,
#' and/or vote choice. This function is mostly for internal use, but you may
#' want it to create a \code{vbdf} object from your own voting bloc analysis.
#' A valid \code{vbdf} object can be used in [vb_difference] and [vb_plot].
#'
#' @param data data.frame of voting-bloc results to convert to a \code{vbdf} object
#' @param bloc_var string, the name of the variable that defines the voting blocs
#' @param var_type string, the type of variable, discrete or continuous
#' @param tolerance tolerance used when checking range of probability estimates
#'
#' @return A \code{vbdf} object.
#'
#' @export

vbdf <-
    function(data, bloc_var, var_type = c("discrete", "continuous"),
             tolerance = sqrt(.Machine$double.eps)){

        var_type <- match.arg(var_type)

        vbdf <-
            new_vbdf(
                x = data,
                bloc_var = bloc_var,
                var_type = var_type
            )

        check_vbdf(vbdf, tolerance = tolerance)

        return(vbdf)
    }
