################ Uncertainty ##############


boot_mat <- function(nrow, iters, weight = NULL){
    itermat <-
        replicate(iters, sample.int(nrow, replace = TRUE, prob = weight))

    out <- cbind(1:nrow, itermat)
    colnames(out) <- c("resample-0",
                       sprintf("resample-%s", 1:iters))

    return(out)
}


#' Compute uncertainty for voting blocs analysis
#'
#' @param object a \code{vbdf} or \code{vbdiff} object.
#' @param ... further arguments to pass to methods
#'
#' @export

vb_uncertainty <- function(object, ...) UseMethod("vb_uncertainty")

#' Compute uncertainty for a vbdf object
#'
#' @param vbdf a \code{vbdf} object, usually the result of \code{vb_discrete} or
#'   \code{vb_continuous}.
#' @param type a string naming the type of independent variable summary. Use
#'   \code{"binned"} when working with binned output of \code{vb_continuous()}.
#' @param estimates character vector naming columns for which to calculate
#'   uncertainty estimates.
#' @param na.rm logical indicating whether to remove \code{NA} values in
#'   \code{estimates}.
#' @param funcs character vector of summary functions to apply to
#'   \code{estimates}. Alternatively, supply your own list of functions, which
#'   should accept a numeric vector input and return a scalar.
#' @param low_ci  numeric. If you include the string \code{"low"} in \code{funcs}, then use this argument to control the lower bound of the confidence interval.
#' @param high_ci numeric. If you include the string \code{"high"} in \code{funcs}, then use this argument to control the upper bound of the confidence interval.
#' @param bin_col character vector naming the columns that contain the bins. Only used if  \code{type} is \code{"binned"}.
#' @return a \code{vbdf} object with additional columns for each combination
#'   of \code{estimates} and \code{funcs}.
#'
#' @export vb_uncertainty.vbdf
#' @export

vb_uncertainty.vbdf <-
    function(vbdf, type = c("continuous", "binned", "discrete"),
             estimates, na.rm = FALSE,
             funcs = c("original", "mean", "median", "low", "high"),
             low_ci = 0.025, high_ci = 0.975,
             bin_col){

        require(dplyr)

        # check_vbdiff(vbdiff)
        type <- match.arg(type)

        bloc_var <- get_bloc_var(vbdf)

        if(is.character(funcs))
            funcs <-
            list(
                original = ~ .x[resample == "resample-0"],
                mean     = ~ mean(.x, na.rm = na.rm),
                median   = ~ median(.x, na.rm = na.rm),
                low      = ~ quantile(.x, prob  = low_ci, na.rm = na.rm),
                high     = ~ quantile(.x, prob = high_ci, na.rm = na.rm)
            )[funcs]


        if(type == "discrete"){

            uncertainty_summary <-
                # For each subgroup, calculate summary stats across iterations
                vbdf %>%
                group_by(across(all_of(c(bin_col)))) %>%
                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }

        if(type == "binned"){

            uncertainty_summary <-

                vbdf %>%
                # Begin by integrating estimates within bins and iteration
                group_by(across(all_of(c("resample", bin_col)))) %>%

                summarize(
                    across(all_of(estimates),
                           sum),
                ) %>%

                # For each subgroup, calculate summary stats across iterations
                group_by(across(all_of(c(bin_col)))) %>%
                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }

        if(type == "continuous"){

            uncertainty_summary <-
                vbdf %>%
                # Across iterations, calculate summary stats
                group_by(across(all_of(c(bloc_var)))) %>%

                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }

        # Use custom class to protect attributes from dplyr verbs
        out <- new_vbdf(uncertainty_summary,
                        bloc_var = bloc_var, var_type = get_var_type(vbdf))

        return(out)

    }


#' Compute uncertainty for a vbdiff object
#'
#' @param vbdiff    a \code{vbdiff} object, the result of \code{vb_diff}.
#' @param type a string naming the type of independent variable summary. Use
#'   \code{"binned"} when working with binned output of \code{vb_continuous()}.
#' @param estimates character vector naming columns for which to calculate
#'   uncertainty estimates.
#' @param bin_col character vector naming columns that contain the bins. Used
#'   only if  \code{type} is \code{"binned"}.
#' @param na.rm logical indicating whether to remove \code{NA} values in
#'   \code{estimates}.
#' @param funcs character vector of summary functions to apply to
#'   \code{estimates}. Alternatively, supply your own list of functions, which
#'   should accept a numeric vector input and return a scalar.
#' @param low_ci  numeric. If you include the string \code{"low"} in
#'   \code{funcs}, then use this argument to control the lower bound of the
#'   confidence interval.
#' @param high_ci numeric. If you include the string \code{"high"} in
#'   \code{funcs}, then use this argument to control the upper bound of the
#'   confidence interval.
#' @return a \code{vbdiff} object with additional columns for each combination
#'   of \code{estimates} and \code{funcs}.
#'
#' @import dplyr
#' @export vb_uncertainty.vbdiff
#' @export

vb_uncertainty.vbdiff <-
    function(vbdiff, type = c("continuous", "binned", "discrete"),
             estimates, bin_col, na.rm = FALSE,
             funcs = c("original", "mean", "median", "low", "high"),
             low_ci = 0.025, high_ci = 0.975){

        require(dplyr)

        type <- match.arg(type)

        bloc_var <- get_bloc_var(vbdiff)

        funcs <-
            list(
                original = ~ as.numeric(.x[resample == "resample-0"]),
                mean     = ~ mean(.x, na.rm = na.rm),
                median   = ~ median(.x, na.rm = na.rm),
                low      = ~ quantile(.x, prob  = low_ci, na.rm = na.rm),
                high     = ~ quantile(.x, prob = high_ci, na.rm = na.rm)
            )[funcs]


        if(type == "discrete"){

            uncertainty_summary <-
                # For each subgroup, calculate summary stats across iterations
                vbdiff %>%
                group_by(across(all_of(c("comp", bloc_var)))) %>%
                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }

        if(type == "binned"){

            uncertainty_summary <-

                vbdiff %>%
                # Begin by integrating estimates within bins and iteration
                group_by(across(all_of(c("comp", "resample", bin_col)))) %>%

                summarize(
                    across(all_of(estimates),
                           sum),
                ) %>%

                # For each subgroup, calculate summary stats across iterations
                group_by(across(all_of(c("comp", bin_col)))) %>%
                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }


        if(type == "continuous"){

            uncertainty_summary <-
                vbdiff %>%
                # Across iterations, calculate summary stats
                group_by(across(all_of(c("comp", bloc_var)))) %>%

                summarize(
                    across(all_of(estimates),
                           .fns = funcs
                    ),
                    boot_iters  = n_distinct(resample)
                )
        }

        # Use custom class to protect attributes from dplyr verbs
        out <-
            new_vbdiff(x = uncertainty_summary, bloc_var = bloc_var,
                       var_type = get_var_type(vbdiff),
                       diff_col  = attr(vbdiff, "diff_col"))

        return(out)

    }

