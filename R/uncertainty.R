################ Uncertainty ##############

boot_mat <- function(nrow, iters, weight = NULL){
    # For zero iterations, return the original row indices
    if(iters == 0){
        out <- matrix(1:nrow)
        colnames(out) <- "original"
    } else {
        out <-
            replicate(iters,
                      sample.int(nrow, replace = TRUE,
                                 prob = weight))

        # unique, sortable tags for each resample
        iter_tags <- formatC(1:iters, width = 8, format = "d", flag = "0")
        colnames(out) <- sprintf("resample-%s", iter_tags)

    }

    return(out)
}


#' Compute uncertainty for voting blocs analysis
#'
#' This generic function will take the result of a voting blocs analysis run
#' with bootstrap iterations and compute resampling-based uncertainty estimates.
#'
#' @param object a \code{vbdf} or \code{vbdiff} object.
#' @param ... further arguments to pass to methods
#'
#' @export

vb_uncertainty <- function(object, type, estimates, ...) UseMethod("vb_uncertainty")


#' @describeIn vb_uncertainty Uncertainty for a vbdf object
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
#' @param bin_col character vector naming the column(s) that define the bins. Used only when  \code{type} is \code{"binned"}.
#' @return A \code{vbdf} object with additional columns for each combination
#'   of \code{estimates} and \code{funcs}.
#'
#' @export vb_uncertainty.vbdf
#' @export

vb_uncertainty.vbdf <-
    function(vbdf, type = c("discrete", "continuous", "binned"),
             estimates = c("prob", "pr_turnout", "pr_voterep", "pr_votedem", "net_rep"),
             na.rm = FALSE,
             funcs = c("mean", "median", "low", "high"),
             low_ci = 0.025, high_ci = 0.975,
             bin_col){

        # check_vbdiff(vbdiff)
        if(length(type) > 1) type <- get_var_type(vbdf)

        stopifnot(rlang::has_name(vbdf, estimates))

        type <- match.arg(type)
        bloc_var <- get_bloc_var(vbdf)

        ind_orig <- which(vbdf$resample == "original")

        if(is.character(funcs))
            funcs <-
            list(
                # original = ~ .x[ind_orig],
                mean     = ~ mean(.x[-ind_orig],     na.rm = na.rm),
                median   = ~ median(.x[-ind_orig],   na.rm = na.rm),
                low      = ~ quantile(.x[-ind_orig], prob = low_ci, na.rm = na.rm),
                high     = ~ quantile(.x[-ind_orig], prob = high_ci, na.rm = na.rm)
            )[funcs]

        switch(type,
               discrete =
                   {
                       uncertainty_summary <-
                           # For each subgroup, calculate summary stats across iterations
                           vbdf %>%
                           dplyr::group_by(
                               dplyr::across(dplyr::all_of(
                                   c(dplyr::group_vars(vbdf), bloc_var)))
                               ) %>%
                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             .fns = funcs
                               )
                           )
                   },
               binned =
                   {
                       if(missing(bin_col)) stop("Missing required argument bin_col")

                       uncertainty_summary <-

                           vbdf %>%
                           # Begin by integrating estimates within bin and iteration
                           dplyr::group_by(dplyr::across(dplyr::all_of(c("resample", dplyr::group_vars(vbdf), bin_col)))) %>%

                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             sum),
                           ) %>%

                           # For each subgroup, calculate summary stats across iterations
                           dplyr::group_by(dplyr::across(dplyr::all_of(c(dplyr::group_vars(vbdf), bin_col)))) %>%
                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             .fns = funcs
                               )
                           )
                   },
               continuous =
                   {
                       uncertainty_summary <-
                           vbdf %>%
                           # Across iterations, calculate summary stats
                           dplyr::group_by(dplyr::across(dplyr::all_of(c(dplyr::group_vars(vbdf), bloc_var)))) %>%

                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             .fns = funcs
                               )
                           )
                   }
               )

        # Use custom class to protect attributes from dplyr verbs
        out <- new_vbdf(uncertainty_summary,
                        bloc_var = bloc_var, var_type = get_var_type(vbdf))

        return(out)

    }



#' @describeIn vb_uncertainty Uncertainty for a vbdiff object
#'
#' @param vbdiff    a \code{vbdiff} object, the result of \code{vb_difference()}.
#' @return A \code{vbdiff} object with additional columns for each combination
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

        if(missing(estimates)) stop("Missing required argument estimates")

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


        switch(type,
               discrete =
                   {
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
                   },
               binned =
                   {
                       if(missing(bin_col)) stop("Missing required argument bin_col")

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
                   },

               continuous =
                   {

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
               )

        # Use custom class to protect attributes from dplyr verbs
        out <-
            new_vbdiff(x = uncertainty_summary, bloc_var = bloc_var,
                       var_type = get_var_type(vbdiff),
                       diff_col  = attr(vbdiff, "diff_col"))

        return(out)

    }

