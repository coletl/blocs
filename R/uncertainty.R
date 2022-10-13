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

#' Constructor for vbdf summaries
#'
#' @param x data.frame of uncertainty summary
#' @param bloc_var string, the name of the variable that defines the voting blocs
#' @param var_type string, the type of variable, discrete or continuous
#' @param summary_type string, the type of variable, discrete or continuous
#' @param resamples numeric, the number of bootstrap resamples
#'
#' @return A \code{vbsum} object

new_vbsum <- function(x, bloc_var, var_type, summary_type, resamples){
    stopifnot(is.data.frame(x))
    # tibble protects attributes from dplyr verbs
    out <-
        tibble::new_tibble(x, nrow = nrow(x), class = "vbsum",
                           bloc_var = bloc_var, var_type = var_type,
                           summary_type = summary_type, resamples = resamples)

    tibble::validate_tibble(out)

    return(out)
}


#' Uncertainty for a vbdf objects
#'
#' @param object a \code{vbdf} object, usually the output of [vb_discrete], [vb_continuous], or [vb_difference].
#' @param type a string naming the type of independent variable summary. Use
#'   \code{"binned"} when using the output of [vb_continuous] plus a binned version of the continuous bloc variable.
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
#' @return A summary object with additional columns for each combination
#'   of \code{estimates} and \code{funcs}.
#'
#' @export

vb_summary <-
    vb_uncertainty <-
    function(object, type = c("discrete", "continuous", "binned"),
             estimates = grep("prob|pr_turnout|pr_votedem|pr_voterep|cond_rep|net_rep",
                              names(object), value = TRUE),
             na.rm = FALSE,
             funcs = c("mean", "median", "low", "high"),
             low_ci = 0.025, high_ci = 0.975,
             bin_col){

        check_vbdf(object)
        if(identical(type, c("discrete", "continuous", "binned")))
            type <- get_var_type(object)
        else type <- match.arg(type, c("discrete", "binned", "continuous"))

        if(!all(rlang::has_name(object, estimates))){
            miss_estim <- paste(setdiff(estimates, names(object)), collapse = ", ")
            stop(sprintf("%s column not found in data\n", miss_estim))
        }

        bloc_var <- get_bloc_var(object)

        if(is.character(funcs))
            funcs <-
            list(
                mean     = ~ mean(.x,     na.rm = na.rm),
                median   = ~ median(.x,   na.rm = na.rm),
                low      = ~ quantile(.x, prob = low_ci, na.rm = na.rm),
                high     = ~ quantile(.x, prob = high_ci, na.rm = na.rm)
            )[funcs]

        switch(type,
               discrete =
                   {
                       uncertainty_summary <-
                           # For each subgroup, calculate summary stats across iterations
                           object %>%
                           dplyr::group_by(
                               dplyr::across(dplyr::all_of(
                                   c(dplyr::group_vars(object), bloc_var)))
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

                           object %>%
                           # Begin by integrating estimates within bin and iteration
                           dplyr::group_by(dplyr::across(dplyr::all_of(c("resample", dplyr::group_vars(object), bin_col)))) %>%

                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             sum),
                           ) %>%

                           # For each subgroup, calculate summary stats across iterations
                           dplyr::group_by(dplyr::across(dplyr::all_of(c(dplyr::group_vars(object), bin_col)))) %>%
                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             .fns = funcs
                               )
                           )
                   },
               continuous =
                   {
                       uncertainty_summary <-
                           object %>%
                           # Across iterations, calculate summary stats
                           dplyr::group_by(dplyr::across(dplyr::all_of(c(dplyr::group_vars(object), bloc_var)))) %>%

                           dplyr::summarize(
                               dplyr::across(dplyr::all_of(estimates),
                                             .fns = funcs
                               )
                           )
                   }
               )

        n_resamples <- length(unique(object$resample))

        # Use custom class to protect attributes from dplyr verbs
        out <- new_vbsum(uncertainty_summary,
                         bloc_var = bloc_var, var_type = get_var_type(object),
                         summary_type = type, resamples = n_resamples)

        return(out)

    }



