#' Calculate differences in bloc contributions
#'
#' Use vbdf output to calculate differences
#' in blocs' net Republican vote contributions.
#'
#' @param vbdf      data.frame holding the results of voting bloc analyses.
#' @param estimates  character vector naming the column(s) in \code{vbdf} with
#'   which to compute differences.
#' @param sort_col  character vector naming the column(s) in \code{vbdf} to use
#'   for sorting before calling \link[base]{diff}.
#' @param tolerance tolerance used when checking range of probability estimates
#'
#' @return A \code{vbdf} object, plus two types of columns:
#' for each column named in \code{estimates}, a column named \code{diff_*} containing the
#' difference in each estimate across \code{sort_col} values,
#' \code{comp}, which contains a string tag for the rows compared (e.g., 2020-2016),
#'
#' @return A \code{vbdf} object.
#'
#' @importFrom dplyr %>%
#'
#' @export

vb_difference <-
    function(vbdf,
             estimates = grep("prob|pr_turnout|pr_votedem|pr_voterep|cond_rep|net_rep",
                              names(vbdf), value = TRUE),
             sort_col = "year", tolerance = sqrt(.Machine$double.eps)){

    stopifnot(length(sort_col) == 1)
    if(length(unique(vbdf[[sort_col]])) < 2)
        stop(sprintf("Need multiple values in vbdf[[%s]] to calculate difference", sort_col))

    check_vbdf(vbdf, tolerance = tolerance)
    if(!rlang::has_name(vbdf, sort_col)) stop(sprintf("Sorting column %s not found in data", sort_col))

    bloc_var     <- get_bloc_var(vbdf)
    var_type     <- get_var_type(vbdf)
    resample_col <- if(rlang::has_name(vbdf, "resample")) "resample" else NULL

    vbdiff <-
        vbdf %>%

        dplyr::group_by(dplyr::across(dplyr::all_of(c(bloc_var, resample_col,
                                                      dplyr::group_vars(vbdf))
                                                      )
                                     )
                            ) %>%
        dplyr::arrange(dplyr::all_of(sort_col)) %>%

        dplyr::transmute(
            dplyr::across(
                dplyr::all_of(estimates),
                .names = "diff_{.col}",
                .fns   =          ~ dplyr::lead(.x)               - .x),
            comp = sprintf("%s-%s",
                           dplyr::lead(.data[[sort_col]]), .data[[sort_col]])
        ) %>%
        collapse::colorderv(neworder = c("comp", resample_col, bloc_var)) %>%
        dplyr::ungroup()

    # Remove invalid differences (last rows of each group)
    bad_diff <- grepl("NA", vbdiff[["comp"]])
    vbdiff <- dplyr::filter(vbdiff, !bad_diff)

    # group_by removed the vbdf class and attributes
    # skip validation because differences may be within bounds
    out <- new_vbdf(x = vbdiff,
                    bloc_var = bloc_var, var_type = var_type)

    return(out)
}
