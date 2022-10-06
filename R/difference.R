#' Calculate differences in bloc contributions
#'
#' Use vbdf output to calculate differences
#' in blocs' net Republican vote contributions.
#'
#' @param vbdf      data.frame holding the results of voting bloc analyses.
#' @param diff_col  character vector naming the column(s) in \code{vbdf} with
#'   which to compute differences.
#' @param sort_col  character vector naming the column(s) in \code{vbdf} to use
#'   for sorting before calling \link[base]{diff}.
#'
#' @return A \code{vbdiff} object, similar to \code{vbdf} plus two columns:
#'   \code{diff_*}, which holds the difference in \code{vbdf[[diff_col]]} across
#'   consecutive years in \code{vbdf$year}; and \code{comp}, which holds a
#'   string tag for the years compared.
#'
#' @return A \code{vbdiff} object.
#'
#' @importFrom dplyr %>%
#'
#' @export

vb_difference <-
    function(vbdf,
             diff_col = intersect(names(vbdf),
                                  c("prob", "pr_turnout",
                                    "pr_votedem", "pr_voterep",
                                    "cond_rep", "net_rep")),
             sort_col = "year"){

    stopifnot(length(sort_col) == 1)
    check_vbdf(vbdf)
    stopifnot(rlang::has_name(vbdf, sort_col))

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
                dplyr::all_of(diff_col),
                .names = "diff_{.col}",
                .fns   =          ~ dplyr::lead(.x)               - .x),
            comp = sprintf("%s-%s",
                           dplyr::lead(.data[[sort_col]]), .data[[sort_col]])
        ) %>%
        collapse::colorderv(neworder = c("comp", resample_col, bloc_var)) %>%
        dplyr::ungroup()

    # Remove invalid differences (last rows of each group)
    bad_diff <- grepl("NA", vbdiff[["comp"]])
    vbdiff <- filter(vbdiff, !bad_diff)

    # group_by removed the vbdf class and attributes
    out <- new_vbdiff(x = vbdiff,
                      bloc_var = bloc_var, var_type = var_type,
                      diff_col = diff_col)

    return(out)
}
