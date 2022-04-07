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
#' @import ggplot2
#' @import dplyr
#'
#' @export

vb_diff <- function(vbdf, diff_col, sort_col = "year"){

    require(dplyr)

    stopifnot(length(sort_col) == 1)
    check_vbdf(vbdf)
    stopifnot(rlang::has_name(vbdf, sort_col))

    bloc_var     <- get_bloc_var(vbdf)
    resample_col <- if(rlang::has_name(vbdf, "resample")) "resample" else NULL

    vbdiff <-
        vbdf %>%

        group_by(across(all_of(c(bloc_var, resample_col)))) %>%
        arrange(across(all_of(c(bloc_var,  resample_col, sort_col)))) %>%

        mutate(
            across(
                all_of(diff_col),
                .names = "diff_{.col}",
                .fns   =          ~ lead(.x)               - .x),
            comp = sprintf("%s-%s", lead(.data[[sort_col]]), .data[[sort_col]])
        ) %>%
        ungroup()

    # group_by removed the vbdf class and attributes
    out <- new_vbdiff(vbdiff,
                      bloc_var = bloc_var, diff_col = diff_col)

    return(out)
}
