#' Plot voting blocs' year-to-year difference
#'
#'
#' @rdname plot_vbdiff
#' @param vbdiff a data.frame of year-to-year differences in the format of the output from \link{vb_difference}.
#' @param x_col      string naming the column that defines voting blocs.
#' @param y_col      string naming the column of point estimates.
#' @param ymin_col   string naming the column to plot as the lower bound of the confidence interval. Valid only after calculating bootstrapped uncertainty. See \link{vb_uncertainty}.
#' @param ymax_col   string naming the column to plot as the upper bound of the confidence interval. Valid only after calculating bootstrapped uncertainty. See \link{vb_uncertainty}.
#' @param discrete logical indicating whether voting blocs are defined along a discrete (not continuous) variable.
#'
#' @import ggplot2
#'
#' @export plot_vbdiff
#' @export
#'
plot_vbdiff <-
    function(vbdiff, x_col = get_bloc_var(vbdiff),
             y_col, ymin_col, ymax_col,
             discrete = length(unique(vbdiff[[x_col]])) < 20
             ){

    ci <- !missing(ymin_col) & !missing(ymax_col)

    if(length(x_col) > 1) stop("Choose one independent variable to plot.")

    out <-
        ggplot(vbdiff) +
        aes(x = .data[[x_col]], y = .data[[y_col]]) +
        geom_hline(yintercept = 0) +
        theme_bw()

    out <- if(discrete) out + geom_point() else out + geom_line()

    if(ci) {
        out <- out + aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]])
        if(discrete) out <- out + geom_pointrange()
        else         out <- out + geom_ribbon(alpha = 0.3)
    }

    if(length(unique(vbdiff[["comp"]])) > 1)
        out <- out + facet_wrap("comp")

    return(out)
}

#' @rdname plot_vbdiff
#'
#' @export plot.vbdiff
#' @export

plot.vbdiff <- plot_vbdiff
