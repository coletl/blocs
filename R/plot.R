#' Plot voting blocs' year-to-year difference
#'
#'
#' @rdname plot_vbdiff
#' @param vbdiff a data.frame of year-to-year differences in the format of the output from \link{vb_diff}.
#' @param x      string naming the column to plot on the x axis.
#' @param y      string naming the column to plot on the y axis.
#' @param ci_low  string naming the column to plot as the lower bound of the confidence interval.
#' @param ci_high string naming the column to plot as the upper bound of the confidence interval.
#'
#' @import ggplot2
#'
#' @export plot_vbdiff
#' @export
#'
plot_vbdiff <-
    function(vbdiff, x = get_bloc_var(vbdiff), y, ymin, ymax,
             discrete = length(unique(vbdiff[[x]])) < 20
             ){

    require(ggplot2)

    ci <- !missing(ymin) & !missing(ymax)

    if(length(x) > 1) stop("Choose one independent variable to plot.")

    out <-
        ggplot(vbdiff) +
        aes(x = .data[[x]], y = .data[[y]]) +
        geom_hline(yintercept = 0) +
        theme_bw()

    out <- if(discrete) out + geom_point() else out + geom_line()

    if(ci) {
        out <- out + aes(ymin = .data[[ymin]], ymax = .data[[ymax]])
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
