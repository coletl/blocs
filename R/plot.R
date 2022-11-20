#' Plot the summary of a voting bloc analysis
#'
#' @rdname vb_plot
#' @param data      a \code{vbsum} data.frame, the result of [vb_summary].
#' @param x_col      string naming the column that defines voting blocs.
#' @param y_col      string naming the column of point estimates.
#' @param ymin_col   string naming the column to plot as the lower bound of the confidence interval.
#' @param ymax_col   string naming the column to plot as the upper bound of the confidence interval.
#' @param discrete   logical indicating whether voting blocs are defined along a discrete (not continuous) variable.
#'
#' @return a ggplot object
#'
#' @import ggplot2
#'
#' @export vb_plot

vb_plot <-
    function(data,
             x_col = get_bloc_var(data),
             y_col, ymin_col, ymax_col,
             discrete = length(unique(data[[x_col]])) < 20
             ){

    ci <- !missing(ymin_col) & !missing(ymax_col)

    if(length(x_col) > 1) stop("Choose one independent variable to plot.")

    out <-
        ggplot(data) +
        aes(x = .data[[x_col]], y = .data[[y_col]]) +
        geom_hline(yintercept = 0) +
        theme_bw()

    out <- if(discrete) out + geom_point() else out + geom_line()

    if(ci) {
        out <- out + aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]])
        if(discrete) out <- out + geom_pointrange()
        else         out <- out + geom_ribbon(alpha = 0.3)
    }

    if(length(unique(data[["comp"]])) > 1)
        out <- out + facet_wrap("comp")

    return(out)
}
