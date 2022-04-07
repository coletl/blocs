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
    function(vbdiff, x = get_bloc_var(vbdiff), y, ci_low = NULL, ci_high = NULL){

    require(ggplot2)

    if(length(x) > 1) stop("Choose one independent variable to plot.")

    ggplot(vbdiff) +
        aes(x = .data[[x]], y = .data[[y]]) +
        geom_line() +
        {
            if(!is.null(ci_low) && !is.null(ci_high))
                geom_ribbon(
                    aes(ymin = .data[[ci_low]], ymax = .data[[ci_high]]),
                    alpha = 0.3
                )
        } +
        geom_hline(yintercept = 0) +
        {
            if( length(unique(vbdiff[["comp"]])) > 1 )
                facet_wrap("comp")
        } +
        theme_bw()
}

#' @rdname plot_vbdiff
#'
#' @export plot.vbdiff
#' @export

plot.vbdiff <- plot_vbdiff
