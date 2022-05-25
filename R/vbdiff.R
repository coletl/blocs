#' vbdiff class and methods

#' Constructor for class vbdiff
#'
#' @param x a data.frame of voting-bloc differences
#' @inheritParams vbdf
#' @inheritParams vb_difference

new_vbdiff <- function(x, bloc_var, var_type, diff_col){
    stopifnot(is.data.frame(x))

    out <- tibble::new_tibble(x, nrow = nrow(x), subclass = "vbdiff",
                              bloc_var = bloc_var, var_type = var_type,
                              diff_col = diff_col)
    tibble::validate_tibble(out)

    return(out)
}


get_diff_col <- function(x) attr(x, "diff_col")

