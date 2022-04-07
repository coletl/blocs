#' vbdiff class and methods

#' Constructor for class vbdiff
#'
#' @param x
#' @param bloc_var
#' @param diff_col

new_vbdiff <- function(x, bloc_var, diff_col){
    stopifnot(is.data.frame(x))

    out <- tibble::new_tibble(x, nrow = nrow(x), subclass = "vbdiff",
                              bloc_var = bloc_var, diff_col = diff_col)
    tibble::validate_tibble(out)

    return(out)
}


get_diff_col <- function(x) attr(x, "diff_col")

group_by.vbdiff <- function(.data, ..., .add = FALSE,
                            .drop = group_by_drop_default(.data)){

    bloc_var <- get_bloc_var(.data)
    diff_col <- get_diff_col(.data)

    vbdiff_grp <- dplyr:::group_by.data.frame(.data, ..., .add = .add, .drop = .drop)


    out <- new_vbdiff(vbdiff_grp,
                      bloc_var = bloc_var, diff_col = diff_col)

    return(out)
}
