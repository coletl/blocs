# vbdf class ----

get_bloc_var <- function(x) attr(x, "bloc_var")
get_var_type <- function(x) attr(x, "var_type")

#' Constructor for class vbdf
#'
#' @param x a data.frame
#' @param bloc_var character vector naming the variables to define voting blocs
#' @param var_type string, the type, discrete or continuous
#'
new_vbdf <- function(x, bloc_var = character(),
                     var_type = c("discrete", "continuous")){

    stopifnot(is.data.frame(x))
    # tibble protects attributes from dplyr verbs
    out <-
        tibble::new_tibble(x, nrow = nrow(x), subclass = "vbdf",
                          bloc_var = bloc_var, var_type = var_type)

    tibble::validate_tibble(out)

    return(out)
}

#' Validator for class vbdf
#'
#' @param x
#'
#' @import dplyr

check_vbdf <- function(x){

    require(dplyr)

    stopifnot(is.data.frame(x))
    stopifnot("bloc_var" %in% names(attributes((x))))
    stopifnot("var_type" %in% names(attributes((x))))
    stopifnot(attr(x, "var_type") %in% c("discrete", "continuous"))

    if(get_var_type(x) == "discrete") {
        # stopifnot(all(rlang::has_name(x, c("results", "covariate", "var_type"))))
        #
        # stopifnot(!anyNA( x$point_est ))
        # var_check <- setdiff(x$variable, "diff_demrep")

        # for(var in var_check) {
        #
        #     stopifnot(all(dplyr::filter(x$results, variable == var)$point_est >= 0))
        #     stopifnot(all(dplyr::filter(x$results, variable == var)$point_est <= 1))
        #
        # }
        #
        # stopifnot(all(dplyr::filter(x$results, variable == "diff_demrep")$point_est >= -1))
        # stopifnot(all(dplyr::filter(x$results, variable == "diff_demrep")$point_est <=  1))

        stopifnot(all(rlang::has_name(x, c(get_bloc_var(x),
                                           "net_rep", "cond_rep", "prob"))))

    }

    if(get_var_type(x) == "continuous") {
        stopifnot(all(rlang::has_name(x, c(get_bloc_var(x)))))
        stopifnot(!anyNA( x$net_rep ))
    }

    stopifnot(
        dplyr::summarize(x,
                         across(matches("net_rep"),
                                ~ all(dplyr::between(.x, -1, 1)))) %>%
            all()
    )

    stopifnot(
        dplyr::summarize(x,
                         across(matches("prob"),
                                ~ all(dplyr::between(.x, 0, 1)))) %>%
            all()
    )

    return(TRUE)
}

#' Create a vbdf object
#'
#' Create a vbdf object holding bloc-level estimates of composition, turnout,
#' and/or vote choice. This function is mostly for internal use, but you may
#' want it to create a \code{vbdf} object from your custom voting bloc analysis.
#' A valid \code{vbdf} object can be used in \link{vb_diff}, \link{plot_vbdf},
#' and \link{vb_swap}.
#'
#' @param data data.frame of voting-bloc results to convert to a \code{vbdf} object
#' @param bloc_var string, the name of the variable that defines the voting blocs
#' @param var_type string, the type of varialbe, discrete or continuous
#'
#' @return A \code{vbdf} object.
#'
#' @export

vbdf <-
    function(data, bloc_var, var_type = c("discrete", "continuous")){

        var_type <- match.arg(var_type)

        vbdf <-
            new_vbdf(
                x = data,
                bloc_var = bloc_var,
                var_type = var_type
            )

        check_vbdf(vbdf)

        return(vbdf)
    }

# vbdf methods ----

#' Row-bind \code{vbdf} objects
#'
#' Row-bind \code{vbdf} objects and bring along their attributes.
#'
#' @param ... \code{vbdf} objects or a list of them to row bind.
#' @param .id string for ID column name, passed to \link[dplyr]{bind_rows}.
#'
#' @return A \code{vbdf} object.
#'
#' @export
vb_rbind <- function(..., .id = NULL){
    require(dplyr)

    dots <- list(...)[[1]]

    test <- dots[[1]]

    for(vbdf in dots) stopifnot(identical(names(vbdf)        , names(test)))
    for(vbdf in dots) stopifnot(identical(get_bloc_var(vbdf), get_bloc_var(test)))
    for(vbdf in dots) stopifnot(identical(get_var_type(vbdf) , get_var_type(test)))
    for(vbdf in dots) stopifnot(identical(class(vbdf)        , class(test)))


    bound_df <- dplyr::bind_rows(dots, .id = .id)

    out <-
        vbdf(data = bound_df,
             bloc_var = get_bloc_var(vbdf),
             var_type = get_var_type(vbdf))

    return(out)
}

#' @export rbind.vbdf
#' @export

rbind.vbdf <- vb_rbind

#' #' @export group_by.vbdf
#' #' @export
#'
#' group_by.vbdf <- function(.data, ..., .add = FALSE,
#'                           .drop = group_by_drop_default(.data)){
#'
#'     bloc_var <- get_bloc_var(.data)
#'     var_type <- get_var_type(.data)
#'
#'     vbdf_grp <- dplyr:::group_by.data.frame(.data, ..., .add = .add, .drop = .drop)
#'
#'
#'     out <- vbdf(vbdf_grp,
#'                 bloc_var = bloc_var, var_type = var_type)
#'
#'     return(out)
#' }

