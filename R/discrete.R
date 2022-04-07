#' Continuous voting bloc analysis
#'
#' Define voting blocs along a \strong{discrete} variable and estimate their partisan
#' vote contributions.
#'
#' @param data       data.frame.
#' @param dv         string, column name of the dependent variable for analysis.
#'   Must be coded {-1, 0, 1} for Democrat, no vote/third party, and Republican
#'   vote.
#' @param indep      string, column name of the independent variable defining
#'   continuous voting blocs.
#' @param cov        character vector,  column names of any covariates.
#' @param weight     string, column name of the sample weights.
#' @param boot_iters integer, number of bootstrap iterations for uncertainty
#'   estimation. The default `NULL` is equivalent to 0 and does not estimate
#'   uncertainty.
#' @param verbose        logical, whether to print iteration number.
#' @param check_discrere logical, whether to check if \code{indep} is a discrete variable.
#'
#' @return A \code{vbdf} object.
#'
#' @import dplyr
#' @export


vb_discrete <-
    function(data, dv, indep, cov = NULL,
             weight = NULL, boot_iters = FALSE,
             verbose = FALSE, check_discrete = TRUE){

        # Grab columns with purrr::pluck since dplyr::pull(NULL) errors out
        require(dplyr)

        stopifnot(rlang::has_name(data, dv))
        stopifnot(rlang::has_name(data, indep))

        stopifnot(rlang::has_name(data, weight))

        # To return NULL instead of error in
        if(dplyr::n_distinct(purrr::pluck(data, indep)) > 25 && check_discrete)
            stop("More than 25 unique values detected in indep. If you are sure you don't want vb_continuous(), set check_discrete = FALSE.")
        if(!is.null(cov))
            if(dplyr::n_distinct(purrr::pluck(data, cov))   > 25 && check_discrete)
                stop("More than 25 unique values detected in cov. If you are sure you don't want vb_continuous(), set check_discrete = FALSE.")

        if(
            anyNA(
                dplyr::select(data, all_of(c(group_vars(data), indep, cov)))
            )
        ){
            warning("Dropping rows with NA values in indep or cov")
            data <- stats::na.omit(select(data, all_of(c(group_vars(data), dv, indep, cov, weight))))
        }

        if(is.null(weight)) weight_vec <- 1L
        else weight_vec <- data[[weight]]

        grp_tbl <-
            grp_wtd_table(data, grp_vars = c(indep, cov),
                          weight = weight_vec)


        if(boot_iters == 0){

            cov_str <- ifelse(is.null(cov),
                              "",
                              sprintf(" + factor(%s)", cov))

            form <- stats::as.formula(sprintf("%s ~ factor(%s) - 1%s", dv, indep, cov_str))
            mod  <- stats::lm(form, data = data,
                              weight = weight_vec)

            results <-
                mutate(ungroup(grp_tbl),
                       # Pr(Rep | x)
                       cond_rep = stats::predict(mod, newdata = grp_tbl),
                       net_rep  = cond_rep * prob
                )

            out <- vbdf(data = results,
                        bloc_var = c(indep, cov),
                        var_type  = "discrete")

        } else {
            # TODO: Abstract to boot_start() with vb_continuous
            # Create matrix of rows for each iteration
            # Accept a column name (string) or vector of weights
            itermat <-
                replicate(boot_iters,
                          sample.int(nrow(data), replace = TRUE,
                                     prob = weight_vec)
                )

            # Add the original sample to the front
            itermat <- cbind(1:nrow(data), itermat)
            colnames(itermat) <- c("resample-0",
                                   sprintf("resample-%s", 1:boot_iters))

            # Run bootstrap
            boot_results <- list()

            for(itnm in colnames(itermat)){
                boot_data <- data[itermat[ , itnm], ]

                boot_out <-
                    vb_discrete(data = boot_data,
                                dv = dv, indep = indep, cov = cov,
                                weight = weight,
                                boot_iters = FALSE)

                boot_results[[itnm]] <- boot_out

                if(verbose) cat("Completed resample", itnm, "\n")

            }

            results <- vb_rbind(boot_results, .id = "resample")

            out <-
                vbdf(results,
                     bloc_var = get_bloc_var(results),
                     var_type = get_var_type(results)
                )
        }

        return(out)
    }

#' Weighted frequency table by group
#'
#' @param data     data.frame
#' @param grp_vars character vector naming columns to group by
#' @param weight   string naming the column of weights
#'
#' @import dplyr
#'
#' @export
grp_wtd_table <- function(data, grp_vars, weight = NULL){

    require(dplyr)

    stopifnot(is.data.frame(data))
    stopifnot(rlang::has_name(data, grp_vars))

    # Sum weights within var and year
    if(is.null(weight)) data$weight <- 1

    out <-
        group_by(data, across(all_of(grp_vars))) %>%
        summarize(prob = sum(weight)/sum(data$weight),
                  n    = n())


    if(is.null(weight)) data$weight <- NULL

    return(out)
}

