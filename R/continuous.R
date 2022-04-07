#' Continuous voting bloc analysis
#'
#' Define voting blocs along a \strong{continuous} variable and estimate their partisan
#' vote contributions.
#'
#' @param data       data.frame
#' @param dv         string, column name of the dependent variable for analysis.
#'   Must be coded {-1, 0, 1} for Democrat, no vote/third party, and Republican
#'   vote.
#' @param indep      string, column name of the independent variable defining
#'   continuous voting blocs.
#' @param cov        character vector,  column names of any covariates for the
#'   GAM model. These are not included in density estimation.
#' @param weight     string, column name of the sample weights
#' @param min_val    scalar, lower bound for density estimation. See
#'   \link{estimate_density}.
#' @param max_val    scalar, upper bound for density estimation. See
#'   \link{estimate_density}.
#' @param n_points   scalar, number of points at which to estimate density. See
#'   \link{estimate_density}.
#' @param boot_iters integer, number of bootstrap iterations for uncertainty
#'   estimation. The default `NULL` is equivalent to 0 and does not estimate
#'   uncertainty.
#' @param verbose    logical, whether to print iteration number.
#' @param ...        further arguments to pass to \link[ks]{kde}.
#'
#' @return A \code{vbdf} object.
#'
#' @import dplyr
#' @export

vb_continuous <-
    function(data, dv, indep, cov = NULL,
             # GAM not weighted!
             weight = NULL,
             min_val = NULL, max_val = NULL, n_points = 100,
             boot_iters = FALSE, verbose = FALSE){

        require(dplyr)

        stopifnot(rlang::has_name(data, dv))
        stopifnot(rlang::has_name(data, indep))
        stopifnot(rlang::has_name(data, weight))

        if(!is.null(cov)){
            stopifnot(rlang::has_name(data, cov))
            if(
                ! all(sapply(select(data, all_of(cov)), is.numeric))
            ) stop("Only continuous variables may define voting blocs in vb_continuous().\nPlease split your data by cov and re-scale your estimates by the density of each group.")
        }

        # No missing values allowed in kde
        if(
            anyNA(
                dplyr::select(data, all_of(c(group_vars(data), indep, cov)))
            )
        ){
            warning("Dropping rows with NA values in indep or cov")
            data <- stats::na.omit(select(data, all_of(c(group_vars(data), dv, indep, cov, weight))))
        }

        # Use weights if supplied
        if(is.null(weight)) weight_vec <- 1L
        else weight_vec <- data[[weight]]

        if(is.null(min_val)) min_val <- mapply(min, select(data, all_of(c(indep, cov))))
        if(is.null(max_val)) max_val <- mapply(max, select(data, all_of(c(indep, cov))))

        # Fit GAM
        cov_str <-
            ifelse(is.null(cov), "",
                   paste(sprintf(" + s(%s)", cov), collapse = ""))

        form <- stats::as.formula(sprintf("%s ~ s(%s)%s", dv, indep, cov_str))

        # UNWEIGHTED!
        # See https://stackoverflow.com/questions/56313837/how-to-use-sample-weights-in-gam-mgcv-on-survey-data-for-logit-regression
        mod_gam <- mgcv::gam(form, data = data)
        # Could try splines package

        if(boot_iters == 0){
            dens_estim <-
                dplyr::select(data, all_of(c(indep, cov))) %>%
                as.matrix() %>%
                estimate_density(x = .,
                                 min = min_val,
                                 max = max_val,
                                 w = weight_vec,
                                 n_points = n_points
                                 )

            # Predict votes on same indep values as density estimation
            ert <- as.data.frame(dens_estim$x_seq)
            names(ert) <- c(indep, cov)

            pred_vote <- stats::predict(mod_gam, newdata = ert)

            # Pr(Rep | x)
            cond_rep <- pred_vote

            # Net Rep votes at n_points levels of indep
            net_rep <- cond_rep * dens_estim$density

            results <- data.frame(net_rep, cond_rep,
                                  prob = dens_estim$density,
                                  ert)

            out <- vbdf(results, bloc_var = c(indep, cov),
                        var_type = "continuous")
        } else {

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
                    vb_continuous(data = boot_data,
                                  dv = dv, indep = indep, cov = cov,
                                  weight = weight, min_val = min_val, max_val = max_val,
                                  boot_iters = FALSE)

                boot_results[[itnm]] <- boot_out

                if(verbose) cat("Completed resample", itnm, "\n")

            }

            # Organize output
            results <- vb_rbind(boot_results, .id = "resample")

            out <-
                vbdf(results,
                     bloc_var = get_bloc_var(results),
                     var_type  = get_var_type(results)
                     )
        }

        return(out)
    }

#' Estimate continuous density
#'
#' Run \link[ks]{kde} to estimate the
#' weighted density of \code{x} at \code{n_points}
#' evenly spaced points between \code{min} and {max}.
#'
#' @param x        numeric vector
#' @param min      scalar, lower bound of evaluation points
#' @param max      scalar, upper bound of evaluation points
#' @param n_points number of evaluation points (estimates)
#' @param w        vector of weights
#' @param ...      further arguments to pass to \link[ks]{kde}
#'

estimate_density <- function(x, min, max, n_points = 100, w, ...){
    require(ks)

    stopifnot(!anyNA(x))

    pred_seq <- mapply(seq, from = min, to = max, length.out = n_points)

    stage <- ks::kde(x, eval.points = pred_seq, w = w, ...)

    probs <- stage$estimate / sum(stage$estimate)
    out <- list(density = probs, x_seq = stage$eval.points)

    return(out)
}

