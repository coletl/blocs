#' Continuous voting bloc analysis
#'
#' Define voting blocs along a \strong{continuous} variable and estimate their
#' partisan vote contributions.
#'
#' @inherit vb_discrete
#'
#' @param min_val    scalar, lower bound for density estimation. See
#'   \link{estimate_density}.
#' @param max_val    scalar, upper bound for density estimation. See
#'   \link{estimate_density}.
#' @param n_points   scalar, number of points at which to estimate density. See
#'   \link{estimate_density}.
#' @param ...        further arguments to pass to \link[ks]{kde} for density estimation.
#'
#' @export

vb_continuous <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv_vote3 = NULL, dv_turnout = NULL,
             weight = NULL, min_val = NULL, max_val = NULL, n_points = 100,
             boot_iters = FALSE, verbose = FALSE, ...){

        if(is_grouped_df(data_density)){
            stop("Density estimation does not permit grouped data frames.\n
                  Please use split-apply-combine to analyze multiple years, or pass multiple column names to the `indep` parameter for multivariate blocs.")
        }

        stopifnot(is.data.frame(data_density))
        stopifnot(is.data.frame(data_turnout))
        stopifnot(is.data.frame(data_vote))

        stopifnot(rlang::has_name(data_density, indep))
        stopifnot(rlang::has_name(data_density, weight))

        if(is.null(dv_vote3)) stopifnot(rlang::has_name(data_turnout, dv_turnout))
        stopifnot(rlang::has_name(data_turnout, indep))
        stopifnot(rlang::has_name(data_turnout, weight))

        stopifnot(rlang::has_name(data_vote, indep))
        stopifnot(rlang::has_name(data_vote, weight))

        # No missing values allowed in kde
        data_density <- stats::na.omit(dplyr::select(data_density, dplyr::all_of(c(indep, weight))))

        if(is.null(min_val)) min_val <- mapply(min, dplyr::select(dplyr::ungroup(data_density), dplyr::all_of(c(indep))))
        if(is.null(max_val)) max_val <- mapply(max, dplyr::select(dplyr::ungroup(data_density), dplyr::all_of(c(indep))))

        # Start with uniform weights if NULL, but grab the col if present
        weight_density <- rep(1L, nrow(data_density))
        weight_turnout <- rep(1L, nrow(data_turnout))
        weight_vote    <- rep(1L, nrow(data_vote))

        if(!is.null(weight)) {
            if(rlang::has_name(data_density, weight))
                weight_density <- data_density[[weight]]

            if(rlang::has_name(data_turnout, weight))
                weight_turnout <- data_turnout[[weight]]

            if(rlang::has_name(data_vote, weight))
                weight_vote    <- data_vote[[weight]]
        }

        # Check that weights greater than non-zero
        if(
            any(
                weight_density <= 0,
                weight_turnout <= 0,
                weight_vote <= 0
            )
        ) stop("Weights must be greater than zero.")

        if(boot_iters == 0){

            ### Estimate Pr(X)
            dens_estim <-
                dplyr::select(ungroup(data_density), all_of(c(indep))) %>%
                as.matrix() %>%
                estimate_density(x = .,
                                 min = min_val,
                                 max = max_val,
                                 w = weight_density,
                                 n_points = n_points,
                                 ...
                )

            ### Estimate Pr(rep | X) ----
            indep_str <-
                sprintf("s(%s)", indep) %>%
                paste(collapse = " * ")

            # 3-valued DV if available ----
            # UNWEIGHTED! Weighted in resampling

            # Fit turnout model
            form_turnout <- stats::as.formula(sprintf("%s ~ %s", dv_turnout, indep_str))
            gam_turnout <- mgcv::gam(form_turnout, data = data_turnout)

            form_dv_vote3 <- stats::as.formula(sprintf("%s ~ %s", dv_vote3, indep_str))
            gam_dv_vote3 <- mgcv::gam(form_dv_vote3, data = data_vote)

            ### Predict ----
            # Predict turnout, vote choice on same X values as density estimation
            ert <- as.data.frame(dens_estim$x_seq)
            names(ert) <- indep

            results <-
                data.frame(as.data.frame(dens_estim$x_seq),
                           prob = dens_estim$density,
                           pr_turnout = stats::predict(gam_turnout, newdata = ert),
                           cond_rep   = stats::predict(gam_dv_vote3, ert),
                           net_rep = cond_rep * prob
                           ) %>%
                dplyr::mutate(pr_turnout = unname(pr_turnout),
                              cond_rep = unname(cond_rep),
                              net_rep = unname(net_rep))

            out <- vbdf(results, bloc_var = indep,
                        var_type = "continuous")

        } else {

            # Create matrix of data-row indices for each iteration
            itermat_density <-
                boot_mat(nrow(data_density), iters = boot_iters,
                         weight = weight_density)

            itermat_turnout <-
                boot_mat(nrow(data_turnout), iters = boot_iters,
                         weight = weight_turnout)

            itermat_vote <-
                boot_mat(nrow(data_vote), iters = boot_iters,
                         weight = weight_vote)

            # Run bootstrap
            boot_results <- list()

            for(itnm in colnames(itermat_density)){
                boot_density <- data_density[itermat_density[ , itnm], ]
                boot_turnout <- data_turnout[itermat_turnout[ , itnm], ]
                boot_vote    <- data_vote[itermat_vote[ , itnm], ]

                boot_out <-
                    vb_continuous(data_density = boot_density,
                                  data_turnout = boot_turnout,
                                  data_vote    = boot_vote,

                                  indep = indep,
                                  dv_vote3 = dv_vote3,
                                  dv_turnout = dv_turnout,
                                  min_val = min_val, max_val = max_val,
                                  # Weighted in resampling
                                  weight = NULL, boot_iters = FALSE
                    )

                boot_results[[itnm]] <- boot_out

                if(verbose) cat("Completed resample", itnm, "\n")

            }

            # Organize output
            results <- bind_rows(boot_results, .id = "resample")

            out <-
                vbdf(results,
                     bloc_var = get_bloc_var(results),
                     var_type  = get_var_type(results)
                )
        }

        colnms <- c("pr_turnout", "pr_votedem", "pr_voterep", "cond_rep")
        out <- dplyr::select(out, any_of("resample"), {indep}, prob, any_of(colnms), net_rep)

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

estimate_density <- function(x, min, max, n_points = 100, w, ...){
    stopifnot(!anyNA(x))

    pred_seq <- mapply(seq, from = min, to = max, length.out = n_points)

    stage <- ks::kde(x, eval.points = pred_seq, w = w, ...)

    probs <- stage$estimate / sum(stage$estimate)
    colnames(stage$eval.points) <- stage$names

    out <- list(density = probs, x_seq = stage$eval.points)

    return(out)
}


#' Weighted quantiles ' ' This function calls \link[collapse]{fnth} repeatedly
#' over a vector of probabilities to produce output like \link[stats]{quantile}.
#' This function is fast, with minimal dependencies, but does not accept negative weights.
#' Importantly, this function does not call Hmisc::wtd.quantile, which fails for non-integer weights.

#' @param x      numeric vector.
#' @param probs  numeric vector of probabilities.
#' @param weight numeric vector of non-negative weights.
#' @param na.rm  logical whether to remove missing values
#' @param ...    further arguments passed to \link[collapse]{fnth}.
#'
#' @export

wtd_quantile <- function(x, probs = seq(0, 1, 0.25), weight, na.rm = FALSE, ...){

    if(any(weight < 0)) stop("collapse::fnth does not support negative weights.")
    probs_tags <- paste0(probs * 100, "%")

    # collapse::fnth doesn't allow 0 or 1 probabilities
    probs0_ind <- which(probs == 0)
    probs1_ind <- which(probs == 1)

    probs[c(probs0_ind, probs1_ind)] <- 0.5

    out <-
        vapply(X = probs, FUN.VALUE = double(1),
               FUN = function(p) collapse::fnth(x, w = weight, n = p,
                                                na.rm = na.rm, ...))
    out[probs0_ind] <- collapse::fmin(x)
    out[probs1_ind] <- collapse::fmax(x)

    names(out) <- probs_tags


    return(out)

}
