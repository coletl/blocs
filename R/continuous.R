#' Continuous voting bloc analysis
#'
#' Define voting blocs along a \strong{continuous} variable and estimate their
#' partisan vote contributions.
#'
#' @param data           default data.frame to use as the source for density,
#'   turnout, and vote choice data.
#' @param data_density   data.frame of blocs' composition/density data. Must
#'   include any columns named by \code{indep} and \code{weight}.
#' @param data_turnout   data.frame of blocs' turnout data. Must include any
#'   columns named by \code{dv_turnout}, \code{indep} and \code{weight}.
#' @param data_vote      data.frame of blocs' vote choice data. Must include any
#'   columns named by \code{dv_turnout}, \code{dv_voterep}, \code{dv_votedem},
#'   \code{indep}, and \code{weight}.
#' @param indep      column names of the independent variable(s) defining
#'   continuous voting blocs.
#' @param dv3        string, column name of the dependent variable coded as
#'   follows: -1 for Republican vote choice, 0 for no or third-party vote, 1 for
#'   Democratic vote choice
#' @param dv_turnout     string, column name of the dependent variable flagging
#'   voter turnout. That column must be coded {0, 1}.
#' @param dv_voterep     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Republican vote
#'   choice.
#' @param dv_votedem     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Democratic vote
#'   choice.
#' @param weight     optional string naming the column of sample weights. Must
#'   be identical in all data sets.
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
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep,
             dv3 = NULL,
             dv_turnout = NULL, dv_voterep = NULL, dv_votedem = NULL,
             weight = NULL, min_val = NULL, max_val = NULL, n_points = 100,
             boot_iters = FALSE, verbose = FALSE){

        require(dplyr)

        if(is_grouped_df(data_density))
            stop("Density estimation does not permit grouped data frames. Please use split-apply-combine.")

        stopifnot(rlang::has_name(data_density, indep))
        stopifnot(rlang::has_name(data_density, weight))

        stopifnot(!is.null(dv3) | length(c(dv_turnout, dv_voterep, dv_votedem)) == 3)

        if(missing(dv3)) stopifnot(rlang::has_name(data_turnout, dv_turnout))
        stopifnot(rlang::has_name(data_turnout, indep))
        stopifnot(rlang::has_name(data_turnout, weight))

        if(missing(dv3)) stopifnot(rlang::has_name(data_vote, dv_turnout))
        if(missing(dv3)) stopifnot(rlang::has_name(data_vote, c(dv_voterep , dv_votedem)))
        stopifnot(rlang::has_name(data_vote, indep))
        stopifnot(rlang::has_name(data_vote, weight))

        # No missing values allowed in kde
        data_density <- stats::na.omit(select(data_density, all_of(c(indep, weight))))

        if(is.null(min_val)) min_val <- mapply(min, select(ungroup(data_density), all_of(c(indep))))
        if(is.null(max_val)) max_val <- mapply(max, select(ungroup(data_density), all_of(c(indep))))

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

            ### Estimate Pr(rep | X)
            indep_str <-
                sprintf("s(%s)", indep) %>%
                paste(collapse = " * ")

            form_dv3 <- stats::as.formula(sprintf("%s ~ %s", dv3, indep_str))
            gam_dv3  <- mgcv::gam(form_dv3, data = data_vote)

            ### Predict
            # Predict turnout, vote choice on same X values as density estimation
            ert <- as.data.frame(dens_estim$x_seq)
            names(ert) <- indep

            results <-
                data.frame(as.data.frame(dens_estim$x_seq),
                           prob = dens_estim$density,
                           cond_rep = predict(gam_dv3, as.data.frame(dens_estim$x_seq))) %>%
                mutate(net_rep = cond_rep * prob)

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
                                  dv3 = dv3,
                                  dv_turnout = dv_turnout,
                                  dv_voterep = dv_voterep, dv_votedem = dv_votedem,
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
    colnames(stage$eval.points) <- stage$names

    out <- list(density = probs, x_seq = stage$eval.points)

    return(out)
}


#' Weighted quantiles ' ' This function calls \link[collapse]{fnth} repeatedly
#' over a vector of probabilities to produce output like \link[stats]{quantile}.
#' Fast, with minimal dependencies, but does not accept negative weights.

#' @param x      numeric vector.
#' @param probs  numeric vector of probabilities.
#' @param weight numeric vector of non-negative weights.
#' @param na.rm  logical whether to remove missing values
#' @param ...    further arguments passed to \link[collapse]{fnth}.
#'
#' @export
#'
#'

wtd_quantile <- function(x, probs = seq(0, 1, 0.25), weight, na.rm = FALSE, ...){

    if(any(weight < 0)) stop("collapse::fnth does not support negative weights.")
    probs_tags <- paste0(probs * 100, "%")

    # collapse::fnth doesn't allow 0 or 1 probabilities
    if(0 %in% probs) probs[probs == 0] <- .Machine$double.xmin
    if(1 %in% probs) probs[probs == 1] <- 1 - 1e-7


    out <-
        vapply(X = probs, FUN.VALUE = double(1),
               FUN = function(p) collapse::fnth(x, w = weight, n = p,
                                                na.rm = na.rm, ...))
    names(out) <- probs_tags

    return(out)

}

