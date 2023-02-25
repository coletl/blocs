#' Continuous voting bloc analysis
#'
#' Define voting blocs along a \strong{continuous} variable and estimate their
#' partisan vote contributions.
#'
#' @inherit vb_discrete
#'
#' @param min_val    numeric vector of the same length as \code{indep}, Lower bound for the density estimation of each respective \code{indep}. See
#'   [estimate_density].
#' @param max_val    numeric vector of the same length as \code{indep}, Upper bound for the density estimation of each respective \code{indep}. See
#'   [estimate_density].
#' @param n_points   scalar, number of points at which to estimate density. See
#'   [estimate_density].
#' @param tolerance tolerance used when checking range of probability estimates
#' @param ...        further arguments to pass to \link[ks]{kde} for density estimation.
#'
#' @return a \code{vbdf} data.frame with columns for the resample, bloc variable,
#' and, for each resample-bloc combination, four estimates:
#' probability density, turnout, Republican vote choice conditional on turnout,
#' and net Republican votes.
#'
#' @export

vb_continuous <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv_vote3, dv_turnout,
             weight = NULL, min_val = NULL, max_val = NULL, n_points = 100,
             boot_iters = FALSE, verbose = FALSE, tolerance = sqrt(.Machine$double.eps),
             ...){

        if(dplyr::is_grouped_df(data_density)){
            stop("Density estimation does not permit grouped data frames.\n
                  Please use split-apply-combine to analyze multiple years, or pass multiple column names to the `indep` parameter for multivariate blocs.")
        }

        if(length(indep) > 1)
            warning("Defining blocs along multiple continuous variables may result in highly variable estimates.")

        if(anyNA(collapse::get_vars(data_density, indep)))
            stop("Density estimation does not permit NA values.")

        if(!is.null(weight) && boot_iters == 0)
            stop("Cannot fit a weighted GAM, so proper weighting for vb_continuous analyses requires resampling.")

        stopifnot(is.data.frame(data_density))
        stopifnot(is.data.frame(data_turnout))
        stopifnot(is.data.frame(data_vote))

        if(!all(rlang::has_name(data_density, indep)))
            stop(sprintf("%s not found in data_density\n", indep))

        if(!all(rlang::has_name(data_turnout, indep)))
            stop(sprintf("%s not found in data_turnout\n", indep))
        if(!rlang::has_name(data_turnout, dv_turnout))
            stop(sprintf("%s not found in data_turnout", dv_turnout))

        if(!all(rlang::has_name(data_vote, indep)))
            stop(sprintf("%s not found in data_vote\n", indep))
        if(!rlang::has_name(data_vote, dv_vote3))
            stop(sprintf("%s not found in data_vote", dv_vote3))

        if(is.null(min_val))
            min_val <- collapse::get_vars(data_density, indep) %>%
            collapse::fmin()
        if(is.null(max_val))
            max_val <- collapse::get_vars(data_density, indep) %>%
            collapse::fmax()

        # Start with uniform weights if NULL, but grab the col if present
        weight_density <- rep(1L, nrow(data_density))
        weight_turnout <- rep(1L, nrow(data_turnout))
        weight_vote    <- rep(1L, nrow(data_vote))

        if(!is.null(weight)) {
            if(rlang::has_name(data_density, weight))
                weight_density <- data_density[[weight]]
            else stop(sprintf("%s not found in data_density", weight))

            if(rlang::has_name(data_turnout, weight))
                weight_turnout <- data_turnout[[weight]]
            else stop(sprintf("%s not found in data_turnout", weight))

            if(rlang::has_name(data_vote, weight))
                weight_vote    <- data_vote[[weight]]
            else stop(sprintf("%s not found in data_vote", weight))
        }

        # Check for negative weights
        if(
            any(
                weight_density <= 0,
                weight_turnout <= 0,
                weight_vote <= 0
            )
        ) stop("Weights must be greater than zero.")


        # Boostrap setup ----
        if(length(boot_iters) == 1){
            boot_iters_density <-
                boot_iters_turnout <-
                boot_iters_vote <- boot_iters
        } else {

            if(!all(rlang::has_name(boot_iters, c("density", "turnout", "vote"))))
                stop("If boot_iters has length greater than 1, you must name each value according to the data set:
                     'density', 'turnout', or 'vote'")

            boot_iters_density <-
                boot_iters[pmatch("density", names(boot_iters))]
            boot_iters_turnout <-
                boot_iters[pmatch("turnout", names(boot_iters))]
            boot_iters_vote    <-
                boot_iters[pmatch("vote", names(boot_iters))]
        }

        results_list <- list()

        # Probability density estimation ----
        # Create matrix of data-row indices for each iteration
        # For iters = 0, returns the original row indices
        itermat_density <-
            boot_mat(nrow(data_density), iters = boot_iters_density,
                     weight = weight_density)

        # Remove weights when using resampled data
        if(boot_iters_density > 0) weight_density <- NULL

        prob_list <-
            apply(itermat_density, 2, simplify = FALSE,
                  FUN = function(ind){
                      dens_estim <-
                          dplyr::slice(data_density, ind) %>%
                          collapse::get_vars(indep) %>%
                          as.matrix() %>%
                          estimate_density(x = .,
                                           min = min_val,
                                           max = max_val,
                                           w = weight_density,
                                           n_points = n_points,
                                           ...
                          )
                      dens_pts <-
                          names(dens_estim) %>%
                          strsplit(split = "_|_", fixed = TRUE) %>%
                          do.call(rbind, .)
                      class(dens_pts) <- "numeric"

                      dens <- data.frame(dens_pts, prob = unname(dens_estim))
                      names(dens)[1:ncol(dens_pts)] <- indep

                      return(dens)
                  }
            )
        results_list$prob <- dplyr::bind_rows(prob_list, .id = "resample")
        results_base <- prob_list[[1]] %>% collapse::fselect(indep)

        # Turnout estimation ----
        indep_str <-
            paste(indep, collapse = " * ") %>%
            sprintf("s(%s)", .)

        itermat_turnout <-
            boot_mat(nrow(data_turnout), iters = boot_iters_turnout,
                     weight = weight_turnout)

        form_turnout <-
            stats::as.formula(sprintf("%s ~ %s", dv_turnout, indep_str))

        results_list$turnout <-
            apply(itermat_turnout, 2, simplify = FALSE,
                  FUN = function(ind){

                      gam_turnout <-
                          dplyr::slice(data_turnout, ind) %>%
                          mgcv::gam(form_turnout, data = .)

                      turn <- as.vector(stats::predict(gam_turnout, newdata = results_base))
                      data.frame(results_base, pr_turnout = turn)
                  }
            ) %>% dplyr::bind_rows(.id = "resample")

        # Vote choice estimation ----
        itermat_vote <-
            boot_mat(nrow(data_vote), iters = boot_iters_vote,
                     weight = weight_vote)

        form_vote <-
            stats::as.formula(sprintf("%s ~ %s", dv_vote3, indep_str))

        results_list$vote <-
            apply(itermat_vote, 2, simplify = FALSE,
                  FUN = function(ind){
                      gam_vote <-
                          dplyr::slice(data_vote, ind) %>%
                          mgcv::gam(form_vote, data = .)

                      cond_rep <- as.vector(stats::predict(gam_vote, newdata = results_base))
                      data.frame(results_base, cond_rep)
                  }
            ) %>% dplyr::bind_rows(.id = "resample")


        results <-
            dplyr::full_join(results_list$prob, results_list$turnout,
                             by = c("resample", indep)) %>%
            dplyr::full_join(results_list$vote, by = c("resample", indep))


        # If at least one data set not resampled
        # populate missing estimates with the original-sample results
        contains_original <- "original" %in% results$resample

        if(contains_original && !all(boot_iters == 0)){
            estim_nms <- c(prob = "density", pr_turnout = "turnout",
                           net_rep = "vote choice")
            vbdf_orig <- dplyr::filter(results, resample == "original")

            data_orig <- stats::na.omit(unique(estim_nms[names(which(!sapply(vbdf_orig, function(x) all(is.na(x)))))]))
            estim_orig <- names(estim_nms[estim_nms == data_orig])

            warning(
                sprintf("No resampling performed for %s data.\n  Populating %s estimates assuming zero uncertainty.",
                        paste(data_orig, collapse = ", "),
                        paste(estim_orig, collapse = ", "))
            )

            # Merge original-sample estimates into resamples
            vbdf_orig <-
                dplyr::select(vbdf_orig,
                              dplyr::all_of(c(indep, estim_orig)))

            results <-
                dplyr::filter(results, resample != "original") %>%
                dplyr::select(-dplyr::all_of(estim_orig)) %>%
                dplyr::left_join(vbdf_orig, by = indep)
        }

        # Calculate net Republican votes
        # Not using pr_turnout because cond_rep comes from the dv_vote3 regression
        results <-
            collapse::fmutate(results,
                              resample = gsub("-0+", "-", resample),
                              net_rep = prob * cond_rep) %>%
            collapse::colorderv(neworder = c("resample", indep,
                                             "prob", "pr_turnout",
                                             "cond_rep", "net_rep")) %>%
            collapse::roworderv(cols = c("resample", indep))

        out <-
            vbdf(results, tolerance = tolerance,
                 bloc_var = indep, var_type  = "continuous")

        return(out)
    }

#' Estimate density
#'
#' Run \link[ks]{kde} for weighted density estimation
#' of a  \code{x} at \code{n_points}
#' evenly spaced points between \code{min} and {max}.
#'
#' @param x        numeric vector or matrix
#' @param min      numeric vector giving the lower bound of evaluation points for each variable in \code{x}
#' @param max      numeric vector giving the upper bound of evaluation points for each variable in \code{x}
#' @param n_points number of evaluation points (estimates)
#' @param w        vector of weights. Default uses uniform weighting.
#' @param ...      further arguments to pass to \link[ks]{kde}

estimate_density <- function(x, min, max, n_points = 100, w = NULL, ...){

    if(length(n_points) != 1) stop("Must estimate density at the same number of points for all variables.")
    if(anyNA(x)) stop("Density estimation does not permit NA values.")

    if(is.null(w)){
        if(is.matrix(x)) w <- rep(1, nrow(x))
        if(is.vector(x)) w <- rep(1, length(x))
    }

    if(is.matrix(x)) stopifnot(identical(ncol(x), length(min)))
    if(is.matrix(x)) stopifnot(identical(ncol(x), length(max)))
    if(is.matrix(x)) stopifnot(nrow(x) == length(w))

    if(is.vector(x)) stopifnot(length(min) == 1)
    if(is.vector(x)) stopifnot(length(max) == 1)
    if(is.vector(x)) stopifnot(length(x) == length(w))

    pred_seq <-
        mapply(lb = min, ub = max,
               FUN = function(lb, ub)
                   seq(from = lb, to = ub, length.out = n_points)
        )

    # check same number of variables and eval-point vectors
    stopifnot(ncol(pred_seq) == ncol(x))

    stage <- ks::kde(x, eval.points = pred_seq, w = w, ...)

    probs <- stage$estimate / sum(stage$estimate)
    names(probs) <- do.call(paste, c(as.data.frame(stage$eval.points), sep = "_|_"))

    return(probs)
}


