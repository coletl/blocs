#' Discrete voting bloc analysis
#'
#' Define voting blocs along a \strong{discrete} variable and estimate their partisan
#' vote contributions.
#'
#' @param data               default data.frame to use as the source for
#'   density, turnout, and vote choice data.
#' @param data_density   data.frame of blocs' composition/density data. Must
#'   include any columns named by \code{indep} and \code{weight}.
#' @param data_turnout   data.frame of blocs' turnout data. Must include any
#'   columns named by \code{dv_turnout}, \code{indep} and
#'   \code{weight}.
#' @param data_vote      data.frame of blocs' vote choice data. Must include any
#'   columns named by \code{dv_vote3}, \code{indep}, and \code{weight}.
#' @param indep      string, column name of the independent variable defining
#'   discrete voting blocs.
#' @param dv_vote3        string, column name of the dependent variable in \code{data_vote}, coded as
#'   follows: -1 for Democrat vote choice, 0 for no or third-party vote, 1 for
#'   Republican vote choice.
#' @param dv_turnout     string, column name of the dependent variable flagging
#'   voter turnout in \code{data_turnout}. That column must be coded 0 =  no vote, 1 = voted.
#' @param weight     optional string naming the column of sample weights.
#' @param boot_iters integer, number of bootstrap iterations for uncertainty
#'   estimation. The default \code{FALSE} is equivalent to 0 and does not estimate
#'   uncertainty.
#' @param verbose        logical, whether to print iteration number.
#' @param check_discrete logical, whether to check if \code{indep} is a discrete variable.
#'
#' @return A \code{vbdf} object.
#' @importFrom dplyr %>%
#'
#' @export


vb_discrete <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv_vote3, dv_turnout,
             weight = NULL, boot_iters = FALSE,
             verbose = FALSE, check_discrete = TRUE){

        if(dplyr::is_grouped_df(data_density)){
            stop("Voting blocs analysis does not permit grouped data frames.\n Please use split-apply-combine to analyze multiple years, or pass multiple column names to the `indep` parameter for multivariate blocs.")
        }

        stopifnot(is.data.frame(data_density))
        stopifnot(is.data.frame(data_turnout))
        stopifnot(is.data.frame(data_vote))

        if(!all(rlang::has_name(data_density, indep)))
            stop(sprintf("%s not found in data_density", indep))

        if(!all(rlang::has_name(data_turnout, indep)))
            stop(sprintf("%s not found in data_turnout\n", indep))
        if(!rlang::has_name(data_turnout, dv_turnout))
            stop(sprintf("%s not found in data_turnout", dv_turnout))

        if(!all(rlang::has_name(data_vote, indep)))
            stop(sprintf("%s not found in data_vote\n", indep))
        if(!rlang::has_name(data_vote, dv_vote3))
            stop(sprintf("%s not found in data_vote", dv_vote3))

        if( check_discrete & dplyr::n_distinct(collapse::get_vars(data_density, indep)) > 50)
            stop("More than 25 unique values detected in indep. \nIf you are sure you don't want vb_continuous(), set check_discrete = FALSE.")

        # Start with NULL weights = 1, but grab the col if present
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

            # Check for negative weights
            if(
                any(
                    weight_density <= 0,
                    weight_turnout <= 0,
                    weight_vote <= 0
                )
            ) stop("Weights must be greater than zero.")

        }

        # Force independent variables to be discrete
        data_density <-
            data_density %>%
            collapse::ftransformv(vars = indep,
                                  FUN  =  function(x)
                                      if(is.factor(x)) x else collapse::qF(x))

        data_turnout <-
            data_turnout %>%
            collapse::ftransformv(vars = indep,
                                  FUN  =  function(x)
                                      if(is.factor(x)) x else collapse::qF(x))

        data_vote <-
            data_vote %>%
            collapse::ftransformv(vars = indep,
                                  FUN  =  function(x)
                                      if(is.factor(x)) x else collapse::qF(x))

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

        results_base <-
            collapse::get_vars(data_density, indep) %>%
            collapse::funique()

        results_list <- list()

        # Probability mass calculation ----
        # Create matrix of data-row indices for each iteration
        # For iters = 0, returns the original row indices
        itermat_density <-
            boot_mat(nrow(data_density), iters = boot_iters_density,
                     weight = weight_density)

        # Remove weights when using resampled data
        if(boot_iters_density > 0) weight_density <- NULL

        results_list$prob <-
            apply(itermat_density, 2,
                  FUN = function(ind)

                      dplyr::slice(data_density, ind) %>%
                      dplyr::ungroup() %>%
                      collapse::get_vars(indep) %>%

                      wtd_table(weight = weight_density,
                                prop = TRUE, return_tibble = TRUE) %>%

                      dplyr::rename(prob = prop)
            ) %>%
            dplyr::bind_rows(.id = "resample")

        # Turnout calculation ----
        itermat_turnout <-
            boot_mat(nrow(data_turnout), iters = boot_iters_turnout,
                     weight = weight_turnout)

        if(boot_iters_turnout > 0) weight_turnout <- NULL

        results_list$turnout <-
            apply(itermat_turnout, 2,
                  FUN = function(ind)

                      calc_turnout(dplyr::slice(data_turnout, ind),
                                   indep = indep,
                                   dv = dv_turnout, weight = weight_turnout)

            ) %>%
            dplyr::bind_rows(.id = "resample")

        # Vote choice calculation ----
        itermat_vote <-
            boot_mat(nrow(data_vote), iters = boot_iters_vote,
                     weight = weight_vote)

        if(boot_iters_vote > 0) weight_vote <- NULL

        results_list$vote <-
            apply(itermat_vote, 2,
                  FUN = function(ind)
                      calc_vote(dplyr::slice(data_vote, ind),
                                indep = indep,
                                dv = dv_vote3, weight = weight_vote)
            ) %>%
            dplyr::bind_rows(.id = "resample")


        results <-
            dplyr::full_join(results_base, results_list$prob, by = indep) %>%
            dplyr::full_join(results_list$turnout,
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
        results <-
            collapse::fmutate(results,
                              resample = gsub("-0+", "-", resample),
                              net_rep = prob * pr_turnout * cond_rep) %>%
            collapse::colorderv(neworder = c("resample", indep,
                                             "prob", "pr_turnout",
                                             "pr_voterep", "pr_votedem",
                                             "cond_rep", "net_rep")) %>%
            collapse::roworderv(cols = c("resample", indep))


        out <-
            vbdf(results,
                 bloc_var = indep, var_type = "discrete")
        return(out)
    }


#' Weighted frequency table or proportions
#'
#' @param ...     vectors of class factor or character, or a list/data.frame of such vectors.
#' @param weight  optional vector of weights. The default uses uniform weights of 1.
#' @param na.rm   logical, whether to remove NA values.
#' @param prop    logical, whether to return proportions or counts. Default returns counts.
#' @param return_tibble    logical, whether to return a tibble or named vector.
#' @param normwt           logical, whether to normalize weights such that they sum to 1.
#'
#' @return a vector or tibble of counts or proportions by group
#'
#' @export

wtd_table <-
    function(...,
             weight = NULL, na.rm = FALSE,
             prop = FALSE, return_tibble = FALSE,
             normwt = FALSE){

        # Factor/character check
        if(!all( sapply(list(...), is.factor)    |
                 sapply(list(...), is.character) |
                 sapply(list(...), is.list)
        )
        ) stop("All vector inputs must be factor or character. All subsequent arguments must be fully named.")


        if(!is.null(weight)) stopifnot(is.numeric(weight))

        tabdf <- data.frame(...)
        if(normwt) weight <- weight * nrow(tabdf)/sum(weight)

        # Use weights if present, otherwise all 1
        weight_vec <- if(is.null(weight)) rep.int(1L, nrow(tabdf)) else weight

        if(na.rm){
            # Remove values where any ... is NA
            tabdf <- stats::na.omit(tabdf)
            # Remove corresponding weights
            na_ind <- unique(attr(tabdf, "na.action"))
            weight_vec <- weight_vec[- na_ind]
        }

        # Sum weights within group
        grps <- collapse::GRP(tabdf)
        out  <- collapse::fsum(weight_vec, grps)

        if(prop) out <- out / sum(out)

        if(return_tibble){
            out <- tibble::tibble(grps$groups, count = unname(out))

            if(prop) names(out)[names(out) == "count"] <- "prop"
        }

        return(out)
    }

calc_turnout <- function(data, indep, dv, weight){

    cgdf <-
        collapse::get_vars(

            collapse::fgroup_by(data, indep),

            dv
        )

    results <- collapse::fmean(cgdf, w = weight)
    out <- dplyr::rename(results, pr_turnout = {{dv}})

    return(out)
}

calc_vote <- function(data, indep, dv, weight){

    cgdf <-
        collapse::get_vars(

            collapse::fgroup_by(

                collapse::ftransform(
                    data,
                    pr_voterep = as.numeric(get({{dv}}) ==  1),
                    pr_votedem = as.numeric(get({{dv}}) == -1)
                ),
                indep),

            c("pr_voterep", "pr_votedem")
        )

    out <- collapse::fmean(cgdf, w = weight)
    out$cond_rep <- out$pr_voterep - out$pr_votedem

    return(out)
}
