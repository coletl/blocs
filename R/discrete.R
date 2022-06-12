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
#'   columns named by \code{dv_turnout}, \code{dv_voterep}, \code{dv_votedem},
#'   \code{indep}, and \code{weight}.
#' @param indep      string, column name of the independent variable defining
#'   discrete voting blocs.
#' @param dv_turnout     string, column name of the dependent variable flagging
#'   voter turnout. That column must be coded {0, 1}.
#' @param dv_voterep     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Republican vote choice.
#' @param dv_votedem     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Democratic vote choice.
#' @param weight     optional string naming the column of sample weights.
#' @param boot_iters integer, number of bootstrap iterations for uncertainty
#'   estimation. The default `NULL` is equivalent to 0 and does not estimate
#'   uncertainty.
#' @param verbose        logical, whether to print iteration number.
#' @param check_discrete logical, whether to check if \code{indep} is a discrete variable.
#'
#' @return A \code{vbdf} object.
#' @importFrom dplyr %>%
#' @export


vb_discrete <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv_turnout, dv_voterep, dv_votedem,
             weight = NULL, boot_iters = FALSE,
             verbose = FALSE, check_discrete = TRUE){

        if(is_grouped_df(data_density))
            stop("Density estimation does not permit grouped data frames. Please use split-apply-combine.")

        if( check_discrete & dplyr::n_distinct(dplyr::select(ungroup(data_density), dplyr::all_of(indep))) > 50)
            stop("More than 25 unique values detected in indep. If you are sure you don't want vb_continuous(), set check_discrete = FALSE.")


        stopifnot(rlang::has_name(data_density, indep))
        stopifnot(rlang::has_name(data_density, weight))

        stopifnot(rlang::has_name(data_turnout, dv_turnout))
        stopifnot(rlang::has_name(data_turnout, indep))
        stopifnot(rlang::has_name(data_turnout, weight))

        stopifnot(rlang::has_name(data_vote, dv_turnout))
        stopifnot(rlang::has_name(data_vote, c(dv_voterep , dv_votedem)))
        stopifnot(rlang::has_name(data_vote, indep))
        stopifnot(rlang::has_name(data_vote, weight))

        # Remove missing values like vb_continuous()
        data_density <- stats::na.omit(dplyr::select(data_density, dplyr::all_of(c(dplyr::group_vars(data_density), indep, weight))))

        # Start with NULL weights, but grab the col if present
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


        # Force independent variables to be discrete
        data_density <-
            data_density %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))

        data_turnout <-
            data_turnout %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))

        data_vote <-
            data_vote %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))



        if(boot_iters == 0){

            # Estimate Pr(X) ----
            grp_tbl <-
                wtd_table(dplyr::select(dplyr::ungroup(data_density), dplyr::all_of(indep)),
                          weight = weight_density,
                          prop = TRUE, return_tibble = TRUE)
            names(grp_tbl)[1:length(indep)] <- indep

            if(any(grp_tbl$prop < 0.005))
                warning("Resampling resulted in an extremely small proportion.\n Regression may fail for absent factor levels.")

            # Estimate Pr(turnout | X) ----
            indep_str <- paste(indep, collapse = " * ")

            form_turnout <- stats::as.formula(sprintf("%s ~ %s", dv_turnout, indep_str))

            lm_turnout  <-
                stats::lm(form_turnout,
                          data = data_turnout, weight = weight_turnout)

            # Estimate Pr(vote | turnout, X) ----
            voter_ind <- which(data_vote[[dv_turnout]] == 1)

            # vote = Rep
            form_voterep <- stats::as.formula(sprintf("%s ~ %s", dv_voterep, indep_str))

            # vote = Dem
            form_votedem <- stats::as.formula(sprintf("%s ~ %s", dv_votedem, indep_str))


            lm_voterep <-
                tryCatch(
                    stats::lm(form_voterep,
                              data = data_vote[voter_ind, ],
                              weight = weight_vote[voter_ind]),
                    error = function(e) NA_integer_
                    )

            lm_votedem <-
                tryCatch(
                    stats::lm(form_votedem,
                              data = data_vote[voter_ind, ],
                              weight = weight_vote[voter_ind]),
                    error = function(e) NA_integer_
                    )

            # Predict ----
            # Predict turnout, vote choice on same X values as density estimation

            results <-
                grp_tbl %>%
                dplyr::mutate(
                    prob = prop,
                    prop = NULL,
                    pr_turnout = stats::predict(lm_turnout, newdata = .),
                    pr_voterep = stats::predict(lm_voterep, newdata = .),
                    pr_votedem = stats::predict(lm_votedem, newdata = .),

                    net_rep = (pr_voterep - pr_votedem) * pr_turnout * prob
                )

            out <- vbdf(data = results,
                        bloc_var = indep,
                        var_type  = "discrete")

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
                    vb_discrete(data_density = boot_density,
                                data_turnout = boot_turnout,
                                data_vote    = boot_vote,

                                indep = indep, dv_turnout = dv_turnout,
                                dv_voterep = dv_voterep, dv_votedem = dv_votedem,
                                weight = NULL,
                                boot_iters = FALSE)

                boot_results[[itnm]] <- boot_out

                if(verbose) cat("Completed resample", itnm, "\n")

            }

            results <- bind_rows(boot_results, .id = "resample")

            out <-
                vbdf(results,
                     bloc_var = get_bloc_var(results),
                     var_type = get_var_type(results)
                )
        }

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

