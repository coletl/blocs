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
#' @param dv3        string, column name of the dependent variable coded as
#'   follows: -1 for Democrat vote choice, 0 for no or third-party vote, 1 for
#'   Republican vote choice. Leave `NULL` when providing `dv_turnout`, `dv_voterep`, AND `dv_votedem`.
#' @param dv_turnout     string, column name of the dependent variable flagging
#'   voter turnout. That column must be coded {0, 1},
#'   with the same name in both \code{data_turnout} and \code{data_vote} data sets.
#' @param dv_voterep     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Republican vote
#'   choice. Ignored when `dv3` is not `NULL`.
#' @param dv_votedem     string, column name of the dependent variable flagging
#'   Republican vote choice.  Must be coded {0, 1} indicating Democratic vote
#'   choice. Ignored when `dv3` is not `NULL`.
#' @param weight     optional string naming the column of sample weights.
#' @param boot_iters integer, number of bootstrap iterations for uncertainty
#'   estimation. The default `NULL` is equivalent to 0 and does not estimate
#'   uncertainty.
#' @param verbose        logical, whether to print iteration number.
#' @param check_discrete logical, whether to check if \code{indep} is a discrete variable.
#'
#' @return A \code{vbdf} object.
#' @importFrom dplyr %>%
#'
#' @note Especially for small voting blocs, it's possible that some resamples
#' will not contain enough variation in voting behavior to estimate the models,
#' In these cases, please try specifying the \code{dv3} parameter,
#' which fits a single model to predict both Republican and Democrat vote choice.
#'
#' @export


vb_discrete <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv3 = NULL,
             dv_turnout = NULL, dv_voterep = NULL, dv_votedem = NULL,
             weight = NULL, boot_iters = FALSE,
             verbose = FALSE, check_discrete = TRUE){

        if(is_grouped_df(data_density)){
            stop("Density estimation does not permit grouped data frames.\n
                  Please use split-apply-combine to analyze multiple years, or pass multiple column names to the `indep` parameter for multivariate blocs.")
        }

        if( check_discrete & dplyr::n_distinct(dplyr::select(ungroup(data_density), dplyr::all_of(indep))) > 50)
            stop("More than 25 unique values detected in indep. If you are sure you don't want vb_continuous(), set check_discrete = FALSE.")


        indep_lvls <-
            lapply(indep, \(col) Reduce(union,
                                        list(
                                            data_density[[col]],
                                            data_turnout[[col]],
                                            data_vote[[col]]
                                        )
            )
            )
        names(indep_lvls) <- indep

        stopifnot(rlang::has_name(data_density, indep))
        stopifnot(rlang::has_name(data_density, weight))

        stopifnot(!is.null(dv3) | length(c(dv_turnout, dv_voterep, dv_votedem)) == 3)

        if(is.null(dv3)) stopifnot(rlang::has_name(data_turnout, dv_turnout))
        stopifnot(rlang::has_name(data_turnout, indep))
        stopifnot(rlang::has_name(data_turnout, weight))

        if(is.null(dv3)) stopifnot(rlang::has_name(data_vote, dv_turnout))
        if(is.null(dv3)) stopifnot(rlang::has_name(data_vote, c(dv_voterep , dv_votedem)))
        stopifnot(rlang::has_name(data_vote, indep))
        stopifnot(rlang::has_name(data_vote, weight))

        # Remove missing values like vb_continuous()
        data_density <- stats::na.omit(dplyr::select(data_density, dplyr::all_of(c(dplyr::group_vars(data_density), indep, weight))))

        # Start with NULL weights = 1, but grab the col if present
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


        indep_str <- paste(indep, collapse = " * ")
        data_vote <- dplyr::filter(data_vote, get(dv_turnout) == 1)

        if(boot_iters == 0){

            # Estimate Pr(X) ----
            grp_tbl <-
                wtd_table(dplyr::select(dplyr::ungroup(data_density), dplyr::all_of(indep)),
                          weight = weight_density,
                          prop = TRUE, return_tibble = TRUE)
            names(grp_tbl)[1:length(indep)] <- indep

            if(any(grp_tbl$prop < 0.005))
                warning("Extremely small voting bloc detected. Regression may fail.")


            # 3-valued DV if available (fits 1, not 3 models) ----
            if(! is.null(dv3)){

                form_dv3 <- stats::as.formula(sprintf("%s ~ %s", dv3, indep_str))
                lm_dv3   <- stats::lm(form_dv3, data = data_vote[voter_ind, ], weight = weight_vote)

                results <-
                    grp_tbl %>%
                    dplyr::mutate(prob = prop,
                                  prop = NULL,
                                  cond_rep = predict(lm_dv3, newdata = grp_tbl)) %>%
                    dplyr::mutate(net_rep = cond_rep * prob)

                # Fit turnout model and predict, if column name provided
                if(! is.null(dv_turnout)){
                    form_turnout <- stats::as.formula(sprintf("%s ~ %s", dv_turnout, indep_str))
                    lm_turnout <- stats::lm(form_turnout, data = data_turnout, weight = weight_turnout)

                    results <- dplyr::mutate(results,
                                             pr_turnout = stats::predict(lm_turnout, newdata = grp_tbl))
                }

            } else {
                # 3 models, 3 DVs ----
                # Estimate Pr(turnout | X)
                form_turnout <- stats::as.formula(sprintf("%s ~ %s", dv_turnout, indep_str))

                lm_turnout  <-
                    stats::lm(form_turnout,
                              data = data_turnout, weight = weight_turnout)

                # Estimate Pr(vote | turnout, X)
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
                # Predict turnout, vote choice

                # With discrete indep, rare factor levels might not be resampled.
                # Set these levels to NA in grp_tbl to avoid a predict() failure.
                model_lvls <- lm_voterep$xlevels
                miss_lvls  <- lapply(indep, \(var) setdiff(indep_lvls[[var]], model_lvls[[var]]))
                names(miss_lvls) <- indep

                for(col in indep)
                    data.table::set(grp_tbl,
                                    i = which(grp_tbl[[col]] == miss_lvls[[col]]),
                                    j = col, value = NA_integer_)

                dplyr::mutate(grp_tbl,
                              pr_turnout = stats::predict(lm_turnout, newdata = grp_tbl))

                results <-

                    dplyr::mutate(grp_tbl,
                                  prob = prop, prop = NULL,
                                  pr_turnout = stats::predict(lm_turnout, newdata = grp_tbl),
                                  pr_voterep = stats::predict(lm_voterep, newdata = grp_tbl),
                                  pr_votedem = stats::predict(lm_votedem, newdata = grp_tbl),

                                  net_rep = (pr_voterep - pr_votedem) * pr_turnout * prob
                    )
            }

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
                                indep = indep,
                                dv3 = dv3,
                                dv_turnout = dv_turnout,
                                dv_voterep = dv_voterep, dv_votedem = dv_votedem,
                                # Weighted in resampling
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

        colnms <- c("pr_turnout", "pr_votedem", "pr_voterep", "cond_rep")
        out <- dplyr::select(out, any_of("resample"), {indep}, prob, any_of(colnms), net_rep)

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
