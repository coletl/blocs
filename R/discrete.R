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
#'   columns named by \code{dv_turnout}, \code{dv_vote3}, \code{indep}, and \code{weight}.
#' @param indep      string, column name of the independent variable defining
#'   discrete voting blocs.
#' @param dv_vote3        string, column name of the dependent variable coded as
#'   follows: -1 for Democrat vote choice, 0 for no or third-party vote, 1 for
#'   Republican vote choice.
#' @param dv_turnout     string, column name of the dependent variable flagging
#'   voter turnout in \code{data_turnout}. That column must be coded 0 =  no vote, 1 = voted.
#'   \code{data_vote} data sets.
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
#' @export


vb_discrete <-
    function(data,
             data_density = data, data_turnout = data, data_vote = data,
             indep, dv_vote3 = NULL, dv_turnout = NULL,
             weight = NULL, boot_iters = FALSE,
             verbose = FALSE, check_discrete = TRUE){

        if(is_grouped_df(data_density)){
            stop("Density estimation does not permit grouped data frames.\n
                  Please use split-apply-combine to analyze multiple years, or pass multiple column names to the `indep` parameter for multivariate blocs.")
        }

        stopifnot(is.data.frame(data_density))
        stopifnot(is.data.frame(data_turnout))
        stopifnot(is.data.frame(data_vote))

        if( check_discrete & dplyr::n_distinct(dplyr::select(dplyr::ungroup(data_density), dplyr::all_of(indep))) > 50)
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

        stopifnot(rlang::has_name(data_turnout, indep))
        stopifnot(rlang::has_name(data_turnout, weight))

        stopifnot(rlang::has_name(data_vote, indep))
        stopifnot(rlang::has_name(data_vote, weight))

        # Remove missing values like vb_continuous()
        nrow_density <- nrow(data_density)
        data_density <- stats::na.omit(dplyr::select(data_density, dplyr::all_of(c(dplyr::group_vars(data_density), indep, weight))))

        na_check <- nrow_density - nrow(data_density)
        if(na_check/nrow_density > 0.05)
            warning(sprintf("Dropped %s missing values from data_density.", na_check))

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

            # Check that weights greater than non-zero
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
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))

        data_turnout <-
            data_turnout %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))

        data_vote <-
            data_vote %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(indep), ~ as.factor(.x)))

        indep_str <- paste(indep, collapse = " * ")

        if(boot_iters == 0){

            # Estimate Pr(X) ----
            if(is.null(weight)){
                prob_tbl <-
                    dplyr::group_by(data_density, dplyr::across(dplyr::all_of(indep))) %>%
                    dplyr::count() %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(prob = n / sum(n), n = NULL)
            } else {
                prob_tbl <-
                    dplyr::group_by(data_density, dplyr::across(dplyr::all_of(indep))) %>%
                    dplyr::count(wt = if(!is.null(weight)) get({{weight}}) else NULL) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(prob = n / sum(n), n = NULL)
            }

            if(any(prob_tbl$prob < 0.005))
                warning("Extremely small voting bloc detected. Regression may fail.")

            # Weighted mean handles 0 turnout better than lm
            if(is.null(weight)){
                turnout_tbl  <-
                    dplyr::group_by(data_turnout, dplyr::across(dplyr::all_of(indep))) %>%
                    dplyr::summarize(pr_turnout = mean(get(dv_turnout),
                                                       na.rm = TRUE))
            } else {
                turnout_tbl  <-
                    dplyr::group_by(data_turnout, dplyr::across(dplyr::all_of(indep))) %>%
                    dplyr::summarize(pr_turnout =
                                         stats::weighted.mean(get(dv_turnout),
                                                              w = get({{weight}}),
                                                              na.rm = TRUE))
            }

            turnout_tbl <- left_join(prob_tbl, turnout_tbl, by = indep)

            # Fit vote choice
            form_dv3 <- stats::as.formula(sprintf("%s ~ %s", dv_vote3, indep_str))
            # Use tryCatch() to return NA when a
            lm_dv3   <-
                tryCatch(
                    stats::lm(form_dv3, data = data_vote, weight = weight_vote),
                    error = function(e) NULL
                )

            # Model failed
            if(is.null(lm_dv3)){
                results <-
                    turnout_tbl %>%
                    dplyr::mutate(
                        cond_rep   = NA_integer_,
                        net_rep    = NA_integer_) %>%
                    # rounding fixes miniscule pred. probs from 0 observed turnout
                    dplyr::mutate(across(where(is.numeric), round, 10)) %>%
                    dplyr::mutate(across(where(is.numeric), unname))
            } else {

                cond_rep <- tryCatch(
                    expr = {
                        stats::predict(lm_dv3, newdata = turnout_tbl)
                    },

                    error = function(e){
                        tmp_turn <- turnout_tbl

                        # remove factor levels not in (prob. resampled) data
                        miss_ind <- which( ! interaction(tmp_turn[indep]) %in%
                                               interaction(data_vote[indep]))
                        miss_lvls <- tmp_turn[miss_ind, ]
                        tmp_turn[miss_ind, indep] <- NA_character_

                        stats::predict(lm_dv3, newdata = tmp_turn)

                    }
                )

                results <-
                    turnout_tbl %>%
                    dplyr::mutate(
                        cond_rep   = cond_rep,
                        net_rep    = cond_rep * prob
                    ) %>%
                    # rounding fixes miniscule pred. probs from 0 observed turnout
                    dplyr::mutate(across(where(is.numeric), round, 10)) %>%
                    dplyr::mutate(across(where(is.numeric), unname))
            }

            out <- vbdf(results, bloc_var = indep,
                        var_type = "discrete")

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
                                dv_vote3 = dv_vote3,
                                dv_turnout = dv_turnout,
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

        return(out)
    }
