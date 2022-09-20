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
#' @param net_rep_only   logical, whether to return only composition and net Republican votes.
#'                       The default \code{FALSE} computes and returns the other components,
#'                       turnout and vote choice. Set to \code{TRUE} to estimate a linear model.
#'                       combining turnout and vote choice estimation.
#'
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
             net_rep_only = FALSE,
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
            collapse::ftransformv(vars = indep, FUN =  collapse::qF)

        data_turnout <-
            data_turnout %>%
            collapse::ftransformv(vars = indep, FUN =  collapse::qF)

        data_vote <-
            data_vote %>%
            collapse::ftransformv(vars = indep, FUN =  collapse::qF)

        indep_str <- paste(indep, collapse = " * ")

        if(boot_iters == 0){

            # Estimate Pr(X) ----
            prob_tbl <-
                wtd_table(dplyr::select(data_density, dplyr::all_of(indep)),
                          weight = weight_density,
                          prop = TRUE, return_tibble = TRUE) %>%
                dplyr::rename(prob = prop)

            if(any(prob_tbl$prob < 0.005))
                warning("Extremely small voting bloc detected. Regression may fail.")

            #########################################
            #### Minimal calc. for Net Rep Votes ####
            #########################################
            if(net_rep_only){

                if(! identical(data_vote, data_turnout) )
                    warning("Ignoring data_turnout argument because net_rep_only is TRUE")

                # Fit vote choice
                form_dv3 <- stats::as.formula(sprintf("%s ~ %s", dv_vote3, indep_str))
                # Use tryCatch() to return NA when lacking variation
                lm_dv3   <-
                    tryCatch(
                        stats::lm(form_dv3, data = data_vote, weight = weight_vote),
                        error = function(e) NULL
                    )

                # Model failed
                if(is.null(lm_dv3)){
                    results <-
                        prob_tbl %>%
                        dplyr::mutate(
                            cond_rep   = NA_integer_,
                            net_rep    = NA_integer_) %>%
                        # rounding fixes miniscule pred. probs from 0 observed turnout
                        dplyr::mutate(across(where(is.numeric), round, 10)) %>%
                        dplyr::mutate(across(where(is.numeric), unname))
                } else {
                # Model fit successfully
                # but resampled data might be missing levels
                    cond_rep <- tryCatch(
                        expr = {
                            stats::predict(lm_dv3, newdata = turnout_tbl)
                            },

                        error = function(e){
                            tmp_tbl <- prob_tbl

                            # remove factor levels not in (prob. resampled) data
                            miss_ind <- which( ! collapse::finteraction(tmp_tbl[indep]) %in%
                                                   collapse::finteraction(data_vote[indep]))
                            miss_lvls <- tmp_tbl[miss_ind, ]
                            tmp_tbl[miss_ind, indep] <- NA_character_

                            stats::predict(lm_dv3, newdata = tmp_tbl)

                        }
                    )

                    results <-
                        prob_tbl %>%
                        dplyr::mutate(
                            cond_rep   = cond_rep,
                            net_rep    = cond_rep * prob
                        ) %>%
                        # rounding fixes miniscule pred. probs from 0 observed turnout
                        dplyr::mutate(across(where(is.numeric), round, 10)) %>%
                        dplyr::mutate(across(where(is.numeric), unname))
                }
            } else {
            ######################################
            ##### Return component estimates #####
            ######################################
                # Weighted mean handles 0 turnout better than lm
                turnout_tbl <-
                    data_turnout %>%
                    collapse::fgroup_by(indep) %>%
                    collapse::get_vars(dv_turnout) %>%
                    collapse::fmean(w = weight_turnout) %>%
                    dplyr::rename(pr_turnout = {{dv_turnout}})


                # Separate grouping for vote-choice data set
                vote_tbl <-
                    data_vote %>%
                    collapse::ftransform(
                        pr_voterep = as.numeric(get({{dv_vote3}}) ==  1),
                        pr_votedem = as.numeric(get({{dv_vote3}}) == -1)
                        ) %>%
                    collapse::fgroup_by(indep) %>%
                    collapse::get_vars(c("pr_voterep", "pr_votedem")) %>%
                    collapse::fmean(w = weight_turnout)

                results <-
                    dplyr::left_join(prob_tbl, turnout_tbl, by = indep) %>%
                    dplyr::left_join(vote_tbl, by = indep) %>%
                    dplyr::mutate(net_rep = prob * pr_turnout * (pr_voterep - pr_votedem))

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
