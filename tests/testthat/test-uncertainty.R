test_that("Uncertainty runs for discrete blocs", {
    library(dplyr)

    data(anes)
    anes_tmp <- filter(anes, year == sample(seq(1976, 2020, 4), 1))
    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########")


    vbdf <- vb_discrete(anes_tmp, indep = "race",
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = 10)
    summary_disc <- vb_uncertainty(vbdf, na.rm = TRUE)
    expect_s3_class(summary_disc, "vbsum")

    summary_check <-
        group_by(vbdf, race) %>%
        summarize(
            prob_mean         = mean(prob, na.rm = TRUE),
            prob_median       = median(prob, na.rm = TRUE),
            prob_low          = quantile(prob, 0.025, na.rm = TRUE),
            prob_high         = quantile(prob, 0.975, na.rm = TRUE),
            pr_turnout_mean   = mean(pr_turnout, na.rm = TRUE),
            pr_turnout_median = median(pr_turnout, na.rm = TRUE),
            pr_turnout_low    = quantile(pr_turnout, 0.025, na.rm = TRUE),
            pr_turnout_high   = quantile(pr_turnout, 0.975, na.rm = TRUE),
            pr_voterep_mean   = mean(pr_voterep, na.rm = TRUE),
            pr_voterep_median = median(pr_voterep, na.rm = TRUE),
            pr_voterep_low    = quantile(pr_voterep, 0.025, na.rm = TRUE),
            pr_voterep_high   = quantile(pr_voterep, 0.975, na.rm = TRUE),
            pr_votedem_mean   = mean(pr_votedem, na.rm = TRUE),
            pr_votedem_median = median(pr_votedem, na.rm = TRUE),
            pr_votedem_low    = quantile(pr_votedem, 0.025, na.rm = TRUE),
            pr_votedem_high   = quantile(pr_votedem, 0.975, na.rm = TRUE),
            cond_rep_mean     = mean(cond_rep, na.rm = TRUE),
            cond_rep_median   = median(cond_rep, na.rm = TRUE),
            cond_rep_low      = quantile(cond_rep, 0.025, na.rm = TRUE),
            cond_rep_high     = quantile(cond_rep, 0.975, na.rm = TRUE),
            net_rep_mean      = mean(net_rep, na.rm = TRUE),
            net_rep_median    = median(net_rep, na.rm = TRUE),
            net_rep_low       = quantile(net_rep, 0.025, na.rm = TRUE),
            net_rep_high      = quantile(net_rep, 0.975, na.rm = TRUE)
        )

    expect_false(anyNA(select(summary_disc, starts_with("pr"), ends_with("rep"))))
    expect_true(anyNA(select(summary_disc, race)))
    expect_equal(summary_disc, summary_check,
                 ignore_attr = TRUE)

    # vector of boot_iters
    expect_warning(
        vbdf <-
            vb_discrete(anes_tmp, indep = c("race", "educ"),
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = c(density = 0, turnout = 10, vote = 10)),
        "No resampling performed for density data"
    )

    summary_disc <- vb_uncertainty(vbdf, na.rm = TRUE)
    summary_check <-
        group_by(vbdf, race, educ) %>%
        summarize(
            prob_mean         = mean(prob, na.rm = TRUE),
            prob_median       = median(prob, na.rm = TRUE),
            prob_low          = quantile(prob, 0.025, na.rm = TRUE),
            prob_high         = quantile(prob, 0.975, na.rm = TRUE),
            pr_turnout_mean   = mean(pr_turnout, na.rm = TRUE),
            pr_turnout_median = median(pr_turnout, na.rm = TRUE),
            pr_turnout_low    = quantile(pr_turnout, 0.025, na.rm = TRUE),
            pr_turnout_high   = quantile(pr_turnout, 0.975, na.rm = TRUE),
            pr_voterep_mean   = mean(pr_voterep, na.rm = TRUE),
            pr_voterep_median = median(pr_voterep, na.rm = TRUE),
            pr_voterep_low    = quantile(pr_voterep, 0.025, na.rm = TRUE),
            pr_voterep_high   = quantile(pr_voterep, 0.975, na.rm = TRUE),
            pr_votedem_mean   = mean(pr_votedem, na.rm = TRUE),
            pr_votedem_median = median(pr_votedem, na.rm = TRUE),
            pr_votedem_low    = quantile(pr_votedem, 0.025, na.rm = TRUE),
            pr_votedem_high   = quantile(pr_votedem, 0.975, na.rm = TRUE),
            cond_rep_mean     = mean(cond_rep, na.rm = TRUE),
            cond_rep_median   = median(cond_rep, na.rm = TRUE),
            cond_rep_low      = quantile(cond_rep, 0.025, na.rm = TRUE),
            cond_rep_high     = quantile(cond_rep, 0.975, na.rm = TRUE),
            net_rep_mean      = mean(net_rep, na.rm = TRUE),
            net_rep_median    = median(net_rep, na.rm = TRUE),
            net_rep_low       = quantile(net_rep, 0.025, na.rm = TRUE),
            net_rep_high      = quantile(net_rep, 0.975, na.rm = TRUE)
        )

    expect_equal(nrow(summary_disc), length(unique(paste(anes_tmp$race, anes_tmp$educ))))
    expect_equal(summary_disc, summary_check,
                 ignore_attr = TRUE)

})

test_that("Uncertainty runs for continuous blocs", {
    library(dplyr)

    data(anes)
    anes_tmp <- filter(anes, year == sample(seq(1976, 2020, 4), 1))
    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########")


    vbdf_cont <- vb_continuous(filter(anes_tmp, !is.na(age)), indep = "age",
                               dv_vote3 = "vote_pres3",
                               dv_turnout = "voted", weight = "weight",
                               boot_iters = 5)

    summary_cont <- vb_uncertainty(vbdf_cont,
                                   estimates = c("prob", "pr_turnout", "net_rep"))

    vbdf_cont$age_bin <- vbdf_cont$age - vbdf_cont$age %% 10

    summary_bin <- vb_uncertainty(vbdf_cont, estimates = c("prob", "pr_turnout", "net_rep"),
                                  type = "binned", bin_col = "age_bin")

    summary_cont <- vb_uncertainty(vbdf_cont, estimates = c("prob", "pr_turnout", "net_rep"),
                                   type = "continuous", bin_col = "age_bin")
    expect_s3_class(summary_cont, "vbsum")
    expect_s3_class(summary_bin, "vbsum")

    expect_error(vb_uncertainty(vbdf_cont, estimates = c("prob", "pr_turnout", "net_rep"),
                                type = "asdf", bin_col = "age_bin"),
                 "should be one of")

})


test_that("Uncertainty runs for vb difference", {
    library(dplyr)

    data(anes)
    anes_tmp <- filter(anes, year %in% sample(seq(1976, 2020, 4), 2))

    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########\n")


    vbdf_cont_list <-
        anes_tmp %>%
        filter(!is.na(age)) %>%
        split(., .$year) %>%
        lapply(
            vb_continuous,
            indep = "age",
            dv_vote3 = "vote_pres3",
            dv_turnout = "voted", weight = "weight",
            boot_iters = 10,
            min_val = 17, max_val = 100
        )

    vbdf_cont <-  bind_rows(vbdf_cont_list, .id = "year")
    vbdiff_cont <- vb_difference(vbdf_cont, sort_col = "year")


    vbdiff_cont$age_bin <- vbdiff_cont$age - vbdiff_cont$age %% 10

    diffsum_bin <- vb_uncertainty(vbdiff_cont,
                                  type = "binned", bin_col = "age_bin")

    diffsum_cont <- vb_uncertainty(vbdiff_cont,
                                   type = "continuous", bin_col = "age_bin")
    expect_s3_class(diffsum_bin, "vbsum")
    expect_s3_class(diffsum_cont, "vbsum")

    identical(diffsum_bin,
              vb_summary(vbdiff_cont,
                         type = "binned", bin_col = "age_bin"))
    identical(diffsum_cont,
              vb_uncertainty(vbdiff_cont,
                             type = "continuous", bin_col = "age_bin"))

})


