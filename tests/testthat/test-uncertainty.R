test_that("Uncertainty runs", {
    data(anes)
    anes_tmp <- filter(anes, year == sample(seq(1976, 2020, 4), 1))
    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########")


    vbdf <- vb_discrete(anes_tmp, indep = "race",
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = 10)

    vbdf_cont <- vb_continuous(anes_tmp, indep = "age",
                               dv_vote3 = "vote_pres3",
                               dv_turnout = "voted", weight = "weight",
                               boot_iters = 10)

    summary_disc <- vb_uncertainty(vbdf)
    expect_equal(nrow(summary_disc), length(unique(anes_tmp$race)))

    summary_cont <- vb_uncertainty(vbdf_cont,
                                   estimates = c("prob", "pr_turnout", "net_rep"))

    vbdf_cont$age_bin <- vbdf_cont$age - vbdf_cont$age %% 10

    summary_bin <- vb_uncertainty(vbdf_cont, estimates = c("prob", "pr_turnout", "net_rep"),
                                  type = "binned", bin_col = "age_bin")
})
