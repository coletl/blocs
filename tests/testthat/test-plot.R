test_that("vb_plot runs for discrete vbdf summary", {
    library(dplyr)

    data(anes)

    anes_test <- filter(anes, year %in% sample(anes$year, 1))

    cat(unique(anes_test$year))

    vbsum <-
        vb_discrete(anes_test, dv_vote3 = "vote_pres3",
                    dv_turnout = "voted", indep = c("race"),
                    weight = "weight", boot_iters = 10) %>%
        bind_rows(.id = "year") %>%
        vb_uncertainty()

    tags <-
        c("prob", "pr_turnout",
          "pr_voterep", "pr_votedem",
          "cond_rep", "net_rep") %>%
        lapply(c("%s_mean", "%s_low", "%s_high"),
               sprintf, .)

    Map(
        f = function(y, ymin, ymax) vb_plot(data = vbsum, x_col = "race",
                                                  y_col = y, ymin_col = ymin, ymax_col = ymax),
        y = tags[[1]],
        ymin = tags[[2]], ymax = tags[[3]]
    )
})


test_that("vb_plot runs for discrete difference summary", {
    library(dplyr)

    data(anes)

    anes_test <-
        filter(anes,
               year %in%
                   {sample(anes$year, 1) %>% c(. - 4, ., . + 4)})

    cat(unique(anes_test$year))

    anes_list <- split(anes_test, anes_test$year)
    vbsum_diff <-
        lapply(anes_list, vb_discrete, dv_vote3 = "vote_pres3",
               dv_turnout = "voted", indep = c("race"),
               weight = "weight", boot_iters = 10) %>%
        bind_rows(.id = "year") %>%
        vb_difference() %>%
        vb_uncertainty(na.rm = TRUE)

    tags <-
        c("prob", "pr_turnout",
          "pr_voterep", "pr_votedem",
          "cond_rep", "net_rep") %>%
        lapply(c("diff_%s_mean", "diff_%s_low", "diff_%s_high"),
               sprintf, .)

    Map(
        f = function(y, ymin, ymax) vb_plot(data = vbsum_diff, x_col = "race",
                                                  y_col = y, ymin_col = ymin, ymax_col = ymax),
        y = tags[[1]],
        ymin = tags[[2]], ymax = tags[[3]]
    )

    # vb_plot(vbsum_diff,
    #               x_col = "race",
    #               y_col = "diff_prob_mean",
    #               ymin_col = "diff_prob_low", ymax_col = "diff_prob_high")
})

test_that("vb_plot runs for continuous vbdf summary", {
    library(dplyr)

    data(anes)

    anes_test <- filter(anes, year %in% sample(anes$year, 1))

    cat(unique(anes_test$year))

    vbdf <-
        filter(anes_test, !is.na(age)) %>%
        vb_continuous(dv_vote3 = "vote_pres3",
                      dv_turnout = "voted", indep = c("age"),
                      weight = "weight", boot_iters = 10) %>%
        bind_rows(.id = "year")

    vbsum_cont <- vb_summary(vbdf)
    vbdf$age_bin <- floor(vbdf$age / 10) * 10
    vbsum_bin <- vb_summary(vbdf, type = "bin", bin_col = "age_bin")

    tags <-
        c("prob", "pr_turnout",
          # "pr_voterep", "pr_votedem",
          "cond_rep", "net_rep") %>%
        lapply(c("%s_mean", "%s_low", "%s_high"),
               sprintf, .)

    Map(
        f = function(y, ymin, ymax) vb_plot(data = vbsum_cont, x_col = "age",
                                                  y_col = y, ymin_col = ymin, ymax_col = ymax),
        y = tags[[1]],
        ymin = tags[[2]], ymax = tags[[3]]
    )

    Map(
        f = function(y, ymin, ymax) vb_plot(data = vbsum_bin, x_col = "age_bin",
                                                  y_col = y, ymin_col = ymin, ymax_col = ymax),
        y = tags[[1]],
        ymin = tags[[2]], ymax = tags[[3]]
    )
})


test_that("vb_plot runs for continuous difference summary", {
    library(dplyr)

    data(anes)

    anes_test <-
        filter(anes,
               year %in%
                   {sample(anes$year, 1) %>% c(. - 4, ., . + 4)}) %>%
        filter(!is.na(age))

    cat(unique(anes_test$year))

    anes_list <- split(anes_test, anes_test$year)
    vbsum_diff <-
        lapply(anes_list, vb_continuous, dv_vote3 = "vote_pres3",
               dv_turnout = "voted", indep = c("age"), min_val = 17, max_val = 99,
               weight = "weight", boot_iters = 10) %>%
        bind_rows(.id = "year") %>%
        vb_difference() %>%
        vb_uncertainty()

    tags <-
        c("prob", "pr_turnout",
          # "pr_voterep", "pr_votedem",
          "cond_rep", "net_rep") %>%
        lapply(c("diff_%s_mean", "diff_%s_low", "diff_%s_high"),
               sprintf, .)

    Map(
        f = function(y, ymin, ymax) vb_plot(data = vbsum_diff, x_col = "age",
                                                  y_col = y, ymin_col = ymin, ymax_col = ymax),
        y = tags[[1]],
        ymin = tags[[2]], ymax = tags[[3]]
    )

    # vb_plot(vbsum_diff,
    #               x_col = "race",
    #               y_col = "diff_prob_mean",
    #               ymin_col = "diff_prob_low", ymax_col = "diff_prob_high")
})
