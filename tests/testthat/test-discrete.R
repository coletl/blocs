test_that("Discrete analysis runs with and without weights", {
    library(dplyr)

    data <- data.frame(
        x_disc = LETTERS[1:4],
        x_cont = 1:4,

        voted    = c( 0, 1,  1,  1),
        vote_dem = c(NA, 0,  1,  1),
        vote_rep = c(NA, 1,  0,  0),
        vote3    = c( 0, 1, -1, -1),

        weight = c(1.5, 2, 0.5, 0)
    )

    vbdf <-
        vb_discrete(data, indep = "x_disc",
                    dv_turnout = "voted",
                    dv_vote3 = "vote3",
                    boot_iters = FALSE)

    check <-
        data.frame(
            prob = rep(0.25, 4),
            pr_turnout = c(0, 1, 1, 1),
            cond_rep = data$vote3,
            net_rep = c(0, 0.25, -0.25, -0.25)
        )

    expect_equal(vbdf$prob,       check$prob)
    expect_equal(vbdf$pr_turnout, check$pr_turnout)
    expect_equal(vbdf$cond_rep,   check$cond_rep)
    expect_equal(vbdf$net_rep,    check$net_rep)

    expect_error(
        expect_warning(
            vbdf_wtd <-
                vb_discrete(data, indep = "x_disc",
                            dv_turnout = "voted",
                            dv_vote3 = "vote3",
                            weight = "weight", boot_iters = FALSE),
            "small voting bloc"
        ), "Weights must be greater than zero"
    )

    data <- mutate(data, weight = c(1.5, 2, 0.5, 0.5))

    vbdf_wtd <-
        vb_discrete(data, indep = "x_disc",
                    dv_turnout = "voted",
                    dv_vote3 = "vote3",
                    weight = "weight", boot_iters = FALSE)

    expect_equal(vbdf_wtd$prob,       (check$prob * data$weight) / sum(check$prob * data$weight))
    expect_equal(vbdf_wtd$pr_turnout, check$pr_turnout)
    expect_equal(vbdf_wtd$cond_rep,   check$cond_rep)
    expect_equal(vbdf_wtd$net_rep,    check$cond_rep * (check$prob * data$weight) / sum(check$prob * data$weight))
}
)

test_that("ANES analysis expected results", {
    data(anes)

    anes20 <- filter(anes, year == 2016)

    vbdf <- vb_discrete(anes20, indep = "race",
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = FALSE)


    check  <-
        filter(anes20, !is.na(race)) %>%
        group_by(race) %>%
        summarize(
            prob = sum(weight) / sum(anes20$weight),
            pr_turnout = weighted.mean(voted, weight, na.rm = TRUE),
            cond_rep   =
                weighted.mean(vote_pres3 == 1, weight, na.rm = TRUE) -
                weighted.mean(vote_pres3 == -1, weight, na.rm = TRUE),
        ) %>%
        mutate(race = as.factor(race), net_rep = prob * cond_rep)

    expect_equal(vbdf, check, ignore_attr = TRUE, tolerance = 0.01)




    vbdf <- vb_discrete(anes20, indep = c("race", "educ"),
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = FALSE)


    check  <-
        filter(anes20, !is.na(race), !is.na(educ)) %>%
        group_by(race, educ) %>%
        summarize(
            prob = sum(weight) / sum(anes20$weight),
            pr_turnout = weighted.mean(voted, weight, na.rm = TRUE),
            cond_rep   =
                weighted.mean(vote_pres3 == 1, weight, na.rm = TRUE) -
                weighted.mean(vote_pres3 == -1, weight, na.rm = TRUE),
        ) %>%
        mutate(race = as.factor(race),
               educ = as.factor(educ),
               net_rep = prob * cond_rep)

    expect_equal(vbdf, check, ignore_attr = TRUE, tolerance = 0.015)

}
)
