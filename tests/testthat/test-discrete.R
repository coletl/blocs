library(dplyr)

##### WEIGHTS ####

test_that("Discrete analysis runs with and without weights", {
    data <- data.frame(
        x_disc = LETTERS[1:4],
        x_cont = 1:4,

        voted    = c( 0, 1,  1,  1),
        pr_votedem = c(NA, 0,  1,  1),
        pr_voterep = c(NA, 1,  0,  0),
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
            pr_votedem = c(0, 0, 1, 1),
            pr_voterep = c(0, 1, 0, 0),
            net_rep = c(0, 0.25, -0.25, -0.25)
        )

    expect_equal(vbdf$prob,       check$prob)
    expect_equal(vbdf$pr_turnout, check$pr_turnout)
    expect_equal(vbdf$pr_voterep,   check$pr_voterep)
    expect_equal(vbdf$pr_votedem,   check$pr_votedem)
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
    expect_equal(vbdf$pr_voterep,     check$pr_voterep)
    expect_equal(vbdf$pr_votedem,     check$pr_votedem)
    expect_equal(vbdf_wtd$net_rep,
                 (check$pr_voterep - check$pr_votedem) * check$pr_turnout * (check$prob * data$weight) / sum(check$prob * data$weight))
}
)

##### ANES ####

test_that("Expected results from ANES analysis", {
    data(anes)
    anes_tmp <- filter(anes, year == sample(seq(1976, 2020, 4), 1))

    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########\n")


    vbdf <- vb_discrete(anes_tmp, indep = "race",
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = FALSE)


    check  <-
        anes_tmp %>%
        group_by(race) %>%
        summarize(
            prob = sum(weight) / sum(anes_tmp$weight),
            pr_turnout = weighted.mean(voted, weight, na.rm = TRUE),
            pr_voterep = weighted.mean(vote_pres3 == 1, weight, na.rm = TRUE),
            pr_votedem = weighted.mean(vote_pres3 == -1, weight, na.rm = TRUE)
        ) %>%
        mutate(resample = "original",
               race = as.factor(race),
               cond_rep = pr_voterep - pr_votedem,
               net_rep = prob * pr_turnout * cond_rep) %>%
        collapse::colorderv(neworder = c("resample", "race",
                                         "prob", "pr_turnout",
                                         "pr_voterep", "pr_votedem",
                                         "cond_rep", "net_rep"))

    expect_equal(vbdf, check, ignore_attr = TRUE)

    # Multivariate blocs ----

    vbdf <-
        anes_tmp %>%
        vb_discrete(indep = c("race", "educ"),
                    dv_vote3 = "vote_pres3",
                    dv_turnout = "voted", weight = "weight",
                    boot_iters = FALSE)


    check  <-
        group_by(anes_tmp, race, educ) %>%
        summarize(
            prob = sum(weight) / sum(anes_tmp$weight),
            pr_turnout = weighted.mean(voted, weight, na.rm = TRUE),
            pr_voterep = weighted.mean(vote_pres3 == 1, weight, na.rm = TRUE),
            pr_votedem = weighted.mean(vote_pres3 == -1, weight, na.rm = TRUE)
        ) %>%
        mutate(resample = "original",
               race = factor(race),
               cond_rep = pr_voterep - pr_votedem,
               net_rep = prob * pr_turnout * cond_rep) %>%
        collapse::colorderv(neworder = c("resample", "race", "educ",
                                         "prob", "pr_turnout",
                                         "pr_voterep", "pr_votedem",
                                         "cond_rep", "net_rep")) %>%
        collapse::roworder(resample, race, educ) %>%
        ungroup()
    # Some problems with density estimation for check.
    # blocs' prob calculation is tested against questionr::wtd.table()
    # in separate test script
    expect_equal(vbdf, check, ignore_attr = TRUE)

}
)

##### BOOTSTRAPPING ####
test_that("Bootstrapping runs", {


    # Many runs on toy data ----
    data <- data.frame(
        x_disc = LETTERS[1:4],
        x_cont = 1:4,

        voted    = c( 0, 1,  1,  1),
        vote_dem = c(NA, 0,  1,  1),
        vote_rep = c(NA, 1,  0,  0),
        vote3    = c( 0, 1, -1, -1),

        weight = c(1.5, 2, 0.5, 0.5)
    )

    set.seed(1)
    vbdf_3 <- vb_discrete(data, indep = "x_disc",
                          dv_turnout = "voted",
                          dv_vote3 = "vote3",
                          weight = "weight", boot_iters = 1e2)

    set.seed(1)
    vbdf_4 <- vb_discrete(data, indep = "x_disc",
                          dv_turnout = "voted",
                          dv_vote3 = "vote3",
                          weight = "weight", boot_iters = 100)

    expect_equal(vbdf_3$prob, vbdf_4$prob)


    #### ANES DATA ####
    data(anes)

    anes_tmp <-
        filter(anes, year == sample(seq(1976, 2020, 4), 1)) %>%
        # Small blocs that correctly create NA values
        filter(!is.na(race), !is.na(educ))

    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########\n")


    # 2 runs on ANES 2020 ----
    set.seed(1)
    vbdf_1a <- vb_discrete(anes_tmp, indep = c("race", "educ"),
                           dv_vote3 = "vote_pres3",
                           dv_turnout = "voted", weight = "weight",
                           boot_iters = 2)

    set.seed(1)
    vbdf_1b <- vb_discrete(anes_tmp, indep = c("race", "educ"),
                           dv_vote3 = "vote_pres3",
                           dv_turnout = "voted", weight = "weight",
                           boot_iters = 2)

    set.seed(2)
    vbdf_2 <- vb_discrete(anes_tmp, indep = c("race", "educ"),
                          dv_vote3 = "vote_pres3",
                          dv_turnout = "voted", weight = "weight",
                          boot_iters = 2)

    expect_equal(vbdf_1a, vbdf_1b)
    expect_equal(nrow(vbdf_1a), nrow(vbdf_2))
    expect_false(isTRUE(all.equal(vbdf_1a, vbdf_2)))



    # Vector of boot_iters ----
    set.seed(1)
    boot_iters <- c(density = 0, turnout = 10, 20)

    expect_error(vb_discrete(anes_tmp, indep = c("race", "educ"),
                             dv_vote3 = "vote_pres3",
                             dv_turnout = "voted", weight = "weight",
                             boot_iters = boot_iters),
                 "If boot_iters has length greater")

    boot_iters <- c(density = 0, turnout = 10, vote = 20)
    vbdf <-
        expect_warning(
            vb_discrete(anes_tmp, indep = c("race", "educ"),
                        dv_vote3 = "vote_pres3",
                        dv_turnout = "voted", weight = "weight",
                        boot_iters = boot_iters),
            "No resampling performed for density data"
        )

    expect_equal(
        length(unique(interaction(vbdf$race, vbdf$educ))),
        length(unique(vbdf$prob))
    )
}
)

test_that("Grouped data.frame throws an error",
          {
              data(anes)
              expect_error(
                  group_by(anes, year) %>% vb_discrete(),
                  "does not permit grouped data frames"
              )

              expect_error(
                  group_by(anes, year) %>% vb_continuous(),
                  "does not permit grouped data frames"
              )
          })

# test_that("Grouped data.frame works", {
#     data(anes)
#     anes_list <-
#         filter(anes, year %in% sample(seq(1976, 2020, 4), 5)) %>%
#         split(., .$year)
#     anes <- bind_rows(anes_list)
#
#     set.seed(2)
#     vbdf_grp <-
#         group_by(anes, year) %>%
#         summarize(
#             vb_discrete(data = ., indep = "race",
#                         dv_vote3 = "vote_pres3", dv_turnout = "voted",
#                         weight = "weight", cache = TRUE,
#                         boot_iters = FALSE)
#         ) %>% ungroup()
#
#     set.seed(2)
#     vbdf_split <-
#         lapply(anes_list, vb_discrete,
#                indep = "race", dv_vote3 = "vote_pres3", dv_turnout = "voted",
#                weight = "weight", cache = TRUE,
#                boot_iters = FALSE) %>%
#         bind_rows(.id = "year")
#
#     expect_equal(vbdf_grp, vbdf_split)
#
# })
