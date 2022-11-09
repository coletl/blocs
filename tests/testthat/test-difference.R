test_that("difference works for discrete blocs", {

    library(dplyr)
    data(anes)

    anes_tmp <- filter(anes, year %in% sample(seq(1976, 2020, 4), 2))

    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########\n")


    vbdf_disc_list <-
        anes_tmp %>%
        split(., .$year) %>%
        lapply(
            vb_discrete,
            indep = c("race", "educ"),
            dv_vote3 = "vote_pres3",
            dv_turnout = "voted", weight = "weight",
            boot_iters = FALSE
        )

    vbdf_disc <-  bind_rows(vbdf_disc_list, .id = "year")
    vbdiff_disc <- vb_difference(vbdf_disc, sort_col = "year")

    diff_check <-
        tibble(
            diff_prob       = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$prob -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$prob,
            diff_pr_turnout = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$pr_turnout -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$pr_turnout,
            diff_pr_voterep = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$pr_voterep -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$pr_voterep,
            diff_pr_votedem = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$pr_votedem -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$pr_votedem,
            diff_cond_rep = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$cond_rep -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$cond_rep,
            diff_net_rep = filter(vbdf_disc_list[[2]], !is.na(race), !is.na(educ))$net_rep -
                                        filter(vbdf_disc_list[[1]], !is.na(race), !is.na(educ))$net_rep)

    expect_equal(filter(vbdiff_disc, !is.na(race), !is.na(educ)) %>% select(starts_with("diff")),
                 diff_check, ignore_attr = TRUE)

})

test_that("difference works with bootstrapping", {
    library(dplyr)
    data(anes)

    anes_tmp <- filter(anes, year %in% sample(seq(1976, 2020, 4), 2))

    cat("########### TESTING WITH ANES", unique(anes_tmp$year), "###########\n")

    vbdf_disc_list <-
        anes_tmp %>%
        split(., .$year) %>%
        lapply(
            vb_discrete,
            indep = "race",
            dv_vote3 = "vote_pres3",
            dv_turnout = "voted", weight = "weight",
            boot_iters = c(density = 0, turnout = 5, vote = 10)
        ) %>%
        lapply(filter, !is.na(race))
    vbdf_disc <-  bind_rows(vbdf_disc_list, .id = "year")
    vbdiff_disc <- vb_difference(vbdf_disc, sort_col = "year")

    diff_check <-
        tibble(
            diff_prob       = vbdf_disc_list[[2]]$prob - vbdf_disc_list[[1]]$prob,
            diff_pr_turnout = vbdf_disc_list[[2]]$pr_turnout - vbdf_disc_list[[1]]$pr_turnout,
            diff_pr_voterep = vbdf_disc_list[[2]]$pr_voterep - vbdf_disc_list[[1]]$pr_voterep,
            diff_pr_votedem = vbdf_disc_list[[2]]$pr_votedem - vbdf_disc_list[[1]]$pr_votedem,
            diff_cond_rep   = vbdf_disc_list[[2]]$cond_rep - vbdf_disc_list[[1]]$cond_rep,
            diff_net_rep    = vbdf_disc_list[[2]]$net_rep - vbdf_disc_list[[1]]$net_rep)

    expect_equal(vbdiff_disc %>% select(starts_with("diff")),
                 diff_check, ignore_attr = TRUE)

})

test_that("difference works for continuous blocs", {

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
            boot_iters = 3,
            min_val = 17, max_val = 100
        )
    vbdf_cont <-  bind_rows(vbdf_cont_list, .id = "year")
    vbdiff_cont <- vb_difference(vbdf_cont, sort_col = "year")

    diff_check <-
        tibble(
            diff_prob       = filter(vbdf_cont_list[[2]], !is.na(age))$prob -
                                        filter(vbdf_cont_list[[1]], !is.na(age))$prob,
            diff_pr_turnout = filter(vbdf_cont_list[[2]], !is.na(age))$pr_turnout -
                                        filter(vbdf_cont_list[[1]], !is.na(age))$pr_turnout,
            diff_cond_rep = filter(vbdf_cont_list[[2]], !is.na(age))$cond_rep -
                                        filter(vbdf_cont_list[[1]], !is.na(age))$cond_rep,
            diff_net_rep = filter(vbdf_cont_list[[2]], !is.na(age))$net_rep -
                                        filter(vbdf_cont_list[[1]], !is.na(age))$net_rep)

    expect_equal(filter(vbdiff_cont, !is.na(age)) %>% select(starts_with("diff")),
                 diff_check, ignore_attr = TRUE)
})
