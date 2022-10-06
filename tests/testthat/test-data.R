test_that("Data properly prepared", {
    data(anes)

    expect_s3_class(anes, "data.frame")

    expect_equal(dim(anes), c(46838, 12))

    expect_false(anyNA(anes %>% select(year, respid, weight)))

    expect_equal(sort(names(anes)),
                 sort(c("year", "respid", "weight", "race", "gender",
                        "educ", "age",
                        "vote_pres", "voted", "vote_pres_dem", "vote_pres_rep",
                        "vote_pres3")))

    na_check <-
        group_by(anes, year) %>%
        summarize(across(everything(), ~ mean(is.na(.x))))

    expect_false(any(na_check == 1))

})
