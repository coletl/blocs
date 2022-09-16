devtools::load_all()

test_that("Data properly prepared", {
    data(anes_sample)

    expect_s3_class(anes_sample, "data.frame")

    expect_equal(dim(anes_sample), c(6822, 8))

    expect_false(anyNA(anes_sample %>% select(year, respid, weight)))

    expect_equal(sort(names(anes_sample)),
                 sort(c("year", "respid", "weight", "race", "gender",
                        "educ", "age", "racialres")))

})
