test_that("equivalent to questionr::wtd.table", {

    vec <- sample(letters, 100, replace = TRUE)
    vec[sample(1:100, 10)] <- NA

    weights <- abs(rnorm(100))

    expect_equal(
        c(wtd_table(vec, weight = weights)),
        c(questionr::wtd.table(x = vec, weights = weights, useNA = "always"))

    )

    expect_equal(
        c(wtd_table(vec, weight = weights, prop = TRUE)),
        c(prop.table(questionr::wtd.table(x = vec, weights = weights, useNA = "always")))
    )
})
