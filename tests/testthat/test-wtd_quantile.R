test_that("roughly equivalent to reldist::wtd.quantile", {

    vec <- rnorm(1e3, sd = 100)
    weights <- abs(rnorm(1e3))

    #### reldist does not return min and max for 0, 1 ####
    # probs <- seq(0, 1, 0.2)
    probs <- seq(0.2, 0.8, 0.2)

    expect_equal(
        unname(wtd_quantile(x = vec, probs, weight = weights)),
        unname(reldist::wtd.quantile(vec, q = probs, weight = weights)),

        #### TOLERANCE TOO HIGH? ####
        tolerance = 0.01
        )
    }
)
