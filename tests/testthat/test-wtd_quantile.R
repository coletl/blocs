source("../wtd_quantile/andrey_wquantile.R")

# Critical bug in Hmisc::wtd.quantile when used with non-integer weights:
# https://github.com/harrelfe/Hmisc/issues/97

# Set testing parameters

# This seed ruins whdquantile for prob = 1
# set.seed(575)

set.seed(1234)

n <- 1e5
vec <- rnorm(n, sd = 100)
weights <- abs(rnorm(n))

probs <- seq(0, 1, 0.2)

all.equal(
    unname(wtd_quantile(x = vec, probs, weights)),
    unname(whdquantile(x = vec, probs, weights))
)

# Tests
test_that("roughly equivalent to andrey's H-D quantile", {
    probs <- seq(0, 1, 0.2)

    expect_equal(
        unname(wtd_quantile(x = vec, probs, weight = weights)),
        unname(whdquantile(vec, probs, weights)),

        tolerance = 0.001
        )
    }
)


