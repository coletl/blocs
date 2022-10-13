test_that("estimate_density returns expected form", {

    x <- sample(1:10, 1e5, replace = TRUE)
    dens_out <- estimate_density(x, min = min(x), max = max(x),
                                 n_points = 10)

    expect_equal(length(dens_out), 10)
    expect_equal(unname(dens_out), rep(0.1, 10), tolerance = 0.05)


    y <- cbind(x, jitter(x * 4, 12))

    dens_out <- estimate_density(y,
                                 min = apply(y, 2, min),
                                 max = apply(y, 2, max),
                                 n_points = 10)




    n_points <- sample(1:100, 1)

    z <- cbind(rbinom(100, n_points, 0.2), rpois(100, n_points),
               rnorm(100), rbeta(100, 1, n_points))

})

test_that("estimate_density weights properly", {
    n_points <- sample(1:100, 1)

    x <- sample(1:10, 1e5, replace = TRUE)

    weights <- rep(1, length(x))
    weights[x == 1]  <- 0
    weights[x == 2]  <- 0
    weights[x == 3]  <- 5
    weights[x == 4]  <- 5
    weights[x == 5]  <- 5
    weights[x == 6]  <- 5
    weights[x == 7]  <- 1
    weights[x == 8]  <- 1
    weights[x == 9]  <- 1
    weights[x == 10] <- 1

    expect_warning(
        dens_out <-
            estimate_density(x, min = min(x), max = max(x),
                             n_points = 10, w = weights),
        "Weights don't sum to sample size")
    round(dens_out, 3)

    expect_equal(
        dens_out,
        c(0, 0,
          rep(5/24, 4),
          rep(1/24, 4)) %>% setNames(1:length(dens_out)),
        tolerance = 0.05
    )

})



test_that("vb_continuous runs on ANES", {

    library(dplyr)
    data(anes)

    test_year <- sample(unique(anes$year), 1)
    anes_test <- filter(anes, year == test_year, !is.na(age))

    vbdf <- vb_continuous(data = anes_test, indep = "age",
                          dv_turnout = "voted",
                          dv_vote3 = "vote_pres3")

    expect_false(is.null(vbdf$pr_turnout))

    anes_test$rand <- rnorm(nrow(anes_test), mean = 20, sd = 10)

    vbdf <-
        vb_continuous(data = anes_test, indep = c("age", "rand"),
                      dv_turnout = "voted", dv_vote3 = "vote_pres3")

    # Vector of boot_Iters
    vbdf <-
        vb_continuous(data = anes_test, indep = c("age", "rand"),
                      dv_turnout = "voted", dv_vote3 = "vote_pres3",
                      boot_iters = c(density = 0, turnout = 5, vote = 10))

    # Can't fit weighted GAM
    expect_error(
        expect_warning(
            vb_continuous(data = anes_test, indep = c("age", "rand"),
                          dv_turnout = "voted", dv_vote3 = "vote_pres3",
                          weight = "weight"),
            "Defining blocs along multiple continuous variables"

        ),
        "Cannot fit a weighted GAM"
    )

    # Multiple continuous variables
    set.seed(1)
    vbdfw <-
        expect_warning(
            vb_continuous(data = anes_test, indep = c("age", "rand"),
                          dv_turnout = "voted", dv_vote3 = "vote_pres3",
                          weight = "weight", boot_iters = 5),
            "Defining blocs along multiple continuous variables"
        )

    set.seed(1)
    vbdfw2 <-
        expect_warning(
            vb_continuous(data = anes_test, indep = c("age", "rand"),
                          dv_turnout = "voted", dv_vote3 = "vote_pres3",
                          weight = "weight", boot_iters = 5),
            "Defining blocs along multiple continuous variables"
        )

    expect_identical(vbdfw, vbdfw2)

})
