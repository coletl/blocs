<!-- badges: start -->

[![R-CMD-check](https://github.com/coletl/blocs/workflows/R-CMD-check/badge.svg)](https://github.com/coletl/blocs/actions)

<!-- badges: end -->

# blocs: Estimate and Visualize Voting Blocs' Partisan Contributions

This package defines functions that combine data on voting blocs' size, turnout, and vote choice to estimate each bloc's vote contributions to the Democratic and Republican parties. The package also includes functions for uncertainty estimation and plotting. Users may define voting blocs along a discrete or continuous variable. The package implements methods described in [Grimmer, Marble, and Tanigawa-Lau (2022)](https://osf.io/preprints/socarxiv/c9fkg/).

## Installation

``` r
# install.packages("devtools")
devtools::install_github("coletl/blocs", dependencies = TRUE)
```

## Quick start

``` r
library(blocs)
data(anes)

anes20 <- anes[anes$year == 2020, ]

# Define voting blocs along a discrete variable, race
# pass to dv_vote3 the column coding vote choice: -1 = Democrat, 0 = third-party, 1 = Republican, and NA for no vote.
# pass to dv_turnout the column coding voter turnout: 0 = no vote, 1 = voted
vb_race <- vb_discrete(anes20, indep = "race", dv_vote3 = "vote_pres3", dv_turnout = "voted")

# Define voting blocs along a continuous variable, age 
# Kernel density estimation prohibits missing values in the independent variable
vb_age <- vb_continuous(anes20[!is.na(anes20$age), ], 
                        indep = "age", dv_vote3 = "vote_pres3", dv_turnout = "voted")

# Bootstrap uncertainty
vb_out <- 
    vb_discrete(anes20, indep = "race", dv_vote3 = "vote_pres3", dv_turnout = "voted", 
                boot_iters = 10)

vb_summary(vb_out)
```

