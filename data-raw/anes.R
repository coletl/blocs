# Prepare sample of 2020 ANES data
library(dplyr)
library(haven)

set.seed(575)

# Read ANES cumulative file
anes_full <- read_sav("data-raw/anes_timeseries_cdf_spss_20211118.sav",
                      col_select =
                          c(
                              year   = VCF0004,
                              respid = VCF0006,
                              weight = VCF0009z,

                              # Basic demographics
                              race   = VCF0105b,
                              gender = VCF0104,
                              educ   = VCF0110,
                              age    = VCF0101,

                              # Voting behavior
                              turnout = VCF0703,
                              vote_pres = VCF0704
                          ))

# Trim columns ----
# Removing labels makes it easier to use dplyr
anes_full <- zap_labels(anes_full)

anes_trim <-
    select(
        anes_full,
        year   = VCF0004,
        respid = VCF0006,
        weight = VCF0009z,

        # Basic demographics
        race   = VCF0105b,
        gender = VCF0104,
        educ   = VCF0110,
        age    = VCF0101,

        # Voting behavior
        turnout = VCF0703,
        vote_pres = VCF0704
    ) %>%
    # Drop years without presidential vote choice
    filter(year %in% seq(1952, 2020, 4))


# Recode responses ----

anes <-
    mutate(anes_trim,
           race   = recode(race,
                           `1` = "white", `2` = "black", `3` = "hispanic", `4` = "other",
                           `9` = NA_character_, `0` = NA_character_
           ),

           educ   = recode(educ,
                           `1` = "HS or less", `2` = "HS or less",
                           `3` = "some college", `4` = "college",
                           .default = NA_character_
           ),

           gender = case_when(gender == 1 ~ "male",
                              gender == 2 ~ "female",
                              gender == 3 ~ "other"
           ),
           age    = case_when(age == 0 ~ NA_real_,
                              TRUE ~ age),

           voted  = as.numeric(turnout == 3),

           vote_pres = recode(vote_pres,
                              `1` = "dem", `2` = "rep",
                              `3` = "third"),
           vote_pres_dem = as.numeric(vote_pres == "dem"),
           vote_pres_rep = as.numeric(vote_pres == "rep"),

           vote_pres3    = case_when(vote_pres == "dem" ~ -1L,
                                     vote_pres == "rep" ~  1L,
                                     turnout   != "voted" ~  0L)
    )

anes <- select(anes, -turnout)

usethis::use_data(anes, overwrite = TRUE)
