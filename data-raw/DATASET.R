# Prepare sample of 2020 ANES data

library(dplyr)
library(haven)
library(rsample)

set.seed(575)

# Read ANES cumulative file
anes_full <- read_sav("data-raw/anes_timeseries_cdf_spss_20211118.sav")

# Trim columns ----
anes_trim <-
    data.frame(
        # When you analyze a survey, always keep
        # the year (or some date), a respondent ID, and the survey weights!
        year   = anes_full$VCF0004,
        respid = anes_full$VCF0006,
        weight = anes_full$VCF0009z,

        # Basic demographics
        race   = anes_full$VCF0105b,
        gender = anes_full$VCF0104,
        educ   = anes_full$VCF0110,
        age    = anes_full$VCF0101
    )

# Removing labels makes it easier to use dplyr
anes_full <- zap_labels(anes_full)

anes_trim <-
    select(
        anes_full,
        # When you analyze a survey, always keep
        # the year (or some date), a respondent ID, and the survey weights!
        year   = VCF0004,
        respid = VCF0006,
        weight = VCF0009z,

        # Basic demographics
        race   = VCF0105b,
        gender = VCF0104,
        educ   = VCF0110,
        age    = VCF0101,

        # Racial resentment
        resent_slavery = VCF9039,
        resent_favors  = VCF9040,
        resent_try     = VCF9041,
        resent_deserve = VCF9042
    )

# Recode responses ----

anes <-
    mutate(anes_trim,
           ######################################################################
           ### recode() function is simple but usage may change in the future ###
           ######################################################################
           # raw column to recode
           race   = recode(race,
                           # from the codebook values
                           `1` = "white", `2` = "black", `3` = "hispanic", `4` = "other",
                           `9` = NA_character_, `0` = NA_character_
           ),

           educ   = recode(educ,
                           `1` = "HS or less", `2` = "HS or less",
                           `3` = "some college", `4` = "college",
                           # if the value is not one of the above, make it NA
                           .default = NA_character_
           ),

           ######################################################################
           ########### case_when() function is flexible but verbose #############
           ######################################################################
           gender = case_when(gender == 1 ~ "male",
                              gender == 2 ~ "female",
                              gender == 3 ~ "other"
           ),
           # dplyr functions are sensitive to the
           # type of missing values. make sure it matches
           # the original column (in this case, a real number)
           age    = case_when(age == 0 ~ NA_real_,
                              # in all other cases, keep the same value
                              TRUE ~ age)
    )

## Racial resentment ----
anes <-
    mutate(anes,
           # Return to codebook for the proper coding of each variable
           resent_slavery = case_when(resent_slavery > 5 ~ NA_real_,
                                      TRUE ~ resent_slavery),
           resent_favors  = case_when(resent_favors > 5 ~ NA_real_,
                                      TRUE ~ resent_favors),
           resent_try     = case_when(resent_try > 5 ~ NA_real_,
                                      TRUE ~ resent_try),
           resent_deserve = case_when(resent_deserve > 5 ~ NA_real_,
                                      TRUE ~ resent_deserve)
    )

# Reorder 2/4 responses
anes <-
    mutate(anes,
           resent_try    = 1 + max(resent_try, na.rm = TRUE)    - resent_try,
           resent_favors = 1 + max(resent_favors, na.rm = TRUE) - resent_favors)

# Rescale and summarize
anes <- mutate(anes, across(starts_with("resent_"), scale))

anes <-
    mutate(rowwise(anes),
           racialres = mean(c(resent_slavery, resent_favors,
                              resent_try, resent_deserve)
                            )
           )
# Drop component columns
anes <- select(anes, -starts_with("resent_"))

# Sample ----
anes_sample <- group_by(anes, year) %>% sample_frac(size = 1/10)

usethis::use_data(anes_sample, overwrite = TRUE)
