# problem: doesn't preserve attributes on other classes inheriting from data.frame
library(dplyr)

atnm <- function(x) names(attributes(x))

attr(iris, "test") <- "pass"

attr(iris, "class") <- c("test", "data.frame")
mutate(iris, new_col = 1) %>% atnm()

attr(iris, "class") <- c("data.frame")
mutate(iris, new_col = 1) %>% atnm()


# solution: use tibbles! for some reason, they force the preservation of attributes
attr(iris, "class") <- c("vbdf", "tbl_df", "data.frame")
mutate(iris, new_col = 1) %>% atnm()
