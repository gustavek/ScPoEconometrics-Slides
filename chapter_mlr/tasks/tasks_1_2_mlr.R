## Task 1 ----

# Question 1
library(haven)
grades <- read_dta("https://www.dropbox.com/s/wwp2cs9f0dubmhr/grade5.dta?dl=1")

# Question 2
lm(avgverb ~ classize, grades)
reg <- lm(avgverb ~ classize + disadvantaged, grades)

# Question 3
reg$coefficients
lm(avgmath ~ classize + disadvantaged, grades)$coefficients
# The "effect" of class size becomes negative for the reading score!

# Question 4
reg_full <- lm(avgverb ~ classize + disadvantaged + school_enrollment + female + religious, grades)
reg_full$coefficients


## Task 2 ----

# Question 1
star_df <- read.csv("https://www.dropbox.com/s/bf1fog8yasw3wjj/star_data.csv?dl=1")
star_df <- star_df[complete.cases(star_df),]
star_df_2_small <- star_df %>%
    filter(grade == "2" & star %in% c("small", "regular"))

# Question 2
star_df_2_small <- star_df_2_small %>%
    mutate(
        small = (star == "small"),
        regular = (star == "regular")
    )
table(star_df_2_small$small)
table(star_df_2_small$regular)

# Question 3
lm(math ~ small, star_df_2_small)
lm(math ~ small, star_df_2_small)$coefficients[1] + lm(math ~ small, star_df_2_small)$coefficients[2]

# Question 4
lm(math ~ regular, star_df_2_small)
lm(math ~ regular, star_df_2_small)$coefficients[1] + lm(math ~ regular, star_df_2_small)$coefficients[2]

# Question 5
lm(math ~ small + regular, star_df_2_small)