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

## Task 3

# Question 1
# jtools
library(jtools)
grades <- grades %>%
    mutate(avgmath_stand = standardize(avgmath))

# base R
grades$avgmath_stand_2 <- (grades$avgmath - mean(grades$avgmath)) / sd(grades$avgmath)

identical(grades$avgmath_stand, grades$avgmath_stand_2)

# Quesiton 2
reg_full_stand <- lm(avgmath_stand ~ classize + disadvantaged + school_enrollment + female + religious, grades)
reg_full_stand$coefficients

# Question 3
# It's hard to tell because the regressors have different scales but it seems like the most important variable is the religious variable.

# Question 4
grades <- grades %>%
    mutate(
        classize_stand = standardize(classize),
        disadvantaged_stand = standardize(disadvantaged),
        school_enrollment_stand = standardize(school_enrollment),
        female_stand = standardize(female)
        )

# Question 5
reg_full_stand_2 <- lm(avgmath_stand ~ classize_stand + disadvantaged_stand + school_enrollment_stand + female_stand + religious, grades)
reg_full_stand_2$coefficients


## Task 4

# Question 1
library(tidyverse)
library(AER)
data("CPS1985")
cps = CPS1985

# Question 2
?AER::CPS1985

# Question 3
# It doesn't matter since we are analysing hourly wages.

# Question 4
cps <- cps %>%
    mutate(log_wage = log(wage))

# Question 5
reg1 <- lm(log_wage ~ gender + education, cps)
reg1

# Question 6
reg2 <- lm(log_wage ~ gender*education, cps)
reg2

# Question 7
reg3 <- lm(log_wage ~ gender*education + experience + age + ethnicity + region + occupation + sector + union + married, cps)
reg3