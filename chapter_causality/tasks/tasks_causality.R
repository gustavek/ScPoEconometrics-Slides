### Task 1 ----
15*11/40 + 25*13/40 + 50 * 16/40
15*29/40 + 35*9/40 + 50*2/40

### Task 2 ----

# Question 1
toy_data = read.csv("https://www.dropbox.com/s/a4u67nmu9gnl94y/toy_data.csv?dl=1")

# Question 2
library(tidyverse)
toy_data <- toy_data %>%
    mutate(
           Y_i = D_i*Y1_i + (1 - D_i)*Y0_i,
           delta_i = Y1_i - Y0_i)

# Question 3
ATE = mean(toy_data$delta_i)
ATE

SDO = mean(toy_data[toy_data$D_i == 1, "Y_i"]) - mean(toy_data[toy_data$D_i == 0, "Y_i"])
SDO

bias = abs(ATE - SDO)
bias

bias/ATE*100

# Question 4
toy_data <- toy_data %>%
    mutate(
        Y_i_random = D_i_random*Y1_i + (1 - D_i_random)*Y0_i)

SDO_random = mean(toy_data[toy_data$D_i_random == 1, "Y_i_random"]) - mean(toy_data[toy_data$D_i_random == 0, "Y_i_random"])
SDO_random

bias_random = abs(ATE - SDO_random)
bias_random

bias_random/ATE

# Question 5
selection_bias = mean(toy_data[toy_data$D_i==1, "Y0_i"]) - mean(toy_data[toy_data$D_i==0, "Y0_i"]) 

het_trt_effect_bias = (1 - sum(toy_data$D_i == 1) / nrow(toy_data)) *(mean(toy_data[toy_data$D_i == 1, "delta_i"]) - mean(toy_data[toy_data$D_i == 0, "delta_i"]))

(SDO == ATE + selection_bias + het_trt_effect_bias)


### Task 3 ----

# Question 1
star_df <- read.csv("https://www.dropbox.com/s/bf1fog8yasw3wjj/star_data.csv?dl=1")

# Question 2
library(AER)
?AER::STAR

# Question 3
# unit of observation: student-grade

# Question 4
# data is in long format

# Question 5
# attrition

# Question 6
star_df <- star_df[complete.cases(star_df),]

# Question 7
library(tidyverse)
star_df %>%
    group_by(grade, star) %>%
    summarise(
        share_female = mean(gender == "female") * 100,
        share_african_american = mean(ethnicity == "afam") * 100,
        share_free_lunch = mean(lunch == "free") * 100
    )

### Task 4 ----

# Question 1
star_df_1_small <- star_df %>%
    filter(star %in% c("small","regular") & grade == "1")

# Question 2
star_df_1_small <- star_df_1_small %>%
    mutate(treatment = (star == "small"))
table(star_df_1_small$treatment)

# Question 3
mean_small = mean(star_df_1_small[star_df_1_small$treatment == 1, "math"])
mean_small

mean_regular = mean(star_df_1_small[star_df_1_small$treatment == 0, "math"])
mean_regular

ATE = mean_small - mean_regular
ATE

# Question 4
lm(math ~ treatment, star_df_1_small)
