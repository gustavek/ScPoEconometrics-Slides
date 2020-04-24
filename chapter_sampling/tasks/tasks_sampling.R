# Task 1 ----

# Q1
pasta <- data.frame(prop_green = c(0.7,0.7,0.5,0.5,0.3,0.5,0.4,0.45,0.55,0.4,0.35,0.45,0.45,0.7,0.55,0.5,0.35,0.65))

#Q2
library(tidyverse)
pasta %>%
    ggplot(aes(x = prop_green)) +
    geom_histogram(boundary = 0.325, binwidth = 0.05, color = "white", fill = "darkgreen")

#Q3
# It kind of starts looking a bit like a normal distribution...


# Task 2 ----

#Q1
# Would take ages.

#Q2
pasta <- read.csv("https://www.dropbox.com/s/qpjsk0rfgc0gx80/pasta.csv?dl=1")

#Q3
library(moderndive)
virtual_samples <- pasta %>% 
    rep_sample_n(size = 50, reps = 1000)

#Q4
virtual_prop_green <- virtual_samples %>% 
    group_by(replicate) %>% 
    summarize(
        num_green = sum(color == "green"),
        sample_n = n()) %>% 
    mutate(prop_green = num_green / sample_n)

#Q5
virtual_prop_green %>% ggplot(
    aes(x = prop_green)) +
    geom_histogram(
        binwidth = 0.02,
        boundary = 0.41,
        color = "white",
        fill = "darkgreen") +
    labs(x = "Proportion of green pasta in sample",
         y = "Frequency",
         title = "Distribution of 1000 samples of size 50") +
    theme_bw(base_size = 14)

#Q6
# The distribution looks very close to a normal distribution.
# Around 0.5
# Looks significantly closer to a normal distribution

#Q7
# Extremely unlikely