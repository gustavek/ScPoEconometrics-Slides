library(AER)
library(tidyverse)

data("STAR", package = "AER")

star <- STAR

star_long <- star %>%
    pivot_longer(
        cols = -c(gender, ethnicity, birth),
        names_to = c(".value", "grade"),
        names_pattern = "(.+)(k|1|2|3)$"
    )

















fwrite(star_long, "~/Desktop/star_data.csv", na = NA)

# levels(star_long$grade) <- c("stark","star1","star2","star3")

star_long <- star_long[complete.cases(star_long),]

star_long %>%
    group_by(grade, star) %>%
    summarise(
        math_mean = mean(math),
        read_mean = mean(read)
    )

# Nice difference table
diff_table = data.frame(
    grade = rep(c("1","2","3","k"), each = 2),
    test = rep(c("math","read"), times = 4),
    star_long  %>%
        pivot_longer(cols = c("math","read"), names_to = "test", values_to = "score") %>%
        filter(star == "regular") %>%
        group_by(grade, test) %>%
        summarise(mean_regular = round(mean(score), 3)) %>%
        ungroup() %>%
        select(mean_regular),
    star_long %>%
        pivot_longer(cols = c("math","read"), names_to = "test", values_to = "score") %>%
        filter(star == "small") %>% group_by(grade, test) %>%
        summarise(mean_small = round(mean(score), 3)) %>%
        ungroup() %>%
        select(mean_small),
    star_long %>%
        pivot_longer(cols = c("math","read"), names_to = "test", values_to = "score") %>%
        filter(star == "regular+aide") %>%
        group_by(grade, test) %>%
        summarise(mean_regular_aide = round(mean(score), 3)) %>%
        ungroup() %>%
        select(mean_regular_aide)) %>%
    mutate(
        diff_small_regular = round(mean_small - mean_regular, 3),
        diff_regular_aide_regular = round(mean_regular_aide - mean_regular, 3)
    ) %>%
    arrange(factor(grade, levels = c("k","1","2","3")))
diff_table

# for simplicity let's just look at small and regular (control)
summary(lm(math ~ star,
         data = star_long %>%
             filter(star %in% c("regular", "small") & grade == "1")))

summary(lm(math ~ star,
         data = star_long %>%
             filter(star %in% c("regular", "regular+aide") & grade == "1")))

summary(lm(math ~ star,
           data = star_long %>%
               filter(grade == "1")))

# histograms
star_long %>%
    filter(star %in% c("regular","small")) %>%
    pivot_longer(cols = c("math","read"), names_to = "test", values_to = "score") %>%
    ggplot(aes(x = star, y = score)) +
        geom_boxplot() +
        facet_wrap(grade ~ test, ncol = 2) +
        theme_bw(base_size = 14)

star_long %>%
    filter(star == "small" & grade == "1") %>%
    summarise(mean_math = mean(math))
star_long %>%
    filter(star == "regular" & grade == "1") %>%
    summarise(mean_math = mean(math))




z[,score := rowMeans(.SD)*100, .SDcols = c("perc_read","perc_math")]  # take average of scores
# and multiply by 100, so it's comparable to Krueger

