pasta <- data.frame(
    pasta_ID = sample(1:713, 713, replace = FALSE),
    color = c(rep("green", 352),rep("yellow", 150),rep("red", 211))
)

pasta <- pasta %>%
    arrange(pasta_ID)


library(data.table)
fwrite(pasta, file = "pasta.csv")

pasta_2 <- read.csv("https://www.dropbox.com/s/qpjsk0rfgc0gx80/pasta.csv?dl=1")
