library(skimr)
library(tidyverse)
library(scales)

df <- read.csv("../line-list-guide/data-raw/clean_linelist.csv")
skim(df)
pct_missing <- 0.2

set.seed(7)
df <- df |> 
  mutate(
    # missing at random
    date_onset_miss_rd = ifelse(runif(n()) <= pct_missing, NA, date_onset),
    # more missing among elderly people
    p_na = rescale(age, to = c(0.05, 0.60)),
    date_onset_miss_age = ifelse(runif(n()) <= p_na, NA, date_onset)
  )

write.csv(df, "../line-list-guide/data-raw/missing_linelist.csv")
