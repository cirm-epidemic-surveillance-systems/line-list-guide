library(skimr)
library(tidyverse)
library(scales)

df <- read.csv("../line-list-guide/data-raw/clean_linelist.csv")
skim(df)

set.seed(7)
df <- df |> 
  mutate(
    # missing at random
    date_onset_miss_rd = ifelse(runif(n()) <= 0.2, NA, date_onset),
    # impute by date reporting
    date_onset_miss_rd_impute_50 = ifelse(is.na(date_onset_miss_rd) & runif(n()) <= 0.5, date_reporting, date_onset_miss_rd),
    date_onset_miss_rd_impute_25 = ifelse(is.na(date_onset_miss_rd) & runif(n()) <= 0.25, date_reporting, date_onset_miss_rd),
    date_onset_miss_rd_impute_75 = ifelse(is.na(date_onset_miss_rd) & runif(n()) <= 0.75, date_reporting, date_onset_miss_rd),
    date_onset_miss_rd_impute_100 = ifelse(is.na(date_onset_miss_rd), date_reporting, date_onset_miss_rd),
    # more missing among elderly people
    p_na = rescale(age, to = c(0.05, 0.3)),
    date_onset_miss_age = ifelse(runif(n()) <= p_na, NA, date_onset),
    # impute by date reporting
    date_onset_miss_age_impute_50 = ifelse(is.na(date_onset_miss_age) & runif(n()) <= 0.5, date_reporting, date_onset_miss_age),
    date_onset_miss_age_impute_25 = ifelse(is.na(date_onset_miss_age) & runif(n()) <= 0.25, date_reporting, date_onset_miss_age),
    date_onset_miss_age_impute_75 = ifelse(is.na(date_onset_miss_age) & runif(n()) <= 0.75, date_reporting, date_onset_miss_age),
    date_onset_miss_age_impute_100 = ifelse(is.na(date_onset_miss_age), date_reporting, date_onset_miss_age),
  )

tmp <- df |> 
  filter(!is.na(date_onset_miss_rd), !is.na(date_reporting))

# lm(data = tmp, formula = date_onset_miss_rd ~ date_reporting)

write.csv(df, "../line-list-guide/data-raw/missing_linelist.csv")
