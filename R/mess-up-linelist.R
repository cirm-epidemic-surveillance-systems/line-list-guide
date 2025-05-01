##' Imputes missing onset dates
##'
##' Sets missing onset dates to the date of reporting.
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
impute_missing_onset <- function(ll) {
  ll |>
    mutate(date_onset = if_else(
      is.na(date_onset),
      date_reporting,
      date_onset
    ))
}

##' Adds missing onset dates
##'
##' @param prop Proportion of genuine missingness to add.
##' @inheritParams impute_missing_onset
add_missing <- function(ll, prop = 0.2) {
  ll |>
    mutate(
      date_onset = if_else(
        runif(n()) <= prop,
        as.Date(NA_character_),
        date_onset
      )
    )
}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
recall_bias <- function(ll) {

}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
date_noise <- function(ll) {

}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
round_dates <- function(ll) {

}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
change_reporting_scheme <- function(ll) {

}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
inconsistent_symptom_definitions <- function(ll) {

}

##' .. content for \description{} (no empty lines) ..
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
vague_symptom_onset <- function(ll) {

}
