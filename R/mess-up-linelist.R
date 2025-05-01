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
##' @param threshold_days Number of days before the date of reporting that acts
##'   as a threshold, missingness only applies for delays greater than this.
##'   This serves as an implementation of recall bias
##' @inheritParams impute_missing_onset
add_missing <- function(ll, prop = 0.2, threshold_days = 0L) {
  ll |>
    mutate(
      date_onset = if_else(
        runif(n()) <= prop &
        threshold_days < as.integer(date_reporting - date_onset),
        as.Date(NA_character_),
        date_onset
      )
    )
}

##' Add noise to dates (due to poor recall)
##'
##' @param noise_func Function to generate the noise; this should have two
##'   inputs, `n` (the number of random variates to generate) and `delay` (the
##'   observed delay between onset and report in days) and return `n` integers.
##'   By default it applies a normal distribution with the given `delay` as mean
##'   and the square root of the delay as standard deviation.
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
date_noise <- function(ll, noise = \(n, delay) {
    rpois(n = n, lambda = delay)
}) {
  ll |>
    mutate(
      date_onset = date_reporting -
        noise(n(), as.integer(date_reporting - date_onset)))
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
