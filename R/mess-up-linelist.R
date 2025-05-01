##' Imputes missing onset dates
##'
##' Sets missing onset dates to the date of reporting.
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
##' @examples
impute_missing_onset <- function(ll) {
  ll |>
    mutate(date_onset = if_else(
      is.na(date_onset),
      date_reporting,
      date_onset
    ))
}

add_missing <- function(ll) {

}

recall_bias <- function(ll) {

}

date_noise <- function(ll) {

}

round_dates <- function(ll) {

}

change_reporting_scheme <- function(ll) {

}

inconsistent_symptom_definitions <- function(ll) {

}

vague_symptom_onset <- function(ll) {

}
