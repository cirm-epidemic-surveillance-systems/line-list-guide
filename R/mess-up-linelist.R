impute_missing <- function(ll) {
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
