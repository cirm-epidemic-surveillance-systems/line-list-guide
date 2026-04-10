##' Imputes missing onset dates
##'
##' Sets missing onset dates to the date of admission
##'
##' @param ll Line list data frame
##' @return A data frame with the same columns as the input line list
impute_missing_onset <- function(ll) {
  ll |>
    mutate(date_onset = if_else(
      is.na(date_onset),
      date_admission,
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
        threshold_days < as.integer(date_admission - date_onset),
        as.Date(NA_character_),
        date_onset
      )
    )
}

##' Add recall-biased missingness to onset dates
##'
##' Introduce missingness in `date_onset` such that older onset dates are less
##' likely to be retained. The probability that an onset date is observed follows
##' a logistic curve as a function of the delay between onset and admission.
##'
##' The curve is parameterised using two interpretable anchors:
##' - at `day_90_observed`, approximately 90% of onset dates are still observed
##' - at `day_50_observed`, approximately 50% of onset dates are still observed
##'
##' @param ll Line list data frame containing `date_onset` and `date_admission`
##'   as Date columns.
##' @param day_90_observed Delay in days at which approximately 90% of onset
##'   dates are still observed.
##' @param day_50_observed Delay in days at which approximately 50% of onset
##'   dates are still observed.
##' @param rng_seed Optional integer for reproducible random number generation.
##'
##' @return A data frame with the same columns as the input line list, with
##'   recall-biased missingness introduced into `date_onset`.
add_recall_missing <- function(ll,
                               day_90_observed = 7,
                               day_50_observed = 21,
                               rng_seed = NULL) {
  if (!is.null(rng_seed)) {
    set.seed(rng_seed)
  }

  if (day_90_observed >= day_50_observed) {
    stop("day_90_observed must be smaller than day_50_observed.")
  }

  ll |>
    mutate(
      # Delay between the true onset date and the admission date.
      # Larger delays correspond to worse recall.
      delay_recall = as.integer(date_admission - date_onset),

      # Keep missing delays as NA and truncate negative delays at zero.
      delay_recall = if_else(
        is.na(delay_recall),
        NA_integer_,
        pmax(delay_recall, 0L)
      ),

      # Probability that the onset date is still observed.
      # By construction:
      # - p_observed_recall(day_90_observed) ~= 0.9
      # - p_observed_recall(day_50_observed) ~= 0.5
      p_observed_recall = plogis(
        qlogis(0.9) +
          (delay_recall - day_90_observed) *
          (qlogis(0.5) - qlogis(0.9)) /
          (day_50_observed - day_90_observed)
      ),

      # Draw one random number per row to decide whether the onset date is kept.
      u = runif(n()),

      # Remove the onset date if the random draw exceeds the probability that
      # the date is still observed.
      remove_onset = !is.na(p_observed_recall) & u > p_observed_recall,

      # Set onset dates selected for removal to missing.
      date_onset = if_else(
        remove_onset,
        as.Date(NA_character_),
        date_onset
      )
    ) |>
    select(-delay_recall, -p_observed_recall, -u, -remove_onset)
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
date_noise <- function(ll, noise_func = \(n, delay) {
  rpois(n = n, lambda = delay)
}) {
  ll |>
    mutate(
      date_onset = date_admission -
        noise_func(n(), as.integer(date_admission - date_onset))
      )
}

##' Round dates (due to poor recall)
##'
##' @param prop Proportion of cases (more than 4 days since report) that are
##'   rounded
##' @return A data frame with the same columns as the input line list
##' @inheritParams impute_missing_onset
round_dates <- function(ll, prop = 0.4) {
  ll |>
    mutate(
      date_onset = if_else(
        runif(n()) <= prop & as.integer(date_admission - date_onset) > 4,
        date_admission - round(as.integer(date_admission - date_onset) / 7) * 7,
        date_onset
      )
    )
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
