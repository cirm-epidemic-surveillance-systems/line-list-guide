library("simulist")
library("readr")
library("here")
library("data.table")

set.seed(1234)

ll <- sim_linelist(
  outbreak_size = c(1000, 1e+5),
  onset_to_hosp = function(x) stats::rlnorm(n = x, meanlog = 1.5, sdlog = 0.5),
  onset_to_death = function(x) stats::rlnorm(n = x, meanlog = 2.5, sdlog = 0.5),
  reporting_delay = \(x) rgamma(n = x, shape = 3, scale = 1)
)

# simulate asymptomatic cases
ll <- setDT(ll)
ll[, asymptomatic := FALSE]
ll[outcome == "recovered", asymptomatic := sample(c(TRUE, FALSE), size = .N, replace = TRUE, prob = c(0.4, 0.6))]
ll[asymptomatic == TRUE, date_onset := NA]
ll[asymptomatic == TRUE & !is.na(date_admission), date_reporting := date_admission + rpois(.N, lambda = 14) - 14]

# simulate mild and severe symptom onset dates
ll <- setDT(ll)
ll[!is.na(date_admission), date_onset_mild := date_onset - rpois(.N, lambda = 2)]
ll[!is.na(date_admission), date_onset_severe := date_onset]
ll[is.na(date_admission), date_onset_mild := date_onset]
ll[is.na(date_admission), date_onset_severe := NA]

write_csv(ll, here("data-raw", "clean_linelist.csv"))
