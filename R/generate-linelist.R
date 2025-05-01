library("simulist")
library("readr")
library("here")

set.seed(1234)

ll <- sim_linelist(
  outbreak_size = c(1000, 1e+5),
  onset_to_hosp = function(x) stats::rlnorm(n = x, meanlog = 1.5, sdlog = 0.5),
  onset_to_death = function(x) stats::rlnorm(n = x, meanlog = 2.5, sdlog = 0.5),
  reporting_delay = \(x) rgamma(n = x, shape = 3, scale = 1)
)
write_csv(ll, here("data-raw", "clean_linelist.csv"))
