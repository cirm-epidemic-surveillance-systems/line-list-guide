## Estimation via primarycensored

library(primarycensored)
library(cmdstanr)



pcd_model <- pcd_cmdstan_model(cpp_options = list(stan_threads = FALSE))



get_dist_estimates_pcd <- function(delay_data) {
  pcd_data <- pcd_as_stan_data(
    delay_data,
    delay = "observed_delay",
    delay_upper = "observed_delay_upper",
    relative_obs_time = "D",
    pwindow = "pwindow",
    dist_id = pcd_stan_dist_id("lognormal", "delay"),
    primary_id = pcd_stan_dist_id("uniform", "primary"),
    param_bounds = list(lower = c(-Inf, 0), upper = c(Inf, Inf)),
    primary_param_bounds = list(lower = numeric(0), upper = numeric(0)),
    priors = list(location = c(1, 0.5), scale = c(1, 0.5)),
    primary_priors = list(location = numeric(0), scale = numeric(0)),
    use_reduce_sum = FALSE # use within chain parallelisation
  )

  pcd_fit <- pcd_model$sample(
    data = pcd_data,
    chains = 4,
    parallel_chains = 2, # Run 2 chains in parallel
    threads_per_chain = 2, # Use 2 cores per chain
    refresh = ifelse(interactive(), 50, 0),
    show_messages = interactive()
  )

  return(pcd_fit)
}



get_delay_counts <- function(df, max_delay) {
  df |>
    filter(delay >= 0) |>
    mutate(delay = as.numeric(delay)) |>
    group_by(delay) |>
    summarise(n = n()) |>
    ungroup() |>
    transmute(
      ll_source = unique(df$ll_source),
      observed_delay = delay,
      observed_delay_upper = delay + 1,
      pwindow = 1,
      D = max_delay,
      n = n)
}


# Delay in admission counts
delay_admission_counts <- delay_admission |>
  group_split(ll_source) |>
  lapply(FUN = get_delay_counts, max_delay = 30)

names(delay_admission_counts) <- sapply(
  delay_admission_counts, function(x) x$ll_source[1]
  )

delay_admission_pcd <- delay_admission_counts |>
  lapply(FUN = get_dist_estimates_pcd)

delay_admission_estimates <- delay_admission_pcd |>
  lapply(FUN = function(fit) {
    fit$summary(c("params[1]", "params[2]")) |>
      mutate(variable = c("meanlog", "sdlog"))
  })

delay_admission_estimates_natural <- delay_admission_pcd |>
  lapply(FUN = function(fit) {
    a <- posterior::as_draws_rvars(fit$draws("params"))
    posterior::summarise_draws(list(
      mean = exp(a$params[[1]] + a$params[[2]]^2 / 2),
      sd = sqrt(exp(2 * a$params[[1]] + 2 * a$params[[2]]^2) - exp(2 * a$params[[1]] + a$params[[2]]^2)
      )))
  })

# Delay in death counts
delay_death_counts <- delay_death |>
  group_split(ll_source) |>
  lapply(FUN = get_delay_counts, max_delay = 90)

names(delay_death_counts) <- sapply(delay_death_counts, function(x) x$ll_source[1])

delay_death_pcd <- delay_death_counts |>
  lapply(FUN = get_dist_estimates_pcd)

delay_death_estimates <- delay_death_pcd |>
  lapply(FUN = function(fit) {
    fit$summary(c("params[1]", "params[2]")) |>
      mutate(variable = c("meanlog", "sdlog"))
  })

delay_death_estimates_natural <- delay_death_pcd |>
  lapply(FUN = function(fit) {
    a <- posterior::as_draws_rvars(fit$draws("params"))
    posterior::summarise_draws(list(
      mean = exp(a$params[[1]] + a$params[[2]]^2 / 2),
      sd = sqrt(exp(2 * a$params[[1]] + 2 * a$params[[2]]^2) - exp(2 * a$params[[1]] + a$params[[2]]^2)
      )))
  })
