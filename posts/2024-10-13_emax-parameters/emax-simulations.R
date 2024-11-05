
# exact same packages in quarto doc, even though not all are used
library(rstanemax)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)

# the emax function
emax_fn <- function(exposure, emax, ec50, e0, gamma = 1, ...) {
  e0 + emax * (exposure ^ gamma) / (ec50 ^ gamma + exposure ^ gamma)
}

# simulated distribution of the exposure metric is a slightly truncated
# log-normal distribution, because I refuse to simulate a compartmental
# model in a blog post
generate_exposure <- function(dose, n, meanlog = 4, sdlog = 0.5) {
  dose * stats::qlnorm(
    p = stats::runif(n, min = .01, max = .99),
    meanlog = meanlog,
    sdlog = sdlog
  )
}

# generate a "design" in the regression sense of the term (i.e., the covariate
# values are part of the design, even though they are uncontrolled outcomes of
# the study)
generate_design <- function(dose = c(100, 50), n = c(100, 100)) {
  purrr::map2(dose, n, \(x, y) {
    tibble::tibble(
      dose = x,
      exposure = generate_exposure(x, y)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(condition = factor(paste(dose, "mg")))
}

# convenience function to create a list of emax model parameters in the expected
# format, supplying default values if the user doesn't
emax_parameters <- function(emax  = 10,
                            ec50  = 4000,
                            e0    = 10,
                            gamma = 1,
                            sigma = .6) {
  list(
    emax = emax,
    ec50 = ec50,
    e0 = e0,
    gamma = gamma,
    sigma = sigma
  )
}

# given a vector of exposures taken from the design matrix, and a list of emax
# model parameters, construct a tibble containing the "observed" exposures,
# the "true" value of the emax function at those parameters/exposures, and
# the "observed" response in the simulated data set
generate_emax_data <- function(exposure, par = list()) {
  par <- do.call(emax_parameters, args = par)
  n <- length(exposure)
  tibble(
    exposure = exposure,
    emax_val = emax_fn(
      exposure,
      emax = par$emax,
      ec50 = par$ec50,
      e0 = par$e0,
      gamma = par$gamma
    ),
    response = emax_val + rnorm(n, 0, par$sigma)
  )
}

# convenience function to specify emax model priors for the simulation; this is
# written assuming rstanemax will be used, which assumes iid normal priors on
# all parameters; because this simulation is designed to look at biases that
# emerge from the likelihood/design, the prior is always rigged so as to be
# centred on the true mean
emax_priors <- function(par) {
  list(
    e0 = c(par$e0, 100),
    ec50 = c(par$ec50, 1000),
    emax = c(par$emax, 100),
    sigma = c(par$sigma, 1)
  )
}

# wrapper function to run a single simulation, note dots are passed down from
# rstanemax::stan_emax() to rstan::sampling in this context
simulate_once <- function(seed,
                          dose = c(100, 50),
                          n = c(100, 100),
                          par = emax_parameters(),
                          priors = emax_priors(par),
                          ...) {
  set.seed(seed)
  dsg <- generate_design(dose = dose, n = n)
  dat <- generate_emax_data(dsg$exposure, par = par)
  mod <- stan_emax(
    formula = response ~ exposure,
    data = dat,
    priors = priors,
    ...
  )
  smp <- extract_param(mod)
  est <- smp |>
    select(-mcmcid) |>
    summarise(
      est_emax = mean(emax),
      est_ec50 = mean(ec50),
      est_e0 = mean(e0),
      est_gamma = mean(gamma),
      est_sigma = mean(sigma)
    ) |>
    mutate(
      src_emax = par$emax,
      src_ec50 = par$ec50,
      src_e0 = par$e0,
      src_gamma = par$gamma,
      src_sigma = par$sigma,
      exp_05 = quantile(dat$exposure, .05),
      exp_25 = quantile(dat$exposure, .25),
      exp_50 = quantile(dat$exposure, .50),
      exp_75 = quantile(dat$exposure, .75),
      exp_95 = quantile(dat$exposure, .95),
      rsp_05 = quantile(dat$response, .05),
      rsp_25 = quantile(dat$response, .25),
      rsp_50 = quantile(dat$response, .50),
      rsp_75 = quantile(dat$response, .75),
      rsp_95 = quantile(dat$response, .95),
      seed = seed
    )
  est
}

# wrapper function to repeat the simulations with k iterations
simulate_many <- function(k = 5,
                          seed = 1:k,
                          dose = c(100, 50),
                          n = c(100, 100),
                          par = emax_parameters(),
                          priors = emax_priors(par),
                          file = NULL,
                          ...) {

  ests <- list()
  for (i in 1:k) {
    ests[[i]] <- simulate_once(
      seed = seed[i],
      dose = dose,
      n = n,
      par = par,
      priors = priors,
      ...
    )
  }
  ests <- bind_rows(ests)
  if (!is.null(file)) readr::write_csv(ests, file)
  ests
}


# paths
post <- "2024-10-13_emax-parameters"
csv_dir <- here::here("posts", post, "csv")

# toggles
vary_ec50 <- TRUE

# run the simulation that varies ec50
if (vary_ec50) {

  ec50 <- c(seq(1500, 4000, 500), seq(5000, 10000, 1000))
  ests <- list()
  for(i in seq_along(ec50)) {
    ests[[i]] <- simulate_many(
      k = 1000,
      par = emax_parameters(ec50 = ec50[i])
    )
  }
  ests <- bind_rows(ests)
  readr::write_csv(ests, fs::path(csv_dir, "vary-ec50.csv"))
}



