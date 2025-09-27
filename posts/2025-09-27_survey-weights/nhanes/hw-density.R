
library(fs)
library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(tibble)
library(gamlss)
library(rprojroot)

proj_root  <- find_root_file(criterion = has_file(".here"))
local_dir  <- path(proj_root, "nhanes")
data_dir   <- path(local_dir, "data")
output_dir <- path(local_dir, "output")

ht_m   <- readRDS(path(output_dir, "ht-m-v03.rds"))
ht_f   <- readRDS(path(output_dir, "ht-f-v03.rds"))
wt_htm <- readRDS(path(output_dir, "wt-htm-v03.rds"))
wt_htf <- readRDS(path(output_dir, "wt-htf-v03.rds"))

nhanes_m <- read_csv(path(output_dir, "nhanes-m-v03.csv"))
nhanes_f <- read_csv(path(output_dir, "nhanes-f-v03.csv"))

get_pars <- function(data, model) {
  pars <- tibble(
    mu    = predict(model, newdata = data, type = "response", what = "mu"),
    sigma = predict(model, newdata = data, type = "response", what = "sigma"),
    nu    = predict(model, newdata = data, type = "response", what = "nu"),
    tau   = predict(model, newdata = data, type = "response", what = "tau"),
  )
  bind_cols(data, pars)
}

ht_cases <- tibble(age_mn  = seq(6, 25*12, by = 3))
wt_cases <- expand_grid(
  age_mn  = seq(6, 25*12, by = 3),
  height_cm = seq(50, 200, by = 1)
)

ht_m_pars <- get_pars(ht_cases, ht_m)
ht_f_pars <- get_pars(ht_cases, ht_f)
wt_m_pars <- get_pars(wt_cases, wt_htm) |> mutate(mu = if_else(mu < 0, 0, mu))
wt_f_pars <- get_pars(wt_cases, wt_htf) |> mutate(mu = if_else(mu < 0, 0, mu))

ht_density <- bind_rows(
  male = ht_m_pars |>
    pmap_dfr(\(age_mn, mu, sigma, nu, tau) {
      tibble(
        age_mn = age_mn,
        height_cm = seq(50, 200, by = 1),
        h_density = dBCPE(height_cm, mu, sigma, nu, tau)
      )
    }),
  female = ht_f_pars |>
    pmap_dfr(\(age_mn, mu, sigma, nu, tau) {
      tibble(
        age_mn = age_mn,
        height_cm = seq(50, 200, by = 1),
        h_density = dBCPE(height_cm, mu, sigma, nu, tau)
      )
    }),
  .id = "sex_fct"
)

wt_density <- bind_rows(
  male = wt_m_pars |>
    pmap_dfr(\(age_mn, height_cm, mu, sigma, nu, tau) {
      tibble(
        age_mn = age_mn,
        height_cm = height_cm,
        weight_kg = seq(1, 150, by = 1),
        w_density = dBCPE(weight_kg, mu, sigma, nu, tau)
      )
    }),
  female = wt_f_pars |>
    pmap_dfr(\(age_mn, height_cm, mu, sigma, nu, tau) {
      tibble(
        age_mn = age_mn,
        height_cm = height_cm,
        weight_kg = seq(1, 150, by = 1),
        w_density = dBCPE(weight_kg, mu, sigma, nu, tau)
      )
    }),
  .id = "sex_fct"
)

hw_density <- wt_density |> 
  left_join(ht_density, by = c("sex_fct", "age_mn", "height_cm")) |>
  mutate(
    density = h_density * w_density,
    sex_fct = factor(sex_fct, labels = c("male", "female"))
  ) |>
  select(-h_density, -w_density)

write_csv(hw_density, path(output_dir, "hw-density.csv"))
