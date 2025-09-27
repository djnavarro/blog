
# setup -------------------------------------------------------------------

# directory structure
proj_root  <- rprojroot::find_root_file(criterion = rprojroot::has_file(".here"))
local_dir  <- fs::path(proj_root, "nhanes")
data_dir   <- fs::path(local_dir, "data")
output_dir <- fs::path(local_dir, "output")


# anthropometric functions ------------------------------------------------

# Buyken, A., Hahn, S. & Kroke, A. Differences between recumbent length 
# and stature measurement in groups of 2- and 3-y-old children and its 
# relevance for the use of European body mass index references. 
# Int J Obes 29, 24–28 (2005). https://doi.org/10.1038/sj.ijo.0802738
#
# - primary source finds 0.5 cm difference
# - they cite earlier NHANES data with 0.8 cm difference
# 
# The NHANES data to current release has a mean difference of 1.06 and
# any effects of age/sex are very small. Given restricted range (i.e.
# we only have both measurements from the 2-4yr age brackets, a simple
# mean adjustment is used here)
length_to_height <- function(length_cm, adjust = 1.06) {
  length_cm - adjust
}
height_to_length <- function(height_cm, adjust = 1.06) {
  height_cm + adjust
}

# Du Bois D, Du Bois EF (Jun 1916). "A formula to estimate the approximate
# surface area if height and weight be known". Archives of Internal Medicine
# 17 (6): 863-71. PMID 2520314.
bsa_dubois <- function(height_cm, weight_kg) {
  0.007184 * weight_kg^0.425 * height_cm^0.725
}


# nhanes data -------------------------------------------------------------

set.seed(2345)
message("processing NHANES data")

# all demographics and body measurement files
demo_files <- fs::dir_ls(fs::path(data_dir, "demo"))
bmx_files <- fs::dir_ls(fs::path(data_dir, "bmx"))

# read demographics file (selected variables only)
demos <- demo_files |> 
  purrr::map(\(xx) {
    dd <- haven::read_xpt(xx) 
    if (!exists("RIDEXAGM", where = dd)) dd$RIDEXAGM <- NA_real_
    dd <- dplyr::select(dd, SEQN, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDEXAGM)
    dd
  }) |> 
  dplyr::bind_rows(.id = "file_demo") |> 
  dplyr::mutate(file_demo = fs::path_file(file_demo))

# read body measurements file (selected variables only)
bmxes <- bmx_files |> 
  purrr::map(\(xx) {
    dd <- haven::read_xpt(xx) 
    dd <- dplyr::select(dd, SEQN, BMXWT, BMXHT, BMXRECUM)
    dd
}) |> 
  dplyr::bind_rows(.id = "file_bmx") |> 
  dplyr::mutate(file_bmx = fs::path_file(file_bmx))

# join data sets, retaining only those rows where the
# required body measurements exist
nhanes <- bmxes |>
  dplyr::left_join(demos, by = "SEQN") |>
  dplyr::select(
    id          = SEQN,
    sex_s       = RIAGENDR, # sex/gender at screen (1 = M, 2 = F, . = NA)
    weight_kg_e = BMXWT,    # weight at exam
    height_cm_e = BMXHT,    # standing height at exam
    length_cm_e = BMXRECUM, # recumbent length at exam (0-47 months only)
    age_yr_s    = RIDAGEYR, # natal age at screening (years)
    age_mn_s    = RIDAGEMN, # natal age at screening (months; 0-24 mos only)
    age_mn_e    = RIDEXAGM, # natal age at exam (months; 0-19 years only)
    file_demo,
    file_bmx
  ) |>
  dplyr::mutate(
    sex_num = sex_s - 1, # rescale to 0 = M, 1 = F
    sex_fct = factor(sex_s, levels = 1:2, labels = c("male", "female")),
    age_mn = dplyr::case_when(
      !is.na(age_mn_e) ~ age_mn_e, # use exam months if present
      !is.na(age_mn_s) ~ age_mn_s, # else use survey months
      TRUE ~ (age_yr_s * 12)       # else use age in years
    ),
    age_yr = age_mn / 12,
    weight_kg = weight_kg_e,
    height_cm = dplyr::case_when(
      !is.na(height_cm_e) ~ height_cm_e, # use height if it was measured
      !is.na(length_cm_e) ~ length_to_height(length_cm_e), # or convert length
      TRUE ~ NA_real_, # else missing
    ),
    cohort = dplyr::case_when(
      file_bmx == "BMX.xpt"   & file_demo == "DEMO.xpt"   ~ "1999-2000",
      file_bmx == "BMX_B.xpt" & file_demo == "DEMO_B.xpt" ~ "2001-2002",
      file_bmx == "BMX_C.xpt" & file_demo == "DEMO_C.xpt" ~ "2003-2004",
      file_bmx == "BMX_D.xpt" & file_demo == "DEMO_D.xpt" ~ "2005-2006",
      file_bmx == "BMX_E.xpt" & file_demo == "DEMO_E.xpt" ~ "2007-2008",
      file_bmx == "BMX_F.xpt" & file_demo == "DEMO_F.xpt" ~ "2009-2010",
      file_bmx == "BMX_G.xpt" & file_demo == "DEMO_G.xpt" ~ "2011-2012",
      file_bmx == "BMX_H.xpt" & file_demo == "DEMO_H.xpt" ~ "2013-2014",
      file_bmx == "BMX_I.xpt" & file_demo == "DEMO_I.xpt" ~ "2015-2016",
      file_bmx == "BMX_J.xpt" & file_demo == "DEMO_J.xpt" ~ "2017-2018",
      file_bmx == "P_BMX.xpt" & file_demo == "P_DEMO.xpt" ~ "2017-2020",
      file_bmx == "BMX_L.xpt" & file_demo == "DEMO_L.xpt" ~ "2021-2023",
      TRUE ~ NA
    ),
    is_pandemic = dplyr::case_when(
      file_bmx == "P_BMX.xpt" & file_demo == "P_DEMO.xpt" ~ TRUE,
      TRUE ~ FALSE
    )
  )

# cross-check: the constructed age in years should never be less than the
# original age in years at screening, and not more than 1.25 years above
# (allowing a 3 month tolerance for the screening/exam gap). if any case
# violates this, flag for manual check
flagged <- nhanes |>
  dplyr::mutate(diff_yr = age_yr - age_yr_s) |>
  dplyr::filter(diff_yr < 0 | diff_yr > 1.25)

# throw error if there's a flag
testthat::expect_equal(nrow(flagged), 0)

# cross-check: the case_when should capture all possible source merges; 
# if not something weird has happened
missing_cohort <- sum(is.na(nhanes$cohort))
testthat::expect_equal(missing_cohort, 0)

# retain only the to-be-used columns, and only those cases for which
# age, weight, height, and sex are all present; filter to age < 80
# because NHANES uses "80" to mean "80 and above" so the actual age
# is not known
ok <- function(x) !is.na(x)
nhanes <- nhanes |>
  dplyr::select(id, sex_num, sex_fct, weight_kg, height_cm, age_mn, age_yr, cohort) |>
  dplyr::filter(ok(sex_num), ok(weight_kg), ok(height_cm), ok(age_mn)) |>
  dplyr::filter(age_yr < 80)

# maximum age used in gamlss modelling; this restriction is
# to ensure that the gamlss models are well-behaved over
# the age range of most interest (i.e. pediatric)
age_max_yr <- 40

# subsets used for gamlss training
nhanes_m <- nhanes |> dplyr::filter(sex_fct == "male", age_yr <= age_max_yr)
nhanes_f <- nhanes |> dplyr::filter(sex_fct == "female", age_yr <= age_max_yr)

# write to file
readr::write_csv(nhanes, fs::path(output_dir, "nhanes.csv"))
readr::write_csv(nhanes_m, fs::path(output_dir, "nhanes-m.csv"))
readr::write_csv(nhanes_f, fs::path(output_dir, "nhanes-f.csv"))


# build gamlss ------------------------------------------------------------

library(gamlss)

set.seed(1234)

# rds file names for gamlss objects
ht_m_file   <- "ht-m.rds"
ht_f_file   <- "ht-f.rds"
wt_htm_file <- "wt-htm.rds"
wt_htf_file <- "wt-htf.rds"

# notes on the gamlss construction: by design it mirrors
# the approach in nhanesgamlss, with modest differences.
# p-splines rather than cubic-splines are used to take
# advantage of the fact that pb() is faster than using
# find.hyper() to tune hyperparameters on cs(). the
# covariate model on nu, and tau is simpler in the current
# formulation, but has little effect on the simulations
# since trimming is used at sampling time to avoid
# biologically impossible/implausible values due to the
# fact that BCPE distribution has support on the positive
# reals

opt_control <- gamlss.control(c.crit = .001, n.cyc = 250)

message("fitting GAMLSS model: height ~ age | male")
ht_m <- gamlss(
  formula = height_cm ~ pb(age_mn),
  sigma.formula = ~pb(age_mn),
  nu.formula    = ~1,
  tau.formula   = ~1,
  data = nhanes_m,
  family = BCPE,
  control = opt_control
)

message("fitting GAMLSS model: height ~ age | female")
ht_f <- gamlss(
  height_cm ~ pb(age_mn),
  sigma.formula = ~pb(age_mn),
  nu.formula    = ~1,
  tau.formula   = ~1,
  data = nhanes_f,
  family = BCPE,
  control = opt_control
)

message("fitting GAMLSS model: weight ~ height + age | male")
wt_htm <- gamlss(
  formula = weight_kg ~ pb(age_mn) + height_cm + pb(age_mn):height_cm,
  sigma.formula = ~pb(age_mn),
  nu.formula    = ~1,
  tau.formula   = ~1,
  data = nhanes_m,
  family = gamlss.dist::BCPE,
  control = opt_control
)

message("fitting GAMLSS model: weight ~ height + age | female")
wt_htf <- gamlss(
  formula = weight_kg ~ pb(age_mn) + height_cm + pb(age_mn):height_cm,
  sigma.formula = ~pb(age_mn),
  nu.formula    = ~1,
  tau.formula   = ~1,
  data = nhanes_f,
  family = BCPE,
  control = opt_control
)

# write to RDS
saveRDS(ht_m, file = fs::path(output_dir, ht_m_file))
saveRDS(ht_f, file = fs::path(output_dir, ht_f_file))
saveRDS(wt_htm, file = fs::path(output_dir, wt_htm_file))
saveRDS(wt_htf, file = fs::path(output_dir, wt_htf_file))

