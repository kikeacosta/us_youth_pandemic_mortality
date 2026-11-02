# Baseline (expected) deaths and excess deaths, 2020-2023.
#
# Baseline: quasi-Poisson GLM, log-linear in year with log(population) offset,
# fitted to 2010-2019 and projected to 2020-2023 (est_baseline_pi, script 00).
# Three series are estimated:
#   (a) each state, ages 0-24 combined          -> excess_states_age_u25.rds
#   (b) each state x age group (0-1 ... 20-24)   -> baselines_states_sex_age.rds
#   (c) the US by age group                      -> excess_age.rds
# Columns: dts observed, bsn baseline, lc/uc 95% CI of the baseline,
#          lp/up 95% bootstrapped prediction interval, exc = dts - bsn, psc = dts/bsn
#
# Input : data_inter/ac_deaths_by_state_sex_age.rds, data_inter/ac_deaths_by_sex_age.rds
# Output: see above

source("code/00_functions.R")

# the prediction intervals are simulated; fix the seed so lp/up are reproducible
set.seed(2024)

add_excess <- function(dt){
  dt %>%
    select(-t, -w) %>%
    mutate(lp = ifelse(lp < 0, 0, lp),
           up = ifelse(up < 0, 0, up),
           psc = dts / bsn,
           psc_up = dts / lp,
           psc_lp = dts / up) %>%
    filter(year >= 2020) %>%
    mutate(exc = dts - bsn,
           exc_r = 1e5 * exc / exposure)
}

dt <- read_rds("data_inter/ac_deaths_by_state_sex_age.rds")

# (b) state x age, both sexes ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt2 <-
  dt %>%
  filter(sex == "t") %>%
  rename(exposure = pop,
         dts = dx)

dt3 <-
  dt2 %>%
  group_by(state, sex, age) %>%
  do(est_baseline_pi(chunk = .data)) %>%
  ungroup()

dt4 <-
  dt3 %>%
  select(-t, -w) %>%
  mutate(lp = ifelse(lp < 0, 0, lp),
         up = ifelse(up < 0, 0, up),
         psc = dts / bsn,
         psc_up = dts / lp,
         psc_lp = dts / up)

write_rds(dt4, "data_inter/baselines_states_sex_age.rds")

# (a) state, ages 0-24 combined ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yth <-
  dt %>%
  filter(sex == "t") %>%
  reframe(dx = sum(dx),
          pop = sum(pop),
          .by = c(state, year)) %>%
  rename(exposure = pop,
         dts = dx)

yth_exc <-
  yth %>%
  group_by(state) %>%
  do(est_baseline_pi(chunk = .data)) %>%
  ungroup() %>%
  add_excess()

write_rds(yth_exc, "data_inter/excess_states_age_u25.rds")

# (c) US by age ====
# ~~~~~~~~~~~~~~~~~~
us <- read_rds("data_inter/ac_deaths_by_sex_age.rds")

us2 <-
  us %>%
  filter(sex == "t") %>%
  rename(exposure = pop,
         dts = dx)

us_exc <-
  us2 %>%
  group_by(sex, age) %>%
  do(est_baseline_pi(chunk = .data)) %>%
  ungroup() %>%
  add_excess()

write_rds(us_exc, "data_inter/excess_age.rds")

# excess deaths by age and year, US (printed for a quick check)
us_exc %>%
  filter(age <= 20) %>%
  select(year, age, exc) %>%
  mutate(exc = round(exc)) %>%
  spread(year, exc) %>%
  print()
