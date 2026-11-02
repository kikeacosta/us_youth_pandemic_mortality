# CYP excess in the context of each state's overall excess, 2020-2023.
#
# P-scores (observed / expected - 1, in %) by state for three populations:
#   - CYP, ages 0-24, directly age-standardised to the US 0-24 population
#     (six age groups, US structure of 2020) from the state x age baselines
#   - adults, ages 25+ (one group, so crude = standardised)
#   - all ages, standardised over the seven groups (0-1 ... 20-24, 25+)
# plus the CYP share of the state's excess deaths, of its expected deaths and of
# its COVID-19 deaths and of its indirect excess (excess - COVID-19 deaths),
# crude and age-standardised, and the CYP p-score split into a direct
# (COVID-19 deaths / expected) and an indirect component.
# The maps drawn from these quantities are in the alt_fig2_* scripts.
#
# Table S1: all quantities by state and year
#
# Input : data_inter/baselines_states_sex_age.rds, excess_states_broad_age.rds (script 04)
#         data_inter/pop_us_by_sex_age_2010_2023.rds (script 03)
#         data_inter/c19_deaths_by_state_and_age.rds (script 01)
# Output: data_inter/cyp_vs_all_ages_by_state.rds (one row per state x year, US included)
#         tables/tableS1_pscores_cyp_share_by_state.csv

source("code/00_functions.R")

# State x age series, ages 0-24 and 25+ ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cyp <-
  read_rds("data_inter/baselines_states_sex_age.rds") %>%
  filter(year >= 2020) %>%
  mutate(age_grp = age_labs[as.character(age)]) %>%
  select(state, year, age_grp, dts, bsn, lc, uc, pop = exposure)

broad <- read_rds("data_inter/excess_states_broad_age.rds")

adults <-
  broad %>%
  filter(age_grp == "25+") %>%
  select(state, year, age_grp, dts, bsn, lc, uc, pop = exposure)

all_fit <-
  broad %>%
  filter(age_grp == "all") %>%
  transmute(state, year, exc_all_fit = dts - bsn, bsn_all_fit = bsn,
            all_exc_sig = (dts - uc) > 0 | (dts - lc) < 0)

by_age <- bind_rows(cyp, adults)

stopifnot(all(count(by_age, state, year)$n == 7))

# Standard population: US 2020, seven groups ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
us_pop <-
  read_rds("data_inter/pop_us_by_sex_age_2010_2023.rds") %>%
  filter(year == 2020) %>%
  reframe(pop = sum(pop), .by = age)          # both sexes

std_pop <-
  bind_rows(us_pop %>%
              filter(age <= 20) %>%
              transmute(age_grp = age_labs[as.character(age)], pop),
            us_pop %>%
              filter(age >= 25) %>%
              summarise(age_grp = "25+", pop = sum(pop)))

stopifnot(nrow(std_pop) == 7)

w_cyp <- std_pop %>% filter(age_grp != "25+") %>% transmute(age_grp, w = pop / sum(pop))
w_all <- std_pop %>% transmute(age_grp, w = pop / sum(pop))

# directly standardised observed and expected rates -> p-score and its 95% CI
# (the CI of the baseline carried through the standardisation)
standardise <- function(d, w){
  d %>%
    inner_join(w, by = "age_grp") %>%
    group_by(state, year) %>%
    summarise(obs_std    = sum(w * dts / pop),
              exp_std    = sum(w * bsn / pop),
              exp_std_lc = sum(w * lc / pop),
              exp_std_uc = sum(w * uc / pop),
              dts = sum(dts), bsn = sum(bsn),
              .groups = "drop") %>%
    mutate(pscore       = 100 * (obs_std / exp_std - 1),
           pscore_lb    = 100 * (obs_std / exp_std_uc - 1),
           pscore_ub    = 100 * (obs_std / exp_std_lc - 1),
           pscore_crude = 100 * (dts / bsn - 1),
           exc          = dts - bsn)
}

p_cyp <- standardise(cyp,    w_cyp)
p_adu <- standardise(adults, tibble(age_grp = "25+", w = 1))
p_all <- standardise(by_age, w_all)

# P-scores by state and year ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the CYP share uses the all-age excess from the all-age fit (coherent with the
# state totals of Table A2, which also fit the aggregate rather than summing ages)
res <-
  p_cyp %>%
  select(state, year, cyp_pscore = pscore, cyp_pscore_lb = pscore_lb, cyp_pscore_ub = pscore_ub,
         cyp_pscore_crude = pscore_crude, cyp_exc = exc, cyp_bsn = bsn) %>%
  inner_join(p_adu %>%
               select(state, year, adult_pscore = pscore, adult_pscore_lb = pscore_lb,
                      adult_pscore_ub = pscore_ub, adult_exc = exc),
             by = c("state", "year")) %>%
  inner_join(p_all %>%
               select(state, year, all_pscore = pscore, all_pscore_lb = pscore_lb,
                      all_pscore_ub = pscore_ub, all_pscore_crude = pscore_crude),
             by = c("state", "year")) %>%
  inner_join(all_fit, by = c("state", "year")) %>%
  mutate(cyp_share_excess   = 100 * cyp_exc / exc_all_fit,
         cyp_share_expected = 100 * cyp_bsn / bsn_all_fit,
         cyp_vs_all_pp      = cyp_pscore - all_pscore)

# difference from the national CYP p-score (percentage points)
us_ref <- res %>% filter(state == "us") %>% select(year, us_cyp_pscore = cyp_pscore)

res <-
  res %>%
  left_join(us_ref, by = "year") %>%
  mutate(cyp_vs_us_pp = cyp_pscore - us_cyp_pscore)

stopifnot(nrow(res) == 52 * 4, !anyNA(res$cyp_pscore), !anyNA(res$all_pscore))

# CYP share of excess and of COVID-19 deaths, age-standardised ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The crude share of CYP in a state's excess (or COVID-19) deaths depends on
# the state's age structure. The standardised share is the share CYP would have
# if the state had the US age structure: age-specific rates of the state
# (seven groups: six CYP groups and 25+) applied to the US 2020 population,
#   share_std = sum_{a in CYP} w_a r_a / sum_a w_a r_a.
# Adults are a single 25+ group because the state population exports do not
# split them further. The share of excess is a ratio of two small numbers when
# the all-age excess is close to zero (many states in 2023); all_exc_sig flags
# the states whose all-age excess CI excludes zero; all_ind_sig does the same
# for the all-age indirect excess (observed - COVID-19 deaths vs the baseline CI).
c19 <-
  read_rds("data_inter/c19_deaths_by_state_and_age.rds") %>%
  mutate(age_grp = ifelse(age >= 25, "25+", age_labs[as.character(age)])) %>%
  reframe(c19 = sum(dx), .by = c(state, year, age_grp))

c19 <-
  bind_rows(c19,
            c19 %>% reframe(c19 = sum(c19), .by = c(year, age_grp)) %>% mutate(state = "us"))

shares <-
  by_age %>%
  inner_join(c19, by = c("state", "year", "age_grp")) %>%
  inner_join(w_all, by = "age_grp") %>%
  mutate(is_cyp = age_grp != "25+",
         exc = dts - bsn,
         ind = exc - c19) %>%
  group_by(state, year) %>%
  summarise(cyp_share_excess_crude = 100 * sum(exc[is_cyp]) / sum(exc),
            cyp_share_excess_std   = 100 * sum((w * exc / pop)[is_cyp]) / sum(w * exc / pop),
            cyp_share_c19_crude    = 100 * sum(c19[is_cyp]) / sum(c19),
            cyp_share_c19_std      = 100 * sum((w * c19 / pop)[is_cyp]) / sum(w * c19 / pop),
            cyp_share_indirect_crude = 100 * sum(ind[is_cyp]) / sum(ind),
            cyp_share_indirect_std   = 100 * sum((w * ind / pop)[is_cyp]) / sum(w * ind / pop),
            cyp_share_pop_std      = 100 * sum(w[is_cyp]),
            .groups = "drop")

stopifnot(nrow(shares) == 52 * 4)

# all-age indirect excess from the all-age fit and its significance
all_ind <-
  broad %>%
  filter(age_grp == "all") %>%
  inner_join(c19 %>% reframe(c19_all = sum(c19), .by = c(state, year)), by = c("state", "year")) %>%
  transmute(state, year, c19_all,
            ind_all_fit = dts - bsn - c19_all,
            all_ind_sig = (dts - c19_all - uc) > 0 | (dts - c19_all - lc) < 0)

shares <- shares %>% inner_join(all_ind, by = c("state", "year"))

res <- res %>% inner_join(shares, by = c("state", "year"))

# CYP p-score split into direct and indirect ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total p-score = excess / expected. Direct = registered COVID-19 deaths /
# expected. Indirect = (excess - COVID-19 deaths) / expected = total - direct,
# which is negative when the excess falls short of the registered COVID-19
# deaths (e.g. excess -100 with 80 COVID-19 deaths gives indirect -180).
# All three are age-standardised to the US 0-24 population (six groups), as the
# CYP p-score above; the direct and indirect components carry the CI of the
# baseline (the COVID-19 count is treated as fixed).
cyp_c19 <-
  cyp %>%
  inner_join(c19 %>% filter(age_grp != "25+"), by = c("state", "year", "age_grp")) %>%
  inner_join(w_cyp, by = "age_grp") %>%
  group_by(state, year) %>%
  summarise(obs_std    = sum(w * dts / pop),
            exp_std    = sum(w * bsn / pop),
            exp_std_lc = sum(w * lc / pop),
            exp_std_uc = sum(w * uc / pop),
            c19_std    = sum(w * c19 / pop),
            c19 = sum(c19), exc = sum(dts - bsn),
            .groups = "drop") %>%
  mutate(cyp_psc_total    = 100 * (obs_std / exp_std - 1),
         cyp_psc_direct   = 100 * c19_std / exp_std,
         cyp_psc_indirect = cyp_psc_total - cyp_psc_direct,
         cyp_psc_direct_lb   = 100 * c19_std / exp_std_uc,
         cyp_psc_direct_ub   = 100 * c19_std / exp_std_lc,
         cyp_psc_indirect_lb = 100 * ((obs_std - c19_std) / exp_std_uc - 1),
         cyp_psc_indirect_ub = 100 * ((obs_std - c19_std) / exp_std_lc - 1),
         cyp_c19_deaths = c19,
         cyp_indirect_excess = exc - c19)

stopifnot(nrow(cyp_c19) == 52 * 4,
          all(abs(cyp_c19$cyp_psc_total - res$cyp_pscore[match(paste(cyp_c19$state, cyp_c19$year),
                                                                paste(res$state, res$year))]) < 1e-9))

res <-
  res %>%
  inner_join(cyp_c19 %>%
               select(state, year, cyp_psc_direct, cyp_psc_direct_lb, cyp_psc_direct_ub,
                      cyp_psc_indirect, cyp_psc_indirect_lb, cyp_psc_indirect_ub,
                      cyp_c19_deaths, cyp_indirect_excess),
             by = c("state", "year"))

write_rds(res, "data_inter/cyp_vs_all_ages_by_state.rds")

# Table S1 ====
# ~~~~~~~~~~~~~
res %>%
  mutate(state = ifelse(state == "us", "US", state)) %>%
  select(state, year,
         cyp_pscore_std = cyp_pscore, cyp_pscore_lb, cyp_pscore_ub, cyp_pscore_crude,
         cyp_pscore_direct = cyp_psc_direct, cyp_pscore_direct_lb = cyp_psc_direct_lb,
         cyp_pscore_direct_ub = cyp_psc_direct_ub,
         cyp_pscore_indirect = cyp_psc_indirect, cyp_pscore_indirect_lb = cyp_psc_indirect_lb,
         cyp_pscore_indirect_ub = cyp_psc_indirect_ub,
         adult_pscore, adult_pscore_lb, adult_pscore_ub,
         all_pscore_std = all_pscore, all_pscore_lb, all_pscore_ub, all_pscore_crude,
         cyp_excess = cyp_exc, cyp_c19_deaths, cyp_indirect_excess,
         adult_excess = adult_exc, all_excess = exc_all_fit,
         cyp_share_excess_crude, cyp_share_excess_std, all_excess_significant = all_exc_sig,
         cyp_share_c19_crude, cyp_share_c19_std, all_c19_deaths = c19_all,
         cyp_share_indirect_crude, cyp_share_indirect_std, all_indirect_excess = ind_all_fit,
         all_indirect_significant = all_ind_sig,
         cyp_share_expected, cyp_vs_all_pp, cyp_vs_us_pp) %>%
  mutate(across(where(is.numeric) & !year, ~ round(.x, 2))) %>%
  arrange(state != "US", state, year) %>%
  write_csv("tables/tableS1_pscores_cyp_share_by_state.csv")

# printed checks: national values and the range across states ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res %>%
  filter(state == "us") %>%
  select(year, cyp_pscore, cyp_psc_direct, cyp_psc_indirect, adult_pscore, all_pscore,
         cyp_share_excess_std, cyp_share_c19_std, cyp_share_indirect_std, cyp_share_expected) %>%
  mutate(across(-year, ~ round(.x, 1))) %>%
  print(width = Inf)

res %>%
  filter(state != "us") %>%
  reframe(n_all_exc_sig = sum(all_exc_sig),
          across(c(cyp_pscore, cyp_psc_direct, cyp_psc_indirect, adult_pscore, all_pscore),
                 list(min = ~ round(min(.x), 1), max = ~ round(max(.x), 1))),
          .by = year) %>%
  print(width = Inf)
