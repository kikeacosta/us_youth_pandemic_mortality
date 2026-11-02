# EXPLORATORY (not in run_all.R): excess mortality by race/ethnicity, ages 0-24,
# US, 2020-2023, as p-scores and excess death rates, with the all-races series
# for reference. The relative risks of Figure 4 show the level of racial
# inequality in COVID-19 mortality; p-scores by race show whether the pandemic
# widened or narrowed the pre-existing inequality in all-cause mortality.
#
# Data: all-cause deaths by race come from two WONDER series with different race
# concepts (data_input/all_cause_race/README.md): bridged race 2010-2020 (D76,
# API) and single race 2018-2024 (WONDER export, Hispanic origin x Single Race 6
# x age x year). The baseline is fitted on the bridged series 2010-2019 (same
# quasi-Poisson log-linear model as the main analysis, on rates), and the
# expected rate is calibrated to the single-race series with the ratio of the
# bridged to the single-race death rate on the overlap years 2018-2019 (by race
# x age; 2020 is left out because the 2020 bridged-race population estimates
# break the series: +11 to +14% for Asian/PI ages 0-9 and -9% for AIAN infants
# relative to 2019, against +-2% for the other groups). Observed deaths and
# populations for 2020-2023 are the single-race ones.
# Groups: Hispanic (all races); non-Hispanic White, Black, Asian, American
# Indian or Alaska Native. The bridged baseline for "nha" is Asian or Pacific
# Islander (not separable in D76); the calibration ratio absorbs the level
# difference with single-race Asian, which is the group of Figure 4. "More than
# one race" and NHOPI have no bridged counterpart and are left out (they are in
# the all-races total). The Hispanic total comes from a separate query without
# race grouping because the export hides cells with < 10 deaths (see below).
# All races: the national series of the main analysis (data_inter/excess_age.rds).
# COVID-19 deaths by race (script 02) give the direct component: direct p-score
# = COVID-19 / expected, indirect = (excess - COVID-19) / expected; and a
# baseline-free measure, COVID-19 / (all-cause - COVID-19) deaths, i.e. by how
# much COVID-19 raised the group's deaths above its other deaths of the same year.
# Rates are directly age-standardised to the 2020 US population aged 0-24.
#
# Input : data_input/all_cause_race/*.csv, data_inter/excess_age.rds,
#         data_inter/c19_deaths_rates_by_race_and_age.rds, data_inter/pop_us_by_sex_age_2010_2023.rds
# Output: figures/alternatives/figR1_pscore_excess_by_race.png (+ .csv)
#         figures/alternatives/figR2_pscore_by_race_age.png (+ .csv)
#         figures/alternatives/figR3_direct_indirect_by_race.png

source("code/00_functions.R")
library(patchwork)

age_code <- c(`< 1 year` = 0, `1-4 years` = 1, `5-9 years` = 5, `10-14 years` = 10,
              `15-19 years` = 15, `20-24 years` = 20)

read_api <- function(file){
  read_csv(file, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(lab1),
           age  = unname(age_code[lab2]),
           dts  = as.numeric(gsub(",", "", deaths)),
           pop  = suppressWarnings(as.numeric(gsub(",", "", population)))) %>%
    select(-deaths, -population, -crude_rate, -lab1, -lab2)
}

race_from <- function(hisp, race){
  case_when(hisp == "Hispanic or Latino" ~ "hsp",
            hisp != "Not Hispanic or Latino" ~ NA_character_,
            race == "White" ~ "nhw",
            race == "Black or African American" ~ "nhb",
            race %in% c("Asian or Pacific Islander", "Asian") ~ "nha",
            race == "American Indian or Alaska Native" ~ "nhaian",
            TRUE ~ NA_character_)
}

# bridged series (API): Hispanic from the origin table, the rest from the race table
read_series <- function(f_hisp, f_race){
  h <- read_api(f_hisp) %>% mutate(race = race_from(lab3, NA)) %>% filter(race == "hsp")
  r <- read_api(f_race) %>% mutate(race = race_from(lab3, lab4)) %>% filter(!is.na(race))
  bind_rows(h, r) %>%
    reframe(dts = sum(dts), pop = sum(pop), .by = c(race, year, age))
}

d <- "data_input/all_cause_race"
br <- read_series(file.path(d, "d76_ac_year_age_hisp_2010_2020.csv"),
                  file.path(d, "d76_ac_year_age_nonhisp_race_2010_2020.csv"))
stopifnot(nrow(br) == 5 * 11 * 6, !anyNA(br$pop))

# single-race series: the non-Hispanic groups from the WONDER export (Hispanic
# origin x Single Race 6 x age x year; footer after the data rows; cells with
# < 10 deaths are hidden, which does not affect these four groups at ages 0-24),
# and Hispanic (all races) from the API query without race grouping, which is
# complete (summing the Hispanic rows of the export would drop the hidden
# Hispanic Asian / AIAN / NHOPI cells and overstate Hispanic rates at ages 1-14
# by 2-5%, as their low-mortality populations would be left out)
sr_nh <-
  read_csv(file.path(d, "dts_ac_hisp_race6_age_2018_2024.csv"),
           col_types = cols(.default = "c"), n_max = 2101, progress = FALSE) %>%
  filter(is.na(Notes) | Notes == "") %>%
  transmute(race = race_from(`Hispanic Origin`, `Single Race 6`),
            age = unname(age_code[`Five-Year Age Groups`]), year = as.integer(Year),
            dts = as.numeric(Deaths), pop = suppressWarnings(as.numeric(Population))) %>%
  filter(!is.na(age), !is.na(race), race != "hsp", year <= 2023)

sr_h <-
  read_api(file.path(d, "d157_ac_year_age_hisp_2018_2023.csv")) %>%
  mutate(race = race_from(lab3, NA)) %>%
  filter(race == "hsp") %>%
  select(race, year, age, dts, pop)

sr <- bind_rows(sr_nh, sr_h)
stopifnot(nrow(sr) == 5 * 6 * 6, !anyNA(sr$pop), !anyNA(sr$dts))

race_labs <- c(all = "All races", hsp = "Hispanic", nhw = "Non-Hispanic White",
               nhb = "Non-Hispanic Black", nha = "Non-Hispanic Asian",
               nhaian = "Non-Hispanic American Indian or Alaska Native")
race_cols <- c(all = "black", hsp = "#ef476f", nha = "#e0b000", nhb = "#06d6a0",
               nhw = "#118ab2", nhaian = "#8338ec")

# Expected rates, 2020-2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# quasi-Poisson, log-linear in year, offset log(pop), fitted to 2010-2019 of the
# bridged series; expected rate per 100,000 with its 95% CI
fit_rate <- function(x){
  m  <- glm(dts ~ year + offset(log(pop)), family = quasipoisson, data = x)
  nd <- tibble(year = 2020:2023, pop = 1e5)
  p  <- predict(m, newdata = nd, type = "response", se.fit = TRUE)
  tibble(year = nd$year, exp_rate = p$fit,
         exp_rate_lc = p$fit - 1.96 * p$se.fit, exp_rate_uc = p$fit + 1.96 * p$se.fit)
}

exp_br <-
  br %>%
  filter(year <= 2019) %>%
  group_by(race, age) %>%
  group_modify(~ fit_rate(.x)) %>%
  ungroup()

# calibration bridged -> single race on 2018-2019, by race x age
k <-
  inner_join(br %>% filter(year %in% 2018:2019) %>% reframe(r_br = sum(dts) / sum(pop), .by = c(race, age)),
             sr %>% filter(year %in% 2018:2019) %>% reframe(r_sr = sum(dts) / sum(pop), .by = c(race, age)),
             by = c("race", "age")) %>%
  mutate(k = r_br / r_sr)
cat("\nbridged / single-race rate ratio, 2018-2019 (calibration):\n")
k %>% select(race, age, k) %>% mutate(k = round(k, 3)) %>%
  pivot_wider(names_from = age, values_from = k) %>% print()
obs <- sr %>% filter(year >= 2020)

by_race_age <-
  obs %>%
  inner_join(exp_br, by = c("race", "year", "age")) %>%
  inner_join(k %>% select(race, age, k), by = c("race", "age")) %>%
  mutate(bsn = exp_rate / k * pop / 1e5,
         lc  = exp_rate_lc / k * pop / 1e5,
         uc  = exp_rate_uc / k * pop / 1e5) %>%
  select(race, year, age, dts, pop, bsn, lc, uc)

# all races: national series of the main analysis
all_races <-
  read_rds("data_inter/excess_age.rds") %>%
  filter(age <= 20) %>%
  transmute(race = "all", year, age, dts, pop = exposure, bsn, lc, uc)

# COVID-19 deaths by race (direct component); nha = single-race Asian only
c19 <-
  read_rds("data_inter/c19_deaths_rates_by_race_and_age.rds") %>%
  filter(age < 25) %>%
  select(race, year, age, c19 = dx)

dat <-
  bind_rows(by_race_age, all_races) %>%
  left_join(c19, by = c("race", "year", "age")) %>%
  mutate(exc = dts - bsn, exc_lb = dts - uc, exc_ub = dts - lc,
         psc = 100 * (dts / bsn - 1), psc_lb = 100 * (dts / uc - 1), psc_ub = 100 * (dts / lc - 1),
         age_group = factor(age_labs[as.character(age)], levels = age_labs),
         race = factor(race, levels = names(race_labs)))

# Age-standardised rates, ages 0-24 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w_cyp <-
  read_rds("data_inter/pop_us_by_sex_age_2010_2023.rds") %>%
  filter(year == 2020, age <= 20) %>%
  reframe(pop = sum(pop), .by = age) %>%
  transmute(age, w = pop / sum(pop))

std <-
  dat %>%
  inner_join(w_cyp, by = "age") %>%
  group_by(race, year) %>%
  summarise(obs_std = 1e5 * sum(w * dts / pop),
            exp_std = 1e5 * sum(w * bsn / pop),
            exp_lc  = 1e5 * sum(w * lc / pop),
            exp_uc  = 1e5 * sum(w * uc / pop),
            c19_std = 1e5 * sum(w * c19 / pop),
            crude_obs = 1e5 * sum(dts) / sum(pop),
            crude_exp = 1e5 * sum(bsn) / sum(pop),
            dts = sum(dts), bsn = sum(bsn), c19 = sum(c19), .groups = "drop") %>%
  mutate(psc    = 100 * (obs_std / exp_std - 1),
         psc_lb = 100 * (obs_std / exp_uc - 1),
         psc_ub = 100 * (obs_std / exp_lc - 1),
         exc_rate    = obs_std - exp_std,
         exc_rate_lb = obs_std - exp_uc,
         exc_rate_ub = obs_std - exp_lc,
         psc_direct      = 100 * c19_std / exp_std,
         psc_direct_lb   = 100 * c19_std / exp_uc,
         psc_direct_ub   = 100 * c19_std / exp_lc,
         psc_indirect    = 100 * ((obs_std - c19_std) / exp_std - 1),
         psc_indirect_lb = 100 * ((obs_std - c19_std) / exp_uc - 1),
         psc_indirect_ub = 100 * ((obs_std - c19_std) / exp_lc - 1),
         # baseline-free: COVID-19 deaths relative to the other deaths of the year
         c19_vs_other       = 100 * c19_std / (obs_std - c19_std),
         c19_vs_other_crude = 100 * c19 / (dts - c19),
         c19_vs_other_lb = NA_real_, c19_vs_other_ub = NA_real_)

# pre-pandemic inequality for reference: expected rate relative to all races
std %>%
  mutate(exp_vs_all = round(exp_std / exp_std[race == "all"], 2),
         obs_vs_all = round(obs_std / obs_std[race == "all"], 2), .by = year) %>%
  transmute(race, year, exp_std = round(exp_std, 1), exp_vs_all, obs_vs_all,
            psc = round(psc, 1), exc_rate = round(exc_rate, 1)) %>%
  arrange(year, race) %>%
  print(n = Inf)

# Figure R1: p-score and excess death rate by race and year ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pd <- position_dodge(width = 0.65)

race_theme <- function(){
  theme_bars() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 9, hjust = 0))
}

race_pts <- function(d, y, lb, ub, ref, title, ylab){
  ggplot(d, aes(x = factor(year), y = .data[[y]], colour = race, shape = race)) +
    geom_hline(yintercept = ref, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(aes(ymin = .data[[lb]], ymax = .data[[ub]]), width = 0.25,
                  position = pd, linewidth = 0.4, na.rm = TRUE) +
    geom_point(position = pd, size = 2) +
    scale_colour_manual(values = race_cols, labels = race_labs) +
    scale_shape_manual(values = c(all = 15, hsp = 16, nhw = 16, nhb = 16, nha = 16, nhaian = 16),
                       labels = race_labs) +
    labs(x = NULL, y = ylab, title = title) +
    race_theme()
}

p_r1 <-
  (race_pts(std, "psc", "psc_lb", "psc_ub", 0,
            "Deaths relative to expected, ages 0-24 (p-score, %)", "p-score (%)") +
   race_pts(std, "exc_rate", "exc_rate_lb", "exc_rate_ub", 0,
            "Excess deaths per 100,000, ages 0-24", "excess deaths per 100,000")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
p_r1 <- p_r1 + plot_annotation(caption = "Age-standardised to the 2020 US population aged 0-24; 95% CI of the baseline.",
                               theme = theme(plot.caption = element_text(size = 7, hjust = 0)))

ggsave("figures/alternatives/figR1_pscore_excess_by_race.png", p_r1, width = 8, height = 4.2, dpi = 300)

# Figure R2: p-score by race, age group and year ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pscore_lab <- function(x) sprintf("%+.0f%%", (x - 1) * 100)
r_breaks <- c(0.5, 1, 2)

p_r2 <-
  dat %>%
  mutate(r = dts / bsn, r_lb = dts / uc, r_ub = dts / lc) %>%
  ggplot(aes(x = r, y = factor(year, levels = 2023:2020), colour = race, shape = race)) +
  geom_vline(xintercept = 1, linewidth = 0.3, colour = "grey40") +
  geom_errorbar(aes(xmin = r_lb, xmax = r_ub), width = 0.3, orientation = "y",
                position = pd, linewidth = 0.4) +
  geom_point(position = pd, size = 1.8) +
  facet_grid(~ age_group) +
  scale_x_continuous(transform = "log10", breaks = r_breaks, labels = pscore_lab, minor_breaks = NULL) +
  coord_cartesian(xlim = c(0.45, 2.3)) +
  scale_colour_manual(values = race_cols, labels = race_labs) +
  scale_shape_manual(values = c(all = 15, hsp = 16, nhw = 16, nhb = 16, nha = 16, nhaian = 16),
                     labels = race_labs) +
  labs(x = "Deaths relative to expected (p-score), log ratio scale", y = NULL,
       title = "P-score by age group, race/ethnicity and year (95% CI of the baseline; clipped at -55% / +130%)") +
  race_theme() +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))

ggsave("figures/alternatives/figR2_pscore_by_race_age.png", p_r2, width = 10, height = 4, dpi = 300)

# Figure R3: total, direct (COVID-19) and indirect p-scores by race ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
std_c19 <- std %>% filter(!is.na(c19_std))

p_r3 <-
  (race_pts(std_c19, "psc", "psc_lb", "psc_ub", 0, "Total: excess / expected (%)", "p-score (%)") +
   race_pts(std_c19, "psc_direct", "psc_direct_lb", "psc_direct_ub", 0, "Direct: COVID-19 deaths / expected (%)", NULL) +
   race_pts(std_c19, "psc_indirect", "psc_indirect_lb", "psc_indirect_ub", 0, "Indirect: (excess - COVID-19) / expected (%)", NULL) +
   race_pts(std_c19, "c19_vs_other", "c19_vs_other_lb", "c19_vs_other_ub", 0, "Baseline-free: COVID-19 / other deaths (%)", NULL)) +
  plot_layout(guides = "collect", nrow = 1) &
  # common y axis across the four panels (range of all values and CI bounds)
  coord_cartesian(ylim = range(c(std_c19$psc_lb, std_c19$psc_ub, std_c19$psc_indirect_lb,
                                 std_c19$psc_indirect_ub, std_c19$psc_direct_ub, std_c19$c19_vs_other))) &
  theme(legend.position = "bottom") &
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
p_r3 <- p_r3 + plot_annotation(caption = "Age-standardised to the 2020 US population aged 0-24, common y axis. The first three panels carry the 95% CI of the baseline; the fourth needs no baseline.",
                               theme = theme(plot.caption = element_text(size = 7, hjust = 0)))

ggsave("figures/alternatives/figR3_direct_indirect_by_race.png", p_r3, width = 12, height = 4.2, dpi = 300)

std %>%
  mutate(race = race_labs[as.character(race)], race_concept = "single race, baseline calibrated from bridged race") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(year, race) %>%
  write_csv("figures/alternatives/figR1_pscore_excess_by_race.csv")

dat %>%
  mutate(race = race_labs[as.character(race)]) %>%
  select(race, year, age_group, deaths = dts, pop, expected = bsn, expected_lc = lc, expected_uc = uc,
         c19, excess = exc, pscore = psc, pscore_lb = psc_lb, pscore_ub = psc_ub) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(year, race, age_group) %>%
  write_csv("figures/alternatives/figR2_pscore_by_race_age.csv")
