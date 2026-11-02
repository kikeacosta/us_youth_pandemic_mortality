# Sensitivity analyses for the excess-mortality estimates (appendix).
#
# (1) Baseline fitting window, US by age group (and ages 0-24 combined):
#     log-linear trend fitted to 2010-2019 (main), 2014-2019 or 2015-2019; each
#     with the 95% CI of the baseline (as in the main figures) and the 95%
#     bootstrapped prediction interval.
#     -> figures/figS1_sensitivity_excess_by_age.png (+ .csv)
#     Figure S2 shows the observed death rates 2010-2023 with the three fitted
#     baselines, i.e. where the windows diverge (ages 20-24 and 0-1).
#     -> figures/figS2_baselines_by_specification.png (+ .csv)
# (2) State excess 0-24: baseline fitted on the 0-24 aggregate (main analysis)
#     vs. sum of the age-specific state baselines (script 04, series b).
#     -> output/sensitivity_state_baseline_aggregation.csv
# (3) Significance of state excess with CI vs. PI.
# (4) Regional intervals: sum of state bounds (Figure 3) vs. baseline fitted on
#     regional totals (script 04, series d).
# (5) Out-of-sample check: 2018-2019 predicted from windows ending in 2017.
#     -> output/sensitivity_cv_2018_2019.csv
# A text summary of (1)-(5) is written to output/sensitivity_summary.txt.
#
# Input : data_inter/ac_deaths_by_sex_age.rds (script 03)
#         data_inter/excess_states_age_u25.rds, baselines_states_sex_age.rds,
#         excess_regions_u25.rds (script 04)

source("code/00_functions.R")

set.seed(2024)

# Baseline with a choice of fitting window ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same model as est_baseline_pi() (script 00); identical when start_year = 2010.
est_baseline_spec <- function(chunk, start_year, end_year = 2019, nsim = 200){

  chunk2 <-
    chunk %>%
    arrange(year) %>%
    mutate(w = ifelse(year >= start_year & year <= end_year, 1, 0),
           t = 1:n())

  model <- glm(dts ~ year + offset(log(exposure)),
               weights = w, family = "quasipoisson", data = chunk2)

  # (summary.glm warns that zero-weight years are not used for the dispersion)
  res <- suppressWarnings(predict(model, newdata = chunk2, type = "response", se.fit = TRUE))

  chunk2 %>%
    mutate(bsn = res$fit,
           lc = bsn - 1.96 * res$se.fit,
           uc = bsn + 1.96 * res$se.fit) %>%
    left_join(simul_intvals(model, model_type = "glm", db = chunk2,
                            nsim = nsim, p = 0.95),
              by = "t")
}

specs <-
  tibble(start_year = c(2010, 2014, 2015)) %>%
  mutate(window = paste0(start_year, "-2019"),
         main = start_year == 2010)          # fitting window used in the paper

# (1) US by age group, 2020-2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
us <-
  read_rds("data_inter/ac_deaths_by_sex_age.rds") %>%
  filter(sex == "t", age <= 20) %>%
  select(age, year, dts = dx, exposure = pop)

# add ages 0-24 combined (coded age = -1 so it sorts first)
us_all <-
  bind_rows(us,
            us %>% reframe(dts = sum(dts), exposure = sum(exposure), .by = year) %>%
              mutate(age = -1))

age_labs_s <- c(`-1` = "0-24", age_labs)

# all years (observed and fitted), used for Figure S2
fits <-
  specs %>%
  mutate(res = map(start_year, function(s){
    us_all %>%
      group_by(age) %>%
      do(est_baseline_spec(chunk = .data, start_year = s)) %>%
      ungroup()
  })) %>%
  select(window, main, res) %>%
  unnest(res) %>%
  mutate(age_group = factor(age_labs_s[as.character(age)], levels = age_labs_s))

sens <-
  fits %>%
  filter(year >= 2020) %>%
  mutate(lp = pmax(lp, 0), up = pmax(up, 0),
         exc    = dts - bsn,
         exc_ci_lb = pmin(dts - lc, dts - uc), exc_ci_ub = pmax(dts - lc, dts - uc),
         exc_pi_lb = pmin(dts - lp, dts - up), exc_pi_ub = pmax(dts - lp, dts - up),
         across(c(exc, exc_ci_lb, exc_ci_ub, exc_pi_lb, exc_pi_ub),
                ~ 1e5 * .x / exposure, .names = "r_{.col}"))

stopifnot(!anyNA(sens$exc))

# the main specification reproduces excess_age.rds (same model, same data)
chk <-
  sens %>% filter(main, age >= 0) %>% select(age, year, exc) %>%
  inner_join(read_rds("data_inter/excess_age.rds") %>% filter(age <= 20) %>%
               select(age, year, exc_main = exc),
             by = c("age", "year"))
stopifnot(max(abs(chk$exc - chk$exc_main)) < 1e-6)

# Figure S1 ====
# ~~~~~~~~~~~~~~
plot_dat <-
  sens %>%
  mutate(window = factor(window, levels = rev(specs$window)))   # main window at the top

window_cols <- c(`2010-2019` = "#762A83", `2014-2019` = "#1B7837", `2015-2019` = "#E08214")

p_sens <-
  ggplot(plot_dat, aes(y = window, colour = window)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_linerange(aes(xmin = r_exc_pi_lb, xmax = r_exc_pi_ub), linewidth = 0.4, alpha = 0.6) +
  geom_linerange(aes(xmin = r_exc_ci_lb, xmax = r_exc_ci_ub), linewidth = 1.2) +
  geom_point(aes(x = r_exc), size = 1.8) +
  scale_colour_manual(values = window_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  facet_grid(year ~ age_group, switch = "y", scales = "free_x") +
  labs(x = "Excess deaths per 100,000 (point, 95% CI of the baseline — thick; 95% prediction interval — thin)",
       y = "Fitting window of the log-linear baseline") +
  theme_bars() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text  = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.text.y  = element_text(size = 7),
        panel.grid.major.y = element_line(colour = "grey92"))

ggsave("figures/figS1_sensitivity_excess_by_age.png", p_sens,
       width = 11, height = 7.5, dpi = 300)

sens %>%
  select(year, age_group, window, main_specification = main,
         deaths = dts, baseline = bsn,
         excess = exc, excess_ci_lower = exc_ci_lb, excess_ci_upper = exc_ci_ub,
         excess_pi_lower = exc_pi_lb, excess_pi_upper = exc_pi_ub,
         excess_rate = r_exc, excess_rate_ci_lower = r_exc_ci_lb, excess_rate_ci_upper = r_exc_ci_ub,
         excess_rate_pi_lower = r_exc_pi_lb, excess_rate_pi_upper = r_exc_pi_ub) %>%
  arrange(year, age_group, window) %>%
  write_csv("figures/figS1_sensitivity_excess_by_age.csv")

# Figure S2: observed rates and fitted baselines, 2010-2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
obs <-
  us_all %>%
  mutate(mx = 1e5 * dts / exposure,
         period = ifelse(year >= 2020, "pandemic (not fitted)", "pre-pandemic"),
         age_group = factor(age_labs_s[as.character(age)], levels = age_labs_s))

fit_lines <-
  fits %>%
  mutate(mx_bsn = 1e5 * bsn / exposure,
         mx_lc  = 1e5 * lc / exposure,
         mx_uc  = 1e5 * uc / exposure,
         window = factor(window, levels = specs$window))

p_fits <-
  ggplot() +
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.6) +
  geom_ribbon(data = fit_lines %>% filter(main),
              aes(x = year, ymin = mx_lc, ymax = mx_uc), fill = window_cols[1], alpha = 0.15) +
  geom_line(data = fit_lines,
            aes(x = year, y = mx_bsn, colour = window), linewidth = 0.7) +
  geom_point(data = obs, aes(x = year, y = mx, shape = period), size = 1.6, fill = "white") +
  scale_colour_manual(values = window_cols, name = "Fitting window") +
  scale_shape_manual(values = c(`pre-pandemic` = 16, `pandemic (not fitted)` = 21), name = "Observed") +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  facet_wrap(~ age_group, scales = "free_y", nrow = 2) +
  labs(x = NULL, y = "Deaths per 100,000",
       caption = "Lines: log-linear baselines fitted to each window and projected to 2020-2023. Shaded band: 95% CI of the main specification (2010-2019).") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(size = 8), legend.text = element_text(size = 8),
        strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 7, hjust = 0),
        axis.text = element_text(size = 7))

ggsave("figures/figS2_baselines_by_specification.png", p_fits, width = 10, height = 5.5, dpi = 300)

fit_lines %>%
  select(age_group, year, window, main_specification = main,
         deaths = dts, population = exposure, baseline = bsn,
         baseline_ci_lower = lc, baseline_ci_upper = uc, rate_baseline = mx_bsn) %>%
  left_join(obs %>% select(age_group, year, rate_observed = mx), by = c("age_group", "year")) %>%
  arrange(age_group, window, year) %>%
  write_csv("figures/figS2_baselines_by_specification.csv")

# pre-pandemic trend by fitting window: annual % change implied by the log-linear fit
slopes <-
  expand_grid(age = unique(us_all$age), start_year = c(2010, 2014, 2015)) %>%
  mutate(pct_per_year = map2_dbl(age, start_year, function(a, s){
    d <- us_all %>% filter(age == a, year >= s, year <= 2019)
    m <- glm(dts ~ year + offset(log(exposure)), family = "quasipoisson", data = d)
    100 * (exp(coef(m)[["year"]]) - 1)
  })) %>%
  mutate(age_group = factor(age_labs_s[as.character(age)], levels = age_labs_s),
         window = paste0(start_year, "-2019")) %>%
  select(age_group, window, pct_per_year) %>%
  pivot_wider(names_from = window, values_from = pct_per_year) %>%
  arrange(age_group)

# (2) State 0-24: aggregate baseline vs sum of age-specific baselines ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
st_agg <-
  read_rds("data_inter/excess_states_age_u25.rds") %>%
  filter(state != "us") %>%
  select(state, year, dts, exposure, bsn_aggregate = bsn)

st_sum <-
  read_rds("data_inter/baselines_states_sex_age.rds") %>%
  filter(state != "us", year >= 2020) %>%
  reframe(bsn_sum_ages = sum(bsn), .by = c(state, year))

st_cmp <-
  st_agg %>%
  inner_join(st_sum, by = c("state", "year")) %>%
  mutate(exc_aggregate = dts - bsn_aggregate,
         exc_sum_ages  = dts - bsn_sum_ages,
         exc_rate_aggregate = 1e5 * exc_aggregate / exposure,
         exc_rate_sum_ages  = 1e5 * exc_sum_ages / exposure,
         diff_rate = exc_rate_sum_ages - exc_rate_aggregate)

stopifnot(nrow(st_cmp) == 51 * 4)

st_cmp %>%
  select(state, year, deaths = dts, bsn_aggregate, bsn_sum_ages,
         exc_rate_aggregate, exc_rate_sum_ages, diff_rate) %>%
  arrange(state, year) %>%
  write_csv("output/sensitivity_state_baseline_aggregation.csv")

# (3) State significance: CI vs PI ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
st_sig <-
  read_rds("data_inter/excess_states_age_u25.rds") %>%
  filter(state != "us") %>%
  mutate(sig_ci = ifelse(dts > uc, "positive", ifelse(dts < lc, "negative", "n.s.")),
         sig_pi = ifelse(dts > up, "positive", ifelse(dts < lp, "negative", "n.s."))) %>%
  count(year, sig_ci, sig_pi)

# (4) Regional intervals: sum of state bounds vs regional fit ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg_sum <-
  read_rds("data_inter/excess_states_age_u25.rds") %>%
  filter(state != "us") %>%
  inner_join(bea_lookup %>% select(state, bea_region), by = "state") %>%
  group_by(bea_region, year) %>%
  summarise(exc_sum = sum(exc),
            lb_sum = sum(dts - uc), ub_sum = sum(dts - lc),
            exposure = sum(exposure), .groups = "drop")

reg_fit <-
  read_rds("data_inter/excess_regions_u25.rds") %>%
  mutate(lb_fit = dts - uc, ub_fit = dts - lc) %>%
  select(bea_region, year, exc_fit = exc, lb_fit, ub_fit)

reg_cmp <-
  reg_sum %>%
  inner_join(reg_fit, by = c("bea_region", "year")) %>%
  mutate(across(c(exc_sum, lb_sum, ub_sum, exc_fit, lb_fit, ub_fit), ~ 1e5 * .x / exposure)) %>%
  select(-exposure) %>%
  mutate(width_ratio = (ub_fit - lb_fit) / (ub_sum - lb_sum),
         sig_sum = ifelse(lb_sum > 0, "pos", ifelse(ub_sum < 0, "neg", "n.s.")),
         sig_fit = ifelse(lb_fit > 0, "pos", ifelse(ub_fit < 0, "neg", "n.s.")))

# (5) Out-of-sample check: predict 2018-2019 from windows ending in 2017 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The same three window lengths as in (1), shifted back so that 2018-2019 are
# held out: 2010-2017 (8 years, the longest available), 2012-2017 (6 years, as
# 2014-2019) and 2013-2017 (5 years, as 2015-2019). The horizon is 1-2 years,
# shorter than the 1-4 years projected in the paper.
cv_windows <- tibble(start_year = c(2010, 2012, 2013), end_year = 2017) %>%
  mutate(window = paste0(start_year, "-", end_year))

cv <-
  cv_windows %>%
  mutate(res = map2(start_year, end_year, function(s, e){
    us_all %>%
      filter(year <= 2019) %>%
      group_by(age) %>%
      do(est_baseline_spec(chunk = .data, start_year = s, end_year = e)) %>%
      ungroup()
  })) %>%
  select(window, res) %>%
  unnest(res) %>%
  filter(year %in% 2018:2019) %>%
  mutate(age_group = factor(age_labs_s[as.character(age)], levels = age_labs_s),
         err = dts - bsn,
         pct_err = 100 * err / dts,
         in_ci = dts >= lc & dts <= uc,
         in_pi = dts >= lp & dts <= up)

cv_summary <-
  cv %>%
  group_by(age_group, window) %>%
  summarise(mape = mean(abs(pct_err)),
            bias_pct = mean(pct_err),
            ci_cover = mean(in_ci),
            pi_cover = mean(in_pi),
            .groups = "drop")

cv %>%
  select(age_group, window, year, deaths = dts, predicted = bsn, ci_lower = lc, ci_upper = uc,
         pi_lower = lp, pi_upper = up, error = err, pct_error = pct_err) %>%
  arrange(age_group, window, year) %>%
  write_csv("output/sensitivity_cv_2018_2019.csv")

# Summary ====
# ~~~~~~~~~~~~
sink("output/sensitivity_summary.txt")
cat("Sensitivity analyses (code/10_sensitivity.R), run", format(Sys.Date()), "\n\n")

cat("== (1) US excess deaths per 100,000 by age and year: range across the three fitting windows ==\n")
cat("   main = 2010-2019 (as in the paper); pi_excl_0 = number of windows (of 3)\n")
cat("   whose 95% prediction interval excludes zero\n\n")
sens %>%
  group_by(year, age_group) %>%
  summarise(main = r_exc[main],
            min  = min(r_exc), max = max(r_exc),
            ci_excl_0 = sum(exc_ci_lb > 0 | exc_ci_ub < 0),
            pi_excl_0 = sum(exc_pi_lb > 0 | exc_pi_ub < 0),
            .groups = "drop") %>%
  mutate(across(c(main, min, max), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (1b) US excess deaths (counts) with 95% CI, ages 15-19 and 20-24, by specification ==\n\n")
sens %>%
  filter(age %in% c(15, 20)) %>%
  mutate(cell = sprintf("%6.0f (%6.0f, %6.0f)", exc, exc_ci_lb, exc_ci_ub)) %>%
  select(age_group, window, year, cell) %>%
  pivot_wider(names_from = year, values_from = cell) %>%
  arrange(age_group, window) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (1c) Pre-pandemic trend: annual % change in the death rate implied by the log-linear fit, by window ==\n\n")
slopes %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (1d) Ages 20-24: observed deaths and the three baselines, 2020-2023 ==\n\n")
fits %>%
  filter(age == 20, year >= 2020) %>%
  mutate(bsn = round(bsn)) %>%
  select(window, year, dts, bsn) %>%
  pivot_wider(names_from = year, values_from = c(dts, bsn)) %>%
  select(window, observed_2020 = dts_2020, starts_with("bsn")) %>%
  rename_with(~ sub("bsn_", "baseline_", .x)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("   observed deaths 20-24:", paste(fits %>% filter(age == 20, year >= 2020, main) %>% pull(dts), collapse = " / "), "(2020-2023)\n")

cat("\n== (2) State excess rate 0-24: baseline on the aggregate vs sum of age-specific baselines ==\n\n")
st_cmp %>%
  group_by(year) %>%
  summarise(corr = cor(exc_rate_aggregate, exc_rate_sum_ages),
            median_abs_diff = median(abs(diff_rate)),
            max_abs_diff = max(abs(diff_rate)),
            state_max = state[which.max(abs(diff_rate))],
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (3) States (51) by significance of excess 0-24: CI of the baseline vs prediction interval ==\n\n")
st_sig %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (4) Regional excess rate 0-24 with 95% CI: sum of state bounds (Figure 3) vs baseline fitted on regional totals ==\n\n")
reg_cmp %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(year, bea_region) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n== (5) Out-of-sample check: 2018-2019 predicted from log-linear baselines fitted to windows ending in 2017 ==\n")
cat("   pct_error = 100 * (observed - predicted) / observed; positive = under-prediction\n\n")
cv %>%
  mutate(predicted = round(bsn), pct_error = round(pct_err, 1)) %>%
  select(age_group, window, year, observed = dts, predicted, pct_error) %>%
  pivot_wider(names_from = year, values_from = c(observed, predicted, pct_error)) %>%
  select(age_group, window, observed_2018, predicted_2018, pct_error_2018,
         observed_2019, predicted_2019, pct_error_2019) %>%
  arrange(age_group, window) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n   mean absolute % error over 2018-2019 by age group and window (best window marked *):\n\n")
cv_summary %>%
  group_by(age_group) %>%
  mutate(best = ifelse(mape == min(mape), "*", "")) %>%
  ungroup() %>%
  mutate(mape = sprintf("%.1f%s", mape, best)) %>%
  select(age_group, window, mape) %>%
  pivot_wider(names_from = window, values_from = mape) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n   all ages combined (7 groups x 2 years): \n\n")
cv %>%
  group_by(window) %>%
  summarise(mape = mean(abs(pct_err)), bias_pct = mean(pct_err),
            ci_cover = mean(in_ci), pi_cover = mean(in_pi), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

sink()

cat(readLines("output/sensitivity_summary.txt"), sep = "\n")
