# Sensitivity analyses for the excess-mortality estimates (appendix).
#
# (1) Baseline specification, US by age group (and ages 0-24 combined):
#     fitting window {2010-2019, 2014-2019, 2015-2019} x trend {log-linear in
#     year, none (constant rate)}; each with the 95% CI of the baseline (as in the
#     main figures) and the 95% bootstrapped prediction interval.
#     -> figures/figS1_sensitivity_excess_by_age.png (+ .csv)
# (2) State excess 0-24: baseline fitted on the 0-24 aggregate (main analysis)
#     vs. sum of the age-specific state baselines (script 04, series b).
#     -> output/sensitivity_state_baseline_aggregation.csv
# (3) Significance of state excess with CI vs. PI.
# A text summary of (1)-(3) is written to output/sensitivity_summary.txt.
#
# Input : data_inter/ac_deaths_by_sex_age.rds (script 03)
#         data_inter/excess_states_age_u25.rds, baselines_states_sex_age.rds (script 04)

source("code/00_functions.R")

set.seed(2024)

# Baseline with a choice of fitting window and trend ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same model as est_baseline_pi() (script 00) when start_year = 2010 and
# trend = TRUE; otherwise the fitting years are restricted / the year term dropped.
est_baseline_spec <- function(chunk, start_year, trend, nsim = 200){

  chunk2 <-
    chunk %>%
    arrange(year) %>%
    mutate(w = ifelse(year >= start_year & year <= 2019, 1, 0),
           t = 1:n())

  f <- if (trend) dts ~ year + offset(log(exposure)) else dts ~ 1 + offset(log(exposure))

  model <- glm(f, weights = w, family = "quasipoisson", data = chunk2)

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
  expand_grid(start_year = c(2010, 2014, 2015),
              trend      = c(TRUE, FALSE)) %>%
  mutate(window = paste0(start_year, "-2019"),
         trend_lab = ifelse(trend, "log-linear trend", "no trend"),
         spec = paste0(window, ", ", trend_lab),
         main = start_year == 2010 & trend)          # specification used in the paper

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

sens <-
  specs %>%
  mutate(res = map2(start_year, trend, function(s, tr){
    us_all %>%
      group_by(age) %>%
      do(est_baseline_spec(chunk = .data, start_year = s, trend = tr)) %>%
      ungroup()
  })) %>%
  select(spec, window, trend_lab, main, res) %>%
  unnest(res) %>%
  filter(year >= 2020) %>%
  mutate(lp = pmax(lp, 0), up = pmax(up, 0),
         exc    = dts - bsn,
         exc_ci_lb = pmin(dts - lc, dts - uc), exc_ci_ub = pmax(dts - lc, dts - uc),
         exc_pi_lb = pmin(dts - lp, dts - up), exc_pi_ub = pmax(dts - lp, dts - up),
         across(c(exc, exc_ci_lb, exc_ci_ub, exc_pi_lb, exc_pi_ub),
                ~ 1e5 * .x / exposure, .names = "r_{.col}"),
         age_group = factor(age_labs_s[as.character(age)], levels = age_labs_s))

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
spec_levels <- rev(specs$spec)           # main specification at the top
plot_dat <-
  sens %>%
  mutate(spec = factor(spec, levels = spec_levels),
         window = factor(window, levels = unique(specs$window)),
         trend_lab = factor(trend_lab, levels = c("log-linear trend", "no trend")))

window_cols <- c(`2010-2019` = "#762A83", `2014-2019` = "#1B7837", `2015-2019` = "#E08214")

p_sens <-
  ggplot(plot_dat, aes(y = spec, colour = window, shape = trend_lab)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_linerange(aes(xmin = r_exc_pi_lb, xmax = r_exc_pi_ub), linewidth = 0.4, alpha = 0.6) +
  geom_linerange(aes(xmin = r_exc_ci_lb, xmax = r_exc_ci_ub), linewidth = 1.2) +
  geom_point(aes(x = r_exc), size = 1.8, fill = "white") +
  scale_colour_manual(values = window_cols, name = "Fitting window") +
  scale_shape_manual(values = c(`log-linear trend` = 16, `no trend` = 24), name = "Trend") +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  facet_grid(year ~ age_group, switch = "y", scales = "free_x") +
  labs(x = "Excess deaths per 100,000 (point, 95% CI of the baseline — thick; 95% prediction interval — thin)",
       y = NULL) +
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
  select(year, age_group, window, trend = trend_lab, main_specification = main,
         deaths = dts, baseline = bsn,
         excess = exc, excess_ci_lower = exc_ci_lb, excess_ci_upper = exc_ci_ub,
         excess_pi_lower = exc_pi_lb, excess_pi_upper = exc_pi_ub,
         excess_rate = r_exc, excess_rate_ci_lower = r_exc_ci_lb, excess_rate_ci_upper = r_exc_ci_ub,
         excess_rate_pi_lower = r_exc_pi_lb, excess_rate_pi_upper = r_exc_pi_ub) %>%
  arrange(year, age_group, window, trend) %>%
  write_csv("figures/figS1_sensitivity_excess_by_age.csv")

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

# Summary ====
# ~~~~~~~~~~~~
sink("output/sensitivity_summary.txt")
cat("Sensitivity analyses (code/10_sensitivity.R), run", format(Sys.Date()), "\n\n")

cat("== (1) US excess deaths per 100,000 by age and year: range across the six baseline specifications ==\n")
cat("   main = 2010-2019, log-linear trend (as in the paper); pi_excl_0 = number of specifications (of 6)\n")
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

cat("\n== (1b) US excess deaths (counts), ages 15-19 and 20-24, by specification ==\n\n")
sens %>%
  filter(age %in% c(15, 20)) %>%
  mutate(exc = round(exc)) %>%
  select(age_group, spec, year, exc) %>%
  pivot_wider(names_from = year, values_from = exc) %>%
  arrange(age_group, spec) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

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

sink()

cat(readLines("output/sensitivity_summary.txt"), sep = "\n")
