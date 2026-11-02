# Alternative versions of Figure 1 (COVID-19 and excess deaths by age group and
# year, US, 2020-2023). Each variant is one block below and writes its own
# png (+ csv) to figures/alternatives/; which variant goes into the paper is
# decided outside this script (see README.md).
#
#   A. three columns of points: death rates per 100,000; observed / expected on
#      a log ratio axis labelled as p-scores; COVID-19 as % of excess deaths
#   B. total, direct and indirect p-scores by age (three columns of points on a
#      log ratio axis) plus a fourth column of stacked bars with the direct and
#      indirect excess rates per 100,000
#
# Input : data_inter/excess_age.rds (script 04)
#         data_inter/fig1_age_data.rds (script 05; COVID-19 deaths by age)
# Output: figures/alternatives/fig1_altA_pscore_by_age.png (+ .csv)
#         figures/alternatives/fig1_altB_total_direct_indirect_by_age.png (+ .csv)

source("code/00_functions.R")
library(patchwork)

# Data: excess (with the CI of the baseline) and COVID-19 deaths, ages 0-24 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exc <- read_rds("data_inter/excess_age.rds")

c19 <-
  read_rds("data_inter/fig1_age_data.rds")$all %>%
  filter(cause == "c19") %>%
  select(year, age, c19 = dx)

alt <-
  exc %>%
  filter(age <= 20) %>%
  select(year, age, dts, bsn, lc, uc, pop = exposure) %>%
  left_join(c19, by = c("year", "age")) %>%
  mutate(exc    = dts - bsn,
         exc_lb = dts - uc,
         exc_ub = dts - lc,
         age_group = factor(age_labs[as.character(age)], levels = age_labs))

stopifnot(nrow(alt) == 6 * 4, !anyNA(alt$c19))

# shared styling (same colours as the main Figure 1)
cause_cols <- c(c19 = "#B2182B", excess = "#762A83", ratio = "#FD8D3C")
pt_shapes  <- c(c19 = 17, excess = 16, ratio = 15)
pscore_lab <- function(x) sprintf("%+.0f%%", (x - 1) * 100)

alt_theme <- function(first = FALSE){
  theme_bars() +
    theme(plot.title = element_text(size = 9, hjust = 0, margin = margin(b = 2)),
          legend.position = "bottom", legend.direction = "vertical",
          legend.text = element_text(size = 8),
          legend.key.height = unit(8, "pt"),
          legend.margin = margin(0, 0, 0, 0)) +
    if (first) theme() else theme(strip.text.y = element_blank(),
                                  strip.text.y.left = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank())
}

# A: points, with the p-score on a log ratio axis ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three columns, dots for excess deaths (with the 95% CI of the baseline) and
# triangles for COVID-19 deaths:
#   1. death rates per 100,000 (as in the main figure, as points)
#   2. observed / expected deaths on a log scale, labelled as p-scores
#      ((ratio - 1) * 100), so that a 10% excess (1.1) and a 10% deficit (0.9)
#      are the same distance from 1. Excess: observed / baseline, CI from
#      observed / uc and observed / lc. COVID-19: 1 + c19 / baseline, i.e. the
#      ratio that would be observed if COVID-19 deaths were the only deviation.
#   3. COVID-19 deaths as % of excess deaths, with the interval implied by the
#      CI of the excess (c19 / upper excess, c19 / lower excess). Where that CI
#      includes zero the ratio is undefined and its interval unbounded: the
#      point is drawn hollow and without an interval.

# column 1: rates
rate_pts <-
  bind_rows(alt %>% transmute(year, age_group, cause = "c19", v = 1e5 * c19 / pop,
                              v_lb = NA_real_, v_ub = NA_real_),
            alt %>% transmute(year, age_group, cause = "excess", v = 1e5 * exc / pop,
                              v_lb = 1e5 * exc_lb / pop, v_ub = 1e5 * exc_ub / pop)) %>%
  # an empty "ratio" row so that the legend of the first panel lists all three keys
  bind_rows(tibble(year = 2020, age_group = factor("0-1", levels = age_labs), cause = "ratio",
                   v = NA_real_, v_lb = NA_real_, v_ub = NA_real_)) %>%
  mutate(cause = factor(cause, levels = c("c19", "excess", "ratio")))

# column 2: ratios to expected
psc_pts <-
  bind_rows(alt %>% transmute(year, age_group, cause = "c19", v = 1 + c19 / bsn,
                              v_lb = NA_real_, v_ub = NA_real_),
            alt %>% transmute(year, age_group, cause = "excess", v = dts / bsn,
                              v_lb = dts / uc, v_ub = dts / lc))

# column 3: COVID-19 / excess (%), interval from the CI of the excess
ratio_pts <-
  alt %>%
  transmute(year, age_group, cause = "ratio",
            v = 100 * c19 / exc,
            undefined = exc_lb <= 0 & exc_ub >= 0,
            b1 = 100 * c19 / exc_ub,
            b2 = 100 * c19 / exc_lb,
            v_lb = ifelse(undefined, NA_real_, pmin(b1, b2)),
            v_ub = ifelse(undefined, NA_real_, pmax(b1, b2))) %>%
  select(-b1, -b2)

pt_labs <- c(c19    = "COVID-19 deaths",
             excess = "Excess deaths (95% CI of the baseline)",
             ratio  = "COVID-19 / excess deaths (interval implied by the CI of the excess;
hollow: the CI of the excess includes zero and the interval is unbounded)")
pt_cols  <- cause_cols[c("c19", "excess", "ratio")]
r_breaks <- c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)

base_pts <- function(d, ref, first = FALSE){
  ggplot(d, aes(x = v, y = age_group)) +
    geom_vline(xintercept = ref, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(aes(xmin = v_lb, xmax = v_ub, colour = cause), width = 0.3,
                  orientation = "y", linewidth = 0.4, na.rm = TRUE, show.legend = FALSE) +
    facet_grid(year ~ ., switch = "y") +
    labs(x = NULL, y = NULL) +
    alt_theme(first)
}

p1 <-
  base_pts(rate_pts, 0, first = TRUE) +
  geom_point(aes(colour = cause, shape = cause), size = 2.2, na.rm = TRUE) +
  scale_colour_manual(values = pt_cols, labels = pt_labs, name = NULL, drop = FALSE) +
  scale_shape_manual(values = pt_shapes, labels = pt_labs, name = NULL, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  labs(title = "Deaths per 100,000")

p2 <-
  base_pts(psc_pts, 1) +
  geom_point(aes(colour = cause, shape = cause), size = 2.2, show.legend = FALSE) +
  scale_colour_manual(values = pt_cols) +
  scale_shape_manual(values = pt_shapes) +
  scale_x_continuous(transform = "log10", breaks = r_breaks, labels = pscore_lab,
                     minor_breaks = NULL, expand = expansion(mult = 0.08)) +
  labs(title = "Deaths relative to expected (p-score)")

p3 <-
  base_pts(ratio_pts, 0) +
  geom_point(aes(colour = cause, shape = cause), size = 2.2, show.legend = FALSE) +
  geom_point(data = ratio_pts %>% filter(undefined), shape = 22, size = 2.2,
             colour = cause_cols["ratio"], fill = "white") +
  scale_colour_manual(values = pt_cols) +
  scale_shape_manual(values = pt_shapes) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  coord_cartesian(xlim = c(-50, 160)) +
  labs(title = "COVID-19 / excess deaths (%)")

p_altA <- (p1 + p2 + p3) + plot_layout(widths = c(1, 1.4, 1), guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/alternatives/fig1_altA_pscore_by_age.png", p_altA,
       width = 8, height = 1.3 * n_distinct(alt$year) + 1.1, dpi = 300)

bind_rows(rate_pts %>% filter(!is.na(v)) %>% mutate(panel = "rate_per_100k"),
          psc_pts %>% mutate(panel = "ratio_to_expected",
                             pscore_pct = (v - 1) * 100),
          ratio_pts %>% mutate(panel = "c19_over_excess_pct")) %>%
  select(panel, year, age_group, cause, value = v, lower = v_lb, upper = v_ub,
         pscore_pct, interval_undefined = undefined) %>%
  arrange(panel, year, cause, age_group) %>%
  write_csv("figures/alternatives/fig1_altA_pscore_by_age.csv")

# B: total, direct and indirect p-scores by age, plus stacked rate bars ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same decomposition as the state maps (script 11): total = observed / expected,
# direct = 1 + COVID-19 deaths / expected (the ratio if COVID-19 deaths were the
# only deviation), indirect = (observed - COVID-19 deaths) / expected. Each
# interval carries the 95% CI of the baseline (division by uc and lc). The axis is
# a log ratio scale labelled as p-scores; each column has its own range.
comp_pts <-
  bind_rows(alt %>% transmute(year, age_group, comp = "total",
                              v = dts / bsn, v_lb = dts / uc, v_ub = dts / lc),
            alt %>% transmute(year, age_group, comp = "direct",
                              v = 1 + c19 / bsn, v_lb = 1 + c19 / uc, v_ub = 1 + c19 / lc),
            alt %>% transmute(year, age_group, comp = "indirect",
                              v = (dts - c19) / bsn, v_lb = (dts - c19) / uc, v_ub = (dts - c19) / lc)) %>%
  mutate(comp = factor(comp, levels = c("total", "direct", "indirect")))

comp_cols   <- c(total = "grey50", direct = unname(cause_cols["c19"]), indirect = "black")   # same colours in every column
comp_titles <- c(total    = "Total: observed / expected",
                 direct   = "Direct: 1 + COVID-19 deaths / expected",
                 indirect = "Indirect: (observed - COVID-19) / expected")
comp_breaks <- list(total = c(0.8, 0.9, 1, 1.1, 1.2, 1.3), direct = c(1, 1.01, 1.02, 1.03, 1.04, 1.05),
                    indirect = c(0.8, 0.9, 1, 1.1, 1.2, 1.3))

comp_panel <- function(k, first = FALSE){
  d <- comp_pts %>% filter(comp == k)
  ggplot(d, aes(x = v, y = age_group)) +
    geom_vline(xintercept = 1, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(aes(xmin = v_lb, xmax = v_ub), width = 0.3, orientation = "y",
                  linewidth = 0.4, colour = comp_cols[k]) +
    geom_point(size = 2.2, colour = comp_cols[k], shape = pt_shapes[c(total = "excess", direct = "c19", indirect = "excess")[k]]) +
    facet_grid(year ~ ., switch = "y") +
    scale_x_continuous(transform = "log10", breaks = comp_breaks[[k]], labels = pscore_lab,
                       minor_breaks = NULL, expand = expansion(mult = 0.1)) +
    labs(x = NULL, y = NULL, title = comp_titles[k]) +
    alt_theme(first) +
    theme(legend.position = "none")
}

# fourth column: the same split as rates per 100,000 (direct = COVID-19 deaths,
# indirect = excess - COVID-19 deaths), stacked bars on a linear scale: a
# negative indirect rate stacks to the left of zero while the direct one stacks
# to the right, so the right-hand end of the bar is the total excess rate when
# both parts are positive. (A pseudo-log axis was tried and set aside: stacked
# segments do not add up on a transformed axis.)
rate_bars <-
  bind_rows(alt %>% transmute(year, age_group, comp = "direct",   v = 1e5 * c19 / pop),
            alt %>% transmute(year, age_group, comp = "indirect", v = 1e5 * (exc - c19) / pop)) %>%
  mutate(comp = factor(comp, levels = c("direct", "indirect")))

p_bars <-
  ggplot(rate_bars, aes(x = v, y = age_group, fill = comp)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_col(position = "stack", width = 0.7, orientation = "y") +
  facet_grid(year ~ ., switch = "y") +
  scale_fill_manual(values = comp_cols[c("direct", "indirect")], name = NULL,
                    labels = c(direct = "Direct: COVID-19 deaths", indirect = "Indirect: excess - COVID-19 deaths")) +
  scale_x_continuous(breaks = seq(-40, 40, 10), minor_breaks = seq(-40, 40, 5), expand = expansion(mult = 0.1)) +
  theme(panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.25)) +
  labs(x = NULL, y = NULL, title = "Deaths per 100,000") +
  alt_theme(first = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p_altB <- comp_panel("total", first = TRUE) + comp_panel("direct") + comp_panel("indirect") + p_bars +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(caption = "p-scores on a log ratio scale (observed / expected - 1), 95% CI of the baseline; each column has its own range",
                  theme = theme(plot.caption = element_text(size = 7, hjust = 0))) &
  theme(legend.position = "bottom")

ggsave("figures/alternatives/fig1_altB_total_direct_indirect_by_age.png", p_altB,
       width = 10.5, height = 1.3 * n_distinct(alt$year) + 1.2, dpi = 300)

bind_rows(comp_pts %>%
            mutate(panel = "ratio_to_expected",
                   pscore_pct = 100 * (v - 1), pscore_lb = 100 * (v_lb - 1), pscore_ub = 100 * (v_ub - 1)),
          rate_bars %>% mutate(panel = "rate_per_100k")) %>%
  select(panel, component = comp, year, age_group, value = v, lower = v_lb, upper = v_ub,
         pscore_pct, pscore_lb, pscore_ub) %>%
  arrange(panel, component, year, age_group) %>%
  write_csv("figures/alternatives/fig1_altB_total_direct_indirect_by_age.csv")
