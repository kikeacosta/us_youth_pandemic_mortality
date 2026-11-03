# Alternative versions of Figure 1 (COVID-19 and excess deaths by age group and
# year, US, 2020-2023). Each variant is one block below and writes its own
# png (+ csv) to figures/alternatives/; which variant goes into the paper is
# decided outside this script (see README.md).
#
#   A. total, direct and indirect p-scores by age (three columns of points on a
#      log ratio axis sharing one scale, panel widths proportional to range) plus
#      a fourth column of stacked bars with the direct and
#      indirect excess rates per 100,000
#
# Input : data_inter/excess_age.rds (script 04)
#         data_inter/fig1_age_data.rds (script 05; COVID-19 deaths by age)
# Output: figures/alternatives/fig1_altA_total_direct_indirect_by_age.png (+ .csv)

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

# A: total, direct and indirect p-scores by age, plus stacked rate bars ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same decomposition as the state maps (script 11): total = observed / expected,
# direct = 1 + COVID-19 deaths / expected (the ratio if COVID-19 deaths were the
# only deviation), indirect = (observed - COVID-19 deaths) / expected. Each
# interval carries the 95% CI of the baseline (division by uc and lc). The axis is
# a log ratio scale labelled as p-scores, common to the three columns: each
# panel's limits are its own data range (plus a fixed margin) and its width is
# proportional to that range, so a given distance means the same ratio
# everywhere and the direct column, never above +5%, is drawn narrow.
comp_pts <-
  bind_rows(alt %>% transmute(year, age_group, comp = "total",
                              v = dts / bsn, v_lb = dts / uc, v_ub = dts / lc),
            alt %>% transmute(year, age_group, comp = "direct",
                              v = 1 + c19 / bsn, v_lb = 1 + c19 / uc, v_ub = 1 + c19 / lc),
            alt %>% transmute(year, age_group, comp = "indirect",
                              v = (dts - c19) / bsn, v_lb = (dts - c19) / uc, v_ub = (dts - c19) / lc)) %>%
  mutate(comp = factor(comp, levels = c("total", "direct", "indirect")))

comp_cols   <- c(total = "grey50", direct = unname(cause_cols["c19"]), indirect = "black")   # same colours in every column
comp_titles <- c(total = "Total", direct = "Direct", indirect = "Indirect")   # definitions in the caption
comp_rng <-
  comp_pts %>%
  reframe(lo = min(log10(v_lb)), hi = max(log10(v_ub)), .by = comp) %>%
  mutate(lo = pmin(lo, 0) - 0.012, hi = hi + 0.012, w = hi - lo)   # include the 1 line; margin ~ 3%
comp_w <- setNames(comp_rng$w, as.character(comp_rng$comp))
comp_breaks <- list(total = c(0.8, 0.9, 1, 1.1, 1.2, 1.3), direct = c(1, 1.05),
                    indirect = c(0.8, 0.9, 1, 1.1, 1.2, 1.3))

comp_panel <- function(k, first = FALSE){
  d <- comp_pts %>% filter(comp == k)
  r <- comp_rng %>% filter(comp == k)
  ggplot(d, aes(x = v, y = age_group)) +
    geom_vline(xintercept = 1, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(aes(xmin = v_lb, xmax = v_ub), width = 0.3, orientation = "y",
                  linewidth = 0.4, colour = comp_cols[k]) +
    geom_point(size = 2.2, colour = comp_cols[k], shape = pt_shapes[c(total = "excess", direct = "c19", indirect = "excess")[k]]) +
    facet_grid(year ~ ., switch = "y") +
    scale_x_continuous(transform = "log10", breaks = comp_breaks[[k]], labels = pscore_lab,
                       minor_breaks = NULL, limits = 10^c(r$lo, r$hi), expand = expansion(0)) +
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
  bind_rows(alt %>% transmute(year, age_group, comp = "total",    v = 0),   # zero-width bars: legend key only
            alt %>% transmute(year, age_group, comp = "direct",   v = 1e5 * c19 / pop),
            alt %>% transmute(year, age_group, comp = "indirect", v = 1e5 * (exc - c19) / pop)) %>%
  mutate(comp = factor(comp, levels = c("total", "direct", "indirect")))

p_bars <-
  ggplot(rate_bars, aes(x = v, y = age_group, fill = comp)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_col(position = "stack", width = 0.7, orientation = "y") +
  facet_grid(year ~ ., switch = "y") +
  scale_fill_manual(values = comp_cols, name = NULL,
                    labels = c(total = "Total: excess deaths", direct = "Direct: COVID-19 deaths",
                               indirect = "Indirect: excess - COVID-19 deaths")) +
  scale_x_continuous(breaks = seq(-40, 40, 10), minor_breaks = seq(-40, 40, 5), expand = expansion(mult = 0.1)) +
  theme(panel.grid.minor.x = element_line(colour = "grey92", linewidth = 0.25)) +
  labs(x = NULL, y = NULL, title = "Deaths per 100,000") +
  alt_theme(first = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p_altA <- comp_panel("total", first = TRUE) + comp_panel("direct") + comp_panel("indirect") + p_bars +
  plot_layout(nrow = 1, guides = "collect", widths = unname(comp_w[c("total", "direct", "indirect", "total")])) +
  plot_annotation(caption = paste0("Total = observed / expected, direct = 1 + COVID-19 deaths / expected, indirect = (observed - COVID-19) / expected;\n",
                                   "p-scores on one common log ratio scale (panel width proportional to range), 95% CI of the baseline"),
                  theme = theme(plot.caption = element_text(size = 7, hjust = 0))) &
  theme(legend.position = "bottom")

ggsave("figures/alternatives/fig1_altA_total_direct_indirect_by_age.png", p_altA,
       width = 10.5, height = 1.3 * n_distinct(alt$year) + 1.2, dpi = 300)

bind_rows(comp_pts %>%
            mutate(panel = "ratio_to_expected",
                   pscore_pct = 100 * (v - 1), pscore_lb = 100 * (v_lb - 1), pscore_ub = 100 * (v_ub - 1)),
          rate_bars %>% filter(comp != "total") %>% mutate(panel = "rate_per_100k")) %>%
  select(panel, component = comp, year, age_group, value = v, lower = v_lb, upper = v_ub,
         pscore_pct, pscore_lb, pscore_ub) %>%
  arrange(panel, component, year, age_group) %>%
  write_csv("figures/alternatives/fig1_altA_total_direct_indirect_by_age.csv")
