# Alternative versions of Figure 2 (maps by state and year, 2020-2023). Each
# variant is one block below and writes its own png to figures/alternatives/;
# which variant goes into the paper is decided outside this script (see
# README.md). All quantities are computed in script 11; the map helpers are in
# code/alt_00_map_helpers.R.
#
#   A. age-standardised p-scores of CYP and adults (25+, one group) and their
#      difference (CYP minus adults), common colour scale
#   B. age-standardised CYP p-score split into total (excess / expected), direct
#      (COVID-19 deaths / expected) and indirect ((excess - COVID-19) / expected),
#      the three maps on one common blue-red log ratio scale
#
# Input : data_inter/cyp_vs_all_ages_by_state.rds (script 11)
#         data_inter/us_states_cb_2022.rds (script 06)
# Output: figures/alternatives/fig2_altA_pscores_by_state.png (+ .csv)
#         figures/alternatives/fig2_altB_cyp_pscore_direct_indirect_by_state.png (+ .csv)

source("code/00_functions.R")
source("code/alt_00_map_helpers.R")

res <- read_rds("data_inter/cyp_vs_all_ages_by_state.rds")

map_dat <-
  states_sf %>%
  inner_join(res %>% filter(state != "us"), by = c("NAME" = "state")) %>%
  # A: CYP minus adult p-score, in percentage points
  mutate(cyp_minus_adult_pp = cyp_pscore - adult_pscore) %>%
  # B: fill on a log ratio scale (observed / expected), labelled as p-scores, so
  # that +10% and -10% are the same distance from white
  mutate(across(c(cyp_pscore, cyp_psc_direct, cyp_psc_indirect),
                ~ log10(1 + .x / 100), .names = "{.col}_log"))

stopifnot(nrow(map_dat) == 51 * 4)

# A: CYP and adult p-scores and their difference, common colour limits ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lim_a <- 40
p_a <-
  draw_pscore_map(map_dat, "cyp_pscore",   "p-score, CYP 0–24 (age-standardised)", lim_a, "%") +
  draw_pscore_map(map_dat, "adult_pscore", "p-score, adults ages 25+", lim_a, "%", show_years = FALSE) +
  draw_pscore_map(map_dat, "cyp_minus_adult_pp", "p-score difference, CYP minus adults", lim_a,
                  "%", show_years = FALSE) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/alternatives/fig2_altA_pscores_by_state.png", p_a,
       width = 8, height = fig_h(3, 8), dpi = 300)

res %>%
  mutate(cyp_minus_adult_pp = cyp_pscore - adult_pscore) %>%
  select(state, year, cyp_pscore, cyp_pscore_lb, cyp_pscore_ub,
         adult_pscore, adult_pscore_lb, adult_pscore_ub, cyp_minus_adult_pp) %>%
  arrange(state != "us", state, year) %>%
  write_csv("figures/alternatives/fig2_altA_pscores_by_state.csv")

# B: total, direct and indirect on one common scale ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The three components on the blue-red log ratio scale clipped at +-30%, one
# legend; the direct component (never above 5%) reads as the faintest column.
lim_log <- log10(1.3)
r_breaks_map <- c(0.8, 0.9, 1, 1.1, 1.2, 1.3)   # labelled -20 ... +30
psc_lab <- function(x) sprintf("%+.0f", (10^x - 1) * 100)
cols_b <- c("#2166AC", "white", "#B2182B")
p_b <-
  draw_component_map(map_dat, "cyp_pscore_log", "Total: excess / expected",
                     c(-lim_log, lim_log), cols_b, log10(r_breaks_map), psc_lab,
                     "p-score (%), log ratio scale") +
  draw_component_map(map_dat, "cyp_psc_direct_log", "Direct: COVID-19 deaths / expected",
                     c(-lim_log, lim_log), cols_b, log10(r_breaks_map), psc_lab,
                     "p-score (%), log ratio scale", show_years = FALSE) +
  draw_component_map(map_dat, "cyp_psc_indirect_log", "Indirect: (excess \u2212 COVID-19) / expected",
                     c(-lim_log, lim_log), cols_b, log10(r_breaks_map), psc_lab,
                     "p-score (%), log ratio scale", show_years = FALSE) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/alternatives/fig2_altB_cyp_pscore_direct_indirect_by_state.png", p_b,
       width = 8, height = fig_h(3, 8) + 0.3, dpi = 300)

res %>%
  select(state, year, cyp_pscore, cyp_pscore_lb, cyp_pscore_ub,
         cyp_psc_direct, cyp_psc_direct_lb, cyp_psc_direct_ub,
         cyp_psc_indirect, cyp_psc_indirect_lb, cyp_psc_indirect_ub,
         cyp_c19_deaths, cyp_indirect_excess) %>%
  arrange(state != "us", state, year) %>%
  write_csv("figures/alternatives/fig2_altB_cyp_pscore_direct_indirect_by_state.csv")
