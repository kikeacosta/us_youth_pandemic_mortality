# Alternative versions of Figure 2 (maps by state and year, 2020-2023). Each
# variant is one block below and writes its own png to figures/alternatives/;
# which variant goes into the paper is decided outside this script (see
# README.md). All quantities are computed in script 11; the map helpers are in
# code/alt_00_map_helpers.R.
#
#   A. age-standardised p-scores of CYP, adults and all ages, common colour scale
#   B. CYP p-score minus the US CYP p-score, and minus the state's all-age p-score
#   C. age-standardised CYP share of the state's excess deaths, of its COVID-19
#      deaths and of its indirect excess (excess - COVID-19 deaths)
#   D. age-standardised CYP p-score split into total (excess / expected), direct
#      (COVID-19 deaths / expected) and indirect ((excess - COVID-19) /
#      expected), each panel with its own scale and colours
#   E. as C, but the three maps on one common colour scale (0-10%)
#
# Input : data_inter/cyp_vs_all_ages_by_state.rds (script 11)
#         data_inter/us_states_cb_2022.rds (script 06)
# Output: figures/alternatives/fig2_altA_pscores_by_state.png
#         figures/alternatives/fig2_altB_cyp_pscore_diff_by_state.png
#         figures/alternatives/fig2_altC_cyp_share_by_state.png
#         figures/alternatives/fig2_altD_cyp_pscore_direct_indirect_by_state.png
#         figures/alternatives/fig2_altE_cyp_share_by_state_common_scale.png

source("code/00_functions.R")
source("code/alt_00_map_helpers.R")

res <- read_rds("data_inter/cyp_vs_all_ages_by_state.rds")

map_dat <-
  states_sf %>%
  inner_join(res %>% filter(state != "us"), by = c("NAME" = "state")) %>%
  # C: the shares are shown only where the all-age denominator is positive and its CI
  # excludes zero (other states grey), and a negative CYP excess counts as a
  # share of 0 (the share is "how much of the positive excess is CYP")
  mutate(cyp_share_excess_map = ifelse(all_exc_sig & exc_all_fit > 0,
                                       pmax(cyp_share_excess_std, 0), NA_real_),
         cyp_share_indirect_map = ifelse(all_ind_sig & ind_all_fit > 0,
                                         pmax(cyp_share_indirect_std, 0), NA_real_)) %>%
  # D: fill on a log ratio scale (observed / expected), labelled as p-scores, so
  # that +10% and -10% are the same distance from white
  mutate(across(c(cyp_pscore, cyp_psc_direct, cyp_psc_indirect),
                ~ log10(1 + .x / 100), .names = "{.col}_log"))

stopifnot(nrow(map_dat) == 51 * 4)

# A: three p-scores, common colour limits so the columns are comparable ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lim_a <- 40
p_a <-
  draw_pscore_map(map_dat, "cyp_pscore",   "CYP, ages 0–24 (age-standardised)", lim_a, "p-score (%)") +
  draw_pscore_map(map_dat, "adult_pscore", "Adults, ages 25+", lim_a, "p-score (%)", show_years = FALSE) +
  draw_pscore_map(map_dat, "all_pscore",   "All ages (age-standardised)", lim_a, "p-score (%)", show_years = FALSE) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/alternatives/fig2_altA_pscores_by_state.png", p_a,
       width = 8, height = fig_h(3, 8), dpi = 300)

# B: CYP p-score relative to the US CYP p-score and to the state's all-age p-score ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lim_b <- 25
p_b <-
  draw_pscore_map(map_dat, "cyp_vs_us_pp",  "CYP p-score: state minus US", lim_b, "percentage points") +
  draw_pscore_map(map_dat, "cyp_vs_all_pp", "CYP p-score minus the state's all-age p-score", lim_b,
                  "percentage points", show_years = FALSE) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/alternatives/fig2_altB_cyp_pscore_diff_by_state.png", p_b,
       width = 6.5, height = fig_h(2, 6.5), dpi = 300)

res %>%
  filter(state != "us") %>%
  reframe(across(c(cyp_pscore, adult_pscore, all_pscore, cyp_vs_us_pp, cyp_vs_all_pp),
                 list(min = ~ round(min(.x), 1), max = ~ round(max(.x), 1))),
          .by = year) %>%
  print(width = Inf)

# C: CYP shares of excess, of COVID-19 and of indirect excess deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Each map is the age-standardised share of CYP in the state's total: all-age
# excess deaths, all-age COVID-19 deaths, all-age indirect excess (excess minus
# COVID-19 deaths). The shares of excess and of indirect excess are shown only
# where the all-age denominator is positive and its CI excludes zero (grey
# otherwise); a negative CYP numerator counts as a share of 0. Reference: CYP
# are 32% of the standard population; the US shares are printed below.
lims_c <- c(0, 10)
p_c <-
  draw_share_map(map_dat, "cyp_share_excess_map",
                 "CYP share of excess deaths (%)\ngrey: all-age excess \u2264 0 or CI includes zero",
                 lims_c, "CYP % of excess deaths (0 if CYP excess < 0; clipped at 10)") +
  draw_share_map(map_dat, "cyp_share_c19_std",
                 "CYP share of COVID-19 deaths (%)\n ",
                 c(0, 2), "CYP % of COVID-19 deaths", show_years = FALSE) +
  draw_share_map(map_dat, "cyp_share_indirect_map",
                 "CYP share of indirect excess deaths (%)\ngrey: all-age indirect excess \u2264 0 or CI includes zero",
                 lims_c, "CYP % of indirect excess (0 if CYP indirect < 0; clipped at 10)", show_years = FALSE)

ggsave("figures/alternatives/fig2_altC_cyp_share_by_state.png", p_c,
       width = 9, height = fig_h(3, 9) + 0.3, dpi = 300)

res %>%
  filter(state == "us") %>%
  select(year, cyp_share_excess_std, cyp_share_c19_std, cyp_share_indirect_std) %>%
  mutate(across(-year, ~ round(.x, 2))) %>%
  print()

res %>%
  filter(state != "us") %>%
  reframe(n_states = n(), n_all_exc_sig = sum(all_exc_sig & exc_all_fit > 0),
          n_all_ind_sig = sum(all_ind_sig & ind_all_fit > 0),
          share_exc_std_med = round(median(cyp_share_excess_std[all_exc_sig & exc_all_fit > 0]), 1),
          share_exc_std_max = round(max(cyp_share_excess_std[all_exc_sig & exc_all_fit > 0]), 1),
          share_ind_std_med = round(median(cyp_share_indirect_std[all_ind_sig & ind_all_fit > 0]), 1),
          share_ind_std_max = round(max(cyp_share_indirect_std[all_ind_sig & ind_all_fit > 0]), 1),
          share_c19_std_med = round(median(cyp_share_c19_std), 2),
          share_c19_std_max = round(max(cyp_share_c19_std), 2),
          .by = year) %>%
  print(width = Inf)

# E: the three shares of C on one common colour scale ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same quantities and masks as C; one 0-10% scale for all three maps, so the
# share of COVID-19 deaths (never above 2%) reads as the faintest column.
p_e <-
  draw_share_map(map_dat, "cyp_share_excess_map",
                 "CYP share of excess deaths (%)\ngrey: all-age excess \u2264 0 or CI includes zero",
                 lims_c, "CYP % of the state's total (0 if CYP value < 0; clipped at 10)") +
  draw_share_map(map_dat, "cyp_share_c19_std",
                 "CYP share of COVID-19 deaths (%)\n ",
                 lims_c, "CYP % of the state's total (0 if CYP value < 0; clipped at 10)", show_years = FALSE) +
  draw_share_map(map_dat, "cyp_share_indirect_map",
                 "CYP share of indirect excess deaths (%)\ngrey: all-age indirect excess \u2264 0 or CI includes zero",
                 lims_c, "CYP % of the state's total (0 if CYP value < 0; clipped at 10)", show_years = FALSE) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/alternatives/fig2_altE_cyp_share_by_state_common_scale.png", p_e,
       width = 9, height = fig_h(3, 9) + 0.3, dpi = 300)

# D: total, direct and indirect ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# total and indirect on a log ratio scale clipped at +-30% (blue-red and
# green-purple), direct on a linear 0-5% sequential scale (oranges)
lim_log <- log10(1.3)
r_breaks_map <- c(0.8, 0.9, 1, 1.1, 1.2, 1.3)   # labelled -20 ... +30
psc_lab <- function(x) sprintf("%+.0f", (10^x - 1) * 100)

p_d <-
  draw_component_map(map_dat, "cyp_pscore_log", "Total: excess / expected",
                     c(-lim_log, lim_log), c("#2166AC", "white", "#B2182B"),
                     log10(r_breaks_map), psc_lab, "p-score (%), log ratio scale") +
  draw_component_map(map_dat, "cyp_psc_direct", "Direct: COVID-19 deaths / expected",
                     c(0, 5), c("white", "#FDAE6B", "#E6550D", "#7F2704"),
                     0:5, function(x) sprintf("+%.0f", x), "p-score (%), linear scale",
                     show_years = FALSE) +
  draw_component_map(map_dat, "cyp_psc_indirect_log", "Indirect: (excess − COVID-19) / expected",
                     c(-lim_log, lim_log), c("#1B7837", "white", "#762A83"),
                     log10(r_breaks_map), psc_lab, "p-score (%), log ratio scale",
                     show_years = FALSE)

ggsave("figures/alternatives/fig2_altD_cyp_pscore_direct_indirect_by_state.png", p_d,
       width = 8, height = fig_h(3, 8) + 0.3, dpi = 300)

res %>%
  filter(state != "us") %>%
  reframe(across(c(cyp_psc_direct, cyp_psc_indirect),
                 list(min = ~ round(min(.x), 1), med = ~ round(median(.x), 1), max = ~ round(max(.x), 1))),
          .by = year) %>%
  print(width = Inf)
