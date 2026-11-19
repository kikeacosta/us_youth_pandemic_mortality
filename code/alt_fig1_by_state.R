# Alternative version of Figure 1 stratified by state instead of age: total,
# direct and indirect p-scores of children and young people (ages 0-24) by state
# and year, 2020-2023, directly age-standardised to the US 0-24 population of
# 2020 (six age groups, as in script 11).
#
#   B. one panel per year, states on the y axis grouped by BEA region (US on
#      top); stacked bars split the age-standardised p-score into a direct
#      (COVID-19 deaths / expected, red) and an indirect ((excess - COVID-19
#      deaths) / expected, black) part, which add up to the total p-score (grey
#      point with the 95% CI of the baseline). The p-scores are on a linear
#      scale so that the two parts stack.
#
# Input : data_inter/cyp_vs_all_ages_by_state.rds (script 11)
# Output: figures/alternatives/fig1_altB_total_direct_indirect_by_state.png (+ .csv)

source("code/00_functions.R")

# Data: age-standardised CYP p-scores (total, direct, indirect) by state ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- read_rds("data_inter/cyp_vs_all_ages_by_state.rds")

# state order: US first, then BEA regions west to east, states alphabetically
# by abbreviation within region (top to bottom)
st_order <-
  bea_lookup %>%
  mutate(bea_region = factor(bea_region, levels = west_to_east)) %>%
  arrange(bea_region, abb)

alt <-
  res %>%
  left_join(bea_lookup %>% select(state, abb, bea_region), by = "state") %>%
  mutate(abb        = ifelse(state == "us", "US", abb),
         bea_region = ifelse(state == "us", "United States", as.character(bea_region)),
         bea_region = factor(bea_region, levels = c("United States", west_to_east)),
         abb        = factor(abb, levels = rev(c("US", st_order$abb)))) %>%
  select(state, abb, bea_region, year,
         total = cyp_pscore, total_lb = cyp_pscore_lb, total_ub = cyp_pscore_ub,
         direct = cyp_psc_direct, direct_lb = cyp_psc_direct_lb, direct_ub = cyp_psc_direct_ub,
         indirect = cyp_psc_indirect, indirect_lb = cyp_psc_indirect_lb, indirect_ub = cyp_psc_indirect_ub)

stopifnot(nrow(alt) == 52 * 4, !anyNA(alt$abb), !anyNA(alt$bea_region),
          all(abs(alt$total - alt$direct - alt$indirect) < 1e-9))

# styling: same colours as fig1_altA
comp_cols <- c(total = "grey50", direct = "#B2182B", indirect = "black")
x_lims    <- c(-40, 50)    # whiskers beyond the limits are cut at the panel edge

bars <-
  alt %>%
  select(abb, bea_region, year, direct, indirect) %>%
  pivot_longer(c(direct, indirect), names_to = "comp", values_to = "v") %>%
  bind_rows(alt %>% transmute(abb, bea_region, year, comp = "total", v = 0)) %>%   # legend key only
  mutate(comp = factor(comp, levels = c("total", "direct", "indirect")))

p_altB <-
  ggplot(alt, aes(y = abb)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_col(data = bars, aes(x = v, fill = comp), position = "stack",
           width = 0.7, orientation = "y") +
  geom_errorbar(aes(xmin = total_lb, xmax = total_ub), width = 0, orientation = "y",
                linewidth = 0.35, colour = comp_cols["total"]) +
  geom_point(aes(x = total), size = 1.4, colour = comp_cols["total"]) +
  facet_grid(bea_region ~ year, scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = comp_cols, name = NULL,
                    labels = c(total = "Total p-score (95% CI)",
                               direct = "Direct: registered COVID-19 deaths",
                               indirect = "Indirect: excess - registered COVID-19 deaths")) +
  scale_x_continuous(breaks = seq(-20, 40, 20), labels = function(x) sprintf("%+.0f%%", x),
                     minor_breaks = seq(-40, 50, 10)) +
  coord_cartesian(xlim = x_lims, expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(
    colour = c(comp_cols["total"], NA, NA), shape = NA))) +
  labs(x = NULL, y = NULL) +
  theme_bars() +
  theme(strip.text.y.left  = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 7,
                                          face = "bold", margin = margin(r = 3)),
        strip.text.x       = element_text(size = 9, face = "bold", hjust = 0),
        axis.text.y        = element_text(size = 6.5),
        panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
        panel.grid.minor.x = element_line(colour = "grey94", linewidth = 0.2),
        panel.spacing.y    = unit(2, "pt"),
        legend.position    = "bottom", legend.text = element_text(size = 8),
        legend.key.size    = unit(9, "pt"),
        plot.caption       = element_text(size = 7, hjust = 0)) +
  labs(caption = paste0("P-scores of ages 0-24, directly age-standardised to the US 0-24 population of 2020 (six age groups). ",
                        "Total = observed / expected - 1 (point, 95% CI of the baseline);\n",
                        "direct = registered COVID-19 deaths / expected; indirect = (excess - registered COVID-19 deaths) / expected; ",
                        "direct + indirect = total. Intervals cut at ", sprintf("%+.0f%%", x_lims[1]), " and ",
                        sprintf("%+.0f%%", x_lims[2]), "."))

ggsave("figures/alternatives/fig1_altB_total_direct_indirect_by_state.png", p_altB,
       width = 9, height = 9.5, dpi = 300)

alt %>%
  mutate(state = ifelse(state == "us", "US", state),
         across(total:indirect_ub, ~ round(.x, 2))) %>%
  arrange(year, bea_region, desc(abb)) %>%
  write_csv("figures/alternatives/fig1_altB_total_direct_indirect_by_state.csv")
