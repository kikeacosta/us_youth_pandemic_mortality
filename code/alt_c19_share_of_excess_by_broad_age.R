# Registered COVID-19 deaths as a share of excess deaths, US, 2020-2023:
# children and young people (ages 0-24 combined) vs adults (25+).
#
# Share = registered COVID-19 deaths / excess deaths (in %). Excess comes from
# one baseline fitted to each aggregate (0-24 and 25+, US, same log-linear
# quasi-Poisson model as everywhere else), so both groups are treated alike.
# The 95% interval carries the CI of the baseline: COVID-19 / (observed - lc)
# and COVID-19 / (observed - uc); when the lower excess bound is <= 0 the upper
# bound of the share is unbounded (drawn as a triangle at the top of the panel).
# A share above 100% means that registered COVID-19 deaths exceed the excess
# (negative indirect excess); share = direct / total p-score of the
# decomposition figures.
#
# Input : data_inter/excess_states_age_u25.rds (script 04; state == "us", 0-24 fit)
#         data_inter/excess_states_broad_age.rds (script 04; state == "us", 25+ fit)
#         data_inter/fig1_age_data.rds (script 05; national COVID-19 deaths by age)
# Output: figures/alternatives/c19_share_of_excess_u25_vs_25plus.png (+ .csv)

source("code/00_functions.R")

# Data ====
# ~~~~~~~~~
exc <-
  bind_rows(read_rds("data_inter/excess_states_age_u25.rds") %>%
              filter(state == "us") %>% mutate(grp = "0-24"),
            read_rds("data_inter/excess_states_broad_age.rds") %>%
              filter(state == "us", age_grp == "25+") %>% mutate(grp = "25+")) %>%
  filter(year >= 2020) %>%
  select(grp, year, dts, bsn, lc, uc)

c19 <-
  read_rds("data_inter/fig1_age_data.rds")$all %>%
  filter(cause == "c19") %>%
  mutate(grp = ifelse(age < 25, "0-24", "25+")) %>%
  reframe(c19 = sum(dx), .by = c(grp, year))

share <-
  exc %>%
  inner_join(c19, by = c("grp", "year")) %>%
  mutate(exc    = dts - bsn,
         exc_lb = dts - uc,
         exc_ub = dts - lc,
         share    = 100 * c19 / exc,
         share_lb = 100 * c19 / exc_ub,
         share_ub = ifelse(exc_lb > 0, 100 * c19 / exc_lb, Inf),
         grp = factor(grp, levels = c("0-24", "25+")))

stopifnot(nrow(share) == 8, all(share$exc > 0))

# Figure ====
# ~~~~~~~~~~~
y_top <- 150      # intervals beyond this are cut and drawn as arrows
grp_cols <- c(`0-24` = "#B2182B", `25+` = "grey40")
dodge <- position_dodge(width = 0.45)

plot_d <-
  share %>%
  mutate(cut   = share_ub > y_top,
         ub_pl = pmin(share_ub, y_top),
         lab   = sprintf("%s / %s", comma(c19), comma(round(exc))))

p <-
  ggplot(plot_d, aes(x = factor(year), y = share, colour = grp)) +
  geom_hline(yintercept = 100, linewidth = 0.3, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(ymin = share_lb, ymax = ub_pl), position = dodge, linewidth = 0.5) +
  geom_point(data = mutate(plot_d, y_arr = ifelse(cut, y_top, NA)),
             aes(y = y_arr), position = dodge, shape = 17, size = 2.4,
             show.legend = FALSE, na.rm = TRUE) +
  geom_point(position = dodge, size = 2.4) +
  geom_text(aes(label = lab, y = share), position = position_dodge(width = 0.45),
            hjust = -0.15, size = 2.4, show.legend = FALSE) +
  scale_colour_manual(values = grp_cols, name = NULL,
                      labels = c(`0-24` = "Ages 0-24", `25+` = "Ages 25+")) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
  scale_y_continuous(breaks = seq(0, y_top, 25), labels = function(x) paste0(x, "%"),
                     limits = c(0, y_top), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Registered COVID-19 deaths / excess deaths",
       caption = paste0("Excess from one baseline per group (US, 0-24 and 25+, fitted to 2010-2019); 95% CI of the baseline. ",
                        "\nLabels: COVID-19 deaths / excess deaths. ",
                        "Triangle: the lower bound of the excess is <= 0, so the share has no upper bound.\n",
                        "Dashed line: COVID-19 deaths = excess (above it the indirect excess is negative).")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 8), axis.title.y = element_text(size = 9),
        legend.position = "bottom", legend.text = element_text(size = 8),
        plot.caption = element_text(size = 7, hjust = 0))

ggsave("figures/alternatives/c19_share_of_excess_u25_vs_25plus.png", p,
       width = 7, height = 4.5, dpi = 300)

share %>%
  select(group = grp, year, observed = dts, expected = bsn, excess = exc,
         excess_lb = exc_lb, excess_ub = exc_ub, c19_deaths = c19,
         share_pct = share, share_lb = share_lb, share_ub = share_ub) %>%
  mutate(across(expected:share_ub & !c19_deaths, ~ round(.x, 1))) %>%
  arrange(group, year) %>%
  write_csv("figures/alternatives/c19_share_of_excess_u25_vs_25plus.csv")

share %>% select(grp, year, c19, exc, share, share_lb, share_ub) %>% mutate(across(exc:share_ub, ~ round(.x, 1))) %>% print()
