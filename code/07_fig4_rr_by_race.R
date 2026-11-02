# Figure 4: relative risk of COVID-19 death by age group and race/ethnicity,
# relative to all races/ethnicities combined, US, 2020-2023.
#
# Input : data_inter/c19_deaths_rates_by_race_and_age.rds (script 02)
# Output: figures/fig4_c19_rr_by_race.png (+ .csv)

source("code/00_functions.R")

dt <- read_rds("data_inter/c19_deaths_rates_by_race_and_age.rds")

race_labs <- c(nhw = "Non-Hispanic White",
               nhb = "Non-Hispanic Black",
               hsp = "Hispanic",
               nha = "Non-Hispanic Asian")

dt2 <-
  dt %>%
  left_join(dt %>%
              filter(race == "all") %>%
              select(year, age, mx_ref = mx),
            by = c("year", "age")) %>%
  mutate(rr = mx / mx_ref)

dt_u25 <-
  dt2 %>%
  filter(race != "all",
         age < 25) %>%
  mutate(race = race_labs[race],
         age_group = factor(age_labs[as.character(age)], levels = age_labs))

cols <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2")
bks  <- c(0.2, 0.5, 1, 2, 5)

p_rr <-
  dt_u25 %>%
  filter(rr != 0) %>%     # cells with zero deaths cannot be shown on a log scale
  ggplot() +
  geom_point(aes(rr, age_group, col = race), alpha = 0.8) +
  scale_x_log10(breaks = bks) +
  facet_grid(~year) +
  scale_color_manual(values = cols) +
  geom_vline(xintercept = 1, lty = "dashed") +
  labs(col = "Race", y = "Age", x = "Relative Risk") +
  coord_cartesian(xlim = c(0.1, 10)) +
  theme_bars() +
  theme(panel.spacing = unit(2, "pt"),
        legend.position = "bottom")

ggsave("figures/fig4_c19_rr_by_race.png", p_rr, w = 6, h = 2.75)

dt_u25 %>%
  select(year, age_group, race, deaths = dx, pop, rate = mx, rate_all = mx_ref, rr) %>%
  arrange(year, age_group, race) %>%
  write_csv("figures/fig4_c19_rr_by_race.csv")
