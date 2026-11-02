# Figure 5: annual rates of change in COVID-19 death rates by age group and
# race/ethnicity, US, 2020-2021, 2021-2022, 2022-2023.
#
# Input : data_inter/c19_deaths_rates_by_race_and_age.rds (script 02)
# Output: figures/fig5_c19_change_by_age_race.png (+ .csv)

source("code/00_functions.R")

dt <- read_rds("data_inter/c19_deaths_rates_by_race_and_age.rds")

race_labs <- c(all = "All races\nand ethnicities",
               hsp = "Hispanic",
               nha = "Non-Hispanic\nAsian",
               nhb = "Non-Hispanic\nBlack",
               nhw = "Non-Hispanic\nWhite")

chg <-
  dt %>%
  group_by(race, age) %>%
  arrange(year) %>%
  mutate(chg = mx / lag(mx)) %>%
  ungroup() %>%
  mutate(race = factor(race_labs[race], levels = race_labs))

# ratio axis labelled as percentage change
bks <- c(0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5, 2, 3, 4, 8, 16)
lbs <- (bks - 1) * 1e2

chg_u25 <-
  chg %>%
  filter(age < 25,
         year >= 2021) %>%
  mutate(per = chg - 1,
         age_group = factor(age_labs[as.character(age)], levels = age_labs))

# NOTE: cells with zero deaths in the previous year give an infinite change and
# are dropped by ggplot (with a warning); values outside the 10%-1600% range of
# the axis are also dropped (coord_cartesian is not used, as in the draft).
p_chg <-
  chg_u25 %>%
  ggplot(aes(year, chg, col = age_group, group = age_group)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, lty = "dashed") +
  scale_x_continuous(breaks = c(2021, 2022, 2023),
                     labels = c("2020-2021", "2021-2022", "2022-2023")) +
  scale_y_log10(breaks = bks, labels = lbs) +
  facet_grid(~race) +
  labs(y = "Change (%)", x = "Period") +
  scale_color_viridis_d(end = 0.9, name = "Age group", direction = -1) +
  theme_bars() +
  theme(panel.spacing.x = unit(1.5, "lines"),
        axis.text.x     = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "right")

ggsave("figures/fig5_c19_change_by_age_race.png", p_chg, w = 8, h = 4)

chg_u25 %>%
  mutate(period = paste(year - 1, year, sep = "-"),
         race = str_replace_all(race, "\n", " ")) %>%
  select(period, race, age_group, rate = mx, ratio = chg, change_pct = per) %>%
  mutate(change_pct = 100 * change_pct) %>%
  arrange(period, race, age_group) %>%
  write_csv("figures/fig5_c19_change_by_age_race.csv")
