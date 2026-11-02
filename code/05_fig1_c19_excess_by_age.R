# Figure 1: COVID-19 deaths, excess deaths (with 95% CI) and COVID-19 deaths as a
# percentage of excess deaths, by age group and year, US, 2020-2023.
#
# Input : data_input/c19/dts_c19_by_age_2020_2023.txt (WONDER, UCD U07.1 by 5-year age)
#         data_inter/excess_age.rds (script 04)
# Output: figures/fig1_c19_excess_by_age.png (+ .csv with the plotted values)
#         data_inter/fig1_age_data.rds (used by scripts 09 and alt_fig1_*)

source("code/00_functions.R")

# COVID-19 deaths by age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt <- read_tsv("data_input/c19/dts_c19_by_age_2020_2023.txt", show_col_types = FALSE)

c19 <-
  dt %>%
  select(age = 3,
         year = 5,
         dx = 6) %>%
  drop_na(year) %>%
  mutate(age = case_when(age == "1" ~ "0",
                         age == "100+" ~ "100",
                         TRUE ~ age)) %>%
  separate(age, c("age", "trash"), sep = "-") %>%
  select(-trash) %>%
  mutate(year = as.double(year)) %>%
  filter(year %in% 2020:2023) %>%
  mutate(age = as.double(age),
         age = ifelse(age >= 85, 85, age)) %>%
  reframe(dx = sum(dx),
          .by = c(age, year))

# Excess deaths by age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
exc <- read_rds("data_inter/excess_age.rds")

pops <-
  exc %>%
  select(age, year, pop = exposure)

# excess with the 95% CI of the baseline carried over to the excess:
# exc = dts - bsn, bounds = dts - uc and dts - lc
# (the prediction interval lp/up is available in exc; see script 10 for the comparison)
exc2 <-
  exc %>%
  mutate(dx = exc,
         dx_unc1 = dts - lc,
         dx_unc2 = dts - uc,
         dx_lb = pmin(dx_unc1, dx_unc2),
         dx_ub = pmax(dx_unc1, dx_unc2)) %>%
  select(year, age, dx_lb, dx, dx_ub)

all <-
  bind_rows(exc2 %>% mutate(cause = "excess"),
            c19 %>% mutate(cause = "c19")) %>%
  left_join(pops, by = c("age", "year")) %>%
  mutate(mx = 1e5 * dx / pop)

# Figure 1 ====
# ~~~~~~~~~~~~~
cause_cols <- c(c19 = "#B2182B", excess = "#762A83", ratio = "#FD8D3C")
cause_labs <- c(c19    = "COVID-19 Deaths",
                excess = "Excess Deaths",
                ratio  = "COVID-19 / Excess Deaths (%)")

deaths_age <-
  all %>%
  filter(age <= 20) %>%
  select(year, age, cause, mx = dx, mx_lb = dx_lb, mx_ub = dx_ub)

ratio_age <-
  c19 %>%
  rename(c19 = dx) %>%
  left_join(exc2, by = c("year", "age")) %>%
  filter(age <= 20) %>%
  mutate(cause = "ratio",
         mx    = c19 / dx * 100,
         mx_lb = NA_real_,
         mx_ub = NA_real_) %>%
  select(year, age, cause, mx, mx_lb, mx_ub)

bar_data_age <-
  bind_rows(deaths_age, ratio_age) %>%
  mutate(age_group = factor(age_labs[as.character(age)],
                            levels = age_labs))   # 0-1 at the bottom, 20-24 at the top

p_bar_age <-
  ggplot(bar_data_age, aes(x = mx, y = age_group, fill = cause)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_errorbar(aes(xmin = mx_lb, xmax = mx_ub), width = 0.4, na.rm = TRUE) +
  geom_text(aes(label = round(mx, 0),
                vjust = 1.1,
                hjust = ifelse(mx < 0, 1.1, -0.1)),
            size = 2.5) +
  scale_fill_manual(values = cause_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.18, 0.2))) +
  facet_grid(year ~ cause, switch = "y", scales = "free_x",
             labeller = labeller(cause = cause_labs)) +
  labs(x = NULL, y = NULL) +
  theme_bars()

ggsave("figures/fig1_c19_excess_by_age.png", p_bar_age,
       width = 7, height = 1.3 * n_distinct(bar_data_age$year) + 0.4, dpi = 300)

bar_data_age %>%
  select(year, age_group, cause, value = mx, lower = mx_lb, upper = mx_ub) %>%
  arrange(year, cause, age_group) %>%
  write_csv("figures/fig1_c19_excess_by_age.csv")

write_rds(list(all = all, deaths_age = deaths_age, ratio_age = ratio_age),
          "data_inter/fig1_age_data.rds")
