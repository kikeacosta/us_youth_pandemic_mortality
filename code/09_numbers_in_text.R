# Numbers quoted in the Results section, computed from the same objects that
# feed Figures 1-5. Writes output/numbers_in_text.txt.
#
# Input : data_inter/fig1_age_data.rds (script 05)
#         data_inter/fig2_fig3_state_region_data.rds (script 06)
#         data_inter/c19_deaths_rates_by_race_and_age.rds (script 02)

source("code/00_functions.R")

age_dat <- read_rds("data_inter/fig1_age_data.rds")
all        <- age_dat$all
deaths_age <- age_dat$deaths_age
ratio_age  <- age_dat$ratio_age

st_dat   <- read_rds("data_inter/fig2_fig3_state_region_data.rds")
dt       <- st_dat$dt
dtregion <- st_dat$dtregion
dtWide   <- st_dat$dtWide

out_file <- "output/numbers_in_text.txt"
# (no on.exit(sink()) here: at the top level of a sourced file it would run at
# once and close the sink before anything is written)
sink(out_file)

section <- function(title) cat("\n\n==== ", title, " ====\n", sep = "")
show <- function(x, n = Inf) print(as.data.frame(x), row.names = FALSE, digits = 4)

cat("Numbers quoted in the Results section (generated ", format(Sys.Date()), ")\n", sep = "")

# Results: Age ====
# ~~~~~~~~~~~~~~~~~
section("Age: total COVID-19 deaths (0-24) by year")
all %>% filter(age <= 20, cause == "c19") %>%
  group_by(year) %>% summarise(dx = sum(dx)) %>% show()

section("Age: COVID-19 deaths, 20-24 by year")
all %>% filter(age == 20, cause == "c19") %>% select(year, dx) %>% show()

section("Age: COVID-19 deaths under 15 (all cells), summary")
all %>% filter(age < 15, cause == "c19") %>%
  summarise(avg = mean(dx), med = median(dx), q25 = quantile(dx, .25),
            q75 = quantile(dx, .75), sd = sd(dx), max = max(dx)) %>% show()

section("Age: COVID-19 deaths, 10-14 by year")
all %>% filter(age == 10, cause == "c19") %>% select(year, dx) %>% show()

section("Age: excess deaths with 95% CI, by age and year")
all %>% filter(age <= 20, cause == "excess") %>%
  select(year, age, dx_lb, dx, dx_ub) %>% arrange(age, year) %>% show()

section("Age: COVID-19 / excess (%) by age and year")
ratio_age %>% select(year, age, pct = mx) %>%
  pivot_wider(names_from = year, values_from = pct) %>% show()

section("Age: ratio summary in cells with significant positive excess (lower bound > 0)")
sig <- deaths_age %>% filter(cause == "excess", mx_lb > 0) %>% select(year, age)
ratio_age %>% inner_join(sig, by = c("year", "age")) %>%
  summarise(n = n(), min = min(mx), q25 = quantile(mx, .25), q50 = median(mx),
            mean = mean(mx), q75 = quantile(mx, .75), max = max(mx)) %>% show()

section("Age: ratio summary in cells with negative excess")
neg <- deaths_age %>% filter(cause == "excess", mx <= 0) %>% select(year, age)
ratio_age %>% inner_join(neg, by = c("year", "age")) %>%
  summarise(n = n(), min = min(mx), q25 = quantile(mx, .25), q50 = median(mx),
            mean = mean(mx), q75 = quantile(mx, .75), max = max(mx)) %>% show()

# Results: State ====
# ~~~~~~~~~~~~~~~~~~~
section("State: US rates by year")
dt %>% filter(state == "US") %>% select(year, cause, mx, mx_lb, mx_ub) %>%
  arrange(cause, year) %>% show()

rank_year <- function(cs, yr, n = 6){
  dt %>% filter(state != "US", cause == cs, year == yr) %>%
    arrange(-mx) %>% mutate(rank = row_number()) %>%
    select(rank, state, mx, mx_lb, mx_ub) %>% head(n)
}

for (yr in 2020:2023) {
  section(paste0("State: highest COVID-19 death rates, ", yr))
  show(rank_year("c19", yr))
}

section("State: COVID-19 rank of NM, SC, OK in 2022")
dt %>% filter(state != "US", cause == "c19", year == 2022) %>%
  arrange(-mx) %>% mutate(rank = row_number()) %>%
  filter(state %in% c("New Mexico", "South Carolina", "Oklahoma")) %>%
  select(rank, state, mx) %>% show()

section("State: number of states with positive / negative excess, by year")
dt %>% filter(state != "US", cause == "excess") %>%
  mutate(sign = ifelse(mx > 0, "positive", "negative")) %>%
  count(year, sign) %>% show()

for (yr in 2020:2023) {
  section(paste0("State: highest excess death rates, ", yr))
  show(rank_year("excess", yr, n = 7))
}

section("State: lowest excess death rates, 2021")
dt %>% filter(state != "US", cause == "excess", year == 2021) %>%
  arrange(mx) %>% select(state, mx, mx_lb, mx_ub) %>% head(7) %>% show()

section("State: change in excess deaths between consecutive years (counts of states)")
dt %>% filter(state != "US", cause == "excess") %>%
  select(state, year, dx) %>% spread(year, dx) %>%
  summarise(inc_20_21 = sum(`2021` > `2020`), dec_20_21 = sum(`2021` < `2020`),
            inc_21_22 = sum(`2022` > `2021`), dec_21_22 = sum(`2022` < `2021`),
            inc_22_23 = sum(`2023` > `2022`), dec_22_23 = sum(`2023` < `2022`)) %>% show()

section("State: year of each state's highest excess death rate (counts of states)")
dt %>% filter(state != "US", cause == "excess") %>%
  group_by(state) %>% slice_max(mx, n = 1) %>% ungroup() %>%
  count(year) %>% show()

section("State: District of Columbia")
dt %>% filter(state == "District of Columbia") %>%
  select(year, cause, mx, mx_lb, mx_ub) %>% arrange(cause, year) %>% show()

section("State: 2021 ratio ranking (New Mexico, Missouri, Montana)")
dtWide %>% filter(state != "US", year == 2021) %>%
  arrange(-prop) %>% mutate(rank_prop = row_number()) %>%
  left_join(dt %>% filter(year == 2021, cause == "excess", state != "US") %>%
              arrange(-mx) %>% mutate(rank_excess = row_number()) %>% select(state, rank_excess),
            by = "state") %>%
  left_join(dt %>% filter(year == 2021, cause == "c19", state != "US") %>%
              arrange(-mx) %>% mutate(rank_c19 = row_number()) %>% select(state, rank_c19),
            by = "state") %>%
  filter(state %in% c("New Mexico", "Missouri", "Montana")) %>%
  select(state, c19, excess, prop, rank_c19, rank_excess, rank_prop) %>% show()

# Results: Region ====
# ~~~~~~~~~~~~~~~~~~~~
section("Region: COVID-19 death rates by region and year")
dtregion %>% filter(cause == "c19") %>% select(year, bea_region, mx) %>%
  pivot_wider(names_from = year, values_from = mx) %>% show()

section("Region: excess death rates with 95% CI by region and year")
dtregion %>% filter(cause == "excess") %>%
  select(year, bea_region, mx, mx_lb, mx_ub) %>% arrange(year, -mx) %>% show()

region_ratio <-
  dtregion %>% select(bea_region, year, cause, dx) %>%
  pivot_wider(values_from = dx, names_from = cause) %>%
  mutate(ratio = c19 / excess * 100)

section("Region: COVID-19 / excess (%) by region and year")
region_ratio %>% select(year, bea_region, ratio) %>%
  pivot_wider(names_from = year, values_from = ratio) %>% show()

section("Region: ratio in region-years with significant positive excess (lower bound > 0)")
sig_reg <- dtregion %>% filter(cause == "excess", mx_lb > 0) %>% select(bea_region, year)
region_ratio %>% inner_join(sig_reg, by = c("bea_region", "year")) %>%
  arrange(ratio) %>% show()

# Two exclusion rules for the mean ratio: region-years with ratio >= 100 %
# (one region per year), or region-years with |excess rate| < 0.2 per 100,000.
section("Region: mean |ratio| by year, excluding region-years with ratio >= 100%")
region_ratio %>% filter(ratio < 100) %>%
  group_by(year) %>% summarise(n = n(), mean_abs_ratio = mean(abs(ratio))) %>% show()

section("Region: mean |ratio| by year, excluding |excess rate| < 0.2 per 100,000")
dtregion %>% select(bea_region, year, cause, mx) %>%
  pivot_wider(values_from = mx, names_from = cause) %>%
  filter(abs(excess) >= 0.2) %>%
  mutate(ratio = c19 / excess * 100) %>%
  group_by(year) %>% summarise(n = n(), mean_abs_ratio = mean(abs(ratio))) %>% show()

# Results: Race/ethnicity ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rr <- read_csv("figures/fig4_c19_rr_by_race.csv", show_col_types = FALSE)
section("Race: relative risk range by race and year (ages 0-24)")
rr %>% group_by(year, race) %>%
  summarise(min_rr = min(rr), max_rr = max(rr), .groups = "drop") %>% show()

chg <- read_csv("figures/fig5_c19_change_by_age_race.csv", show_col_types = FALSE)
section("Race: % change in COVID-19 death rates by period, race and age")
chg %>% select(period, race, age_group, change_pct) %>%
  pivot_wider(names_from = age_group, values_from = change_pct) %>% show()

sink()
cat("written:", out_file, "\n")
