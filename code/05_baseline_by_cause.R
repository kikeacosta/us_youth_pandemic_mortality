rm (list = ls())
source("Code/00_functions.R")

db <- read_rds("data_inter/deaths_population_master.rds")

unique(db$race)
unique(db$sex)
unique(db$age)
unique(db$cause)


# marging deaths and population
# grouping by 5-y age groups
db2 <- 
  db %>% 
  mutate(dts = ifelse(dts == 0, 1, dts))


# # selecting causes to be analyzed
# db2 %>% 
#   filter(year %in% 2015:2020) %>% 
#   group_by()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dts_all_fit <- 
  db2 %>% 
  # filter(year >= 2015) %>% 
  group_by(race, sex, age, cause) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup()

out <- 
  dts_all_fit %>% 
  mutate(
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    psc = dts / bsn,
    up = dts / bsn_lp,
    lp = dts / bsn_up)

write_rds(out, "data_inter/p_scores_excess_deaths.rds")


