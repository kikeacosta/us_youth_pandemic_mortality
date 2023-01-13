rm (list = ls())
source("Code/00_functions.R")

db <- read_rds("data_inter/usa_mort_chapters_1999_2021.rds")
pp <- read_rds("data_inter/pop_race_sex_age_2010_2021.rds")


unique(db$cause)

# marging deaths and population
# grouping by 5-y age groups
db2 <- 
  db %>% 
  filter(year >= 2010) %>% 
  left_join(pp) %>% 
  filter(age <= 29) %>% 
  mutate(age = case_when(age %in% 1:4 ~ 1,
                         age %in% 5:29 ~ age - age%%5,
                         TRUE ~ age)) %>% 
  group_by(year, race, sex, age, cause) %>% 
  summarise(dts = sum(d),
            exposure = sum(pop)) %>% 
  ungroup()
# %>% 
#   mutate(dts = ifelse(dts == 0, 1, dts))

db3 <- 
  db2 %>% 
  filter(year %in% 2014:2021,
         sex == "t",
         race == "t",
         cause != "total") %>% 
  group_by(year, age) %>% 
  mutate(prop = dts / sum(dts)) %>% 
  ungroup() %>% 
  group_by(age, cause) %>% 
  mutate(av_prop = mean(prop)) %>% 
  ungroup()

# selecting causes with at least 1% of deaths on average
cod_sel <- 
  db3 %>% 
  filter((av_prop >= 0.01 | cause %in% c("suicide", "covid19", "drugs")),
         cause != "other") %>% 
  select(age, cause) %>% 
  unique()

unique(cod_sel$cause)

db4 <- 
  db2 %>% 
  filter(year >= 2014) %>% 
  inner_join(cod_sel) %>% 
  bind_rows(db2 %>% 
              filter(cause == "total"))

# residual category
resid <- 
  db2 %>% 
  filter(year >= 2014) %>% 
  anti_join(cod_sel) %>% 
  group_by(year, race, sex, age) %>% 
  summarise(dts = sum(dts),
            exposure = mean(exposure)) %>% 
  ungroup() %>% 
  mutate(cause = "other")

out <- 
  db4 %>% 
  bind_rows(resid) %>% 
  arrange(year, race, sex, age, cause)

write_rds(out, "data_inter/deaths_population_master.rds")

