# COVID-19 deaths and death rates by age group, race/ethnicity and year, US, 2020-2023.
#
# Same differencing-of-open-ended-age-groups approach as script 01 (see there),
# applied separately to all races, Hispanic, and non-Hispanic single-race groups.
# Rates use WONDER single-race population estimates. Population estimates for
# 2023 were not available by race/ethnicity at query time (Aug 2024), so the
# 2022 populations are carried forward to 2023 (see README, Methodological notes).
#
# Input : data_input/c19/dts_c19_ages_*_{all,hisp,nhsp}_2020_2023.txt
#         data_input/population/pop_{all_races,hisp,single_race_no_hisp}_by_age_2020_2022.txt
# Output: data_inter/c19_deaths_rates_by_race_and_age.rds

source("code/00_functions.R")

age_cuts <- c("knw", "01plus", "05plus", "10plus", "15plus", "20plus", "25plus",
              "35plus", "45plus", "55plus", "65plus", "75plus", "85plus")
cut_names <- c("dxknw", "dxo01", "dxo05", "dxo10", "dxo15", "dxo20", "dxo25",
               "dxo35", "dxo45", "dxo55", "dxo65", "dxo75", "dxo85")

# recover closed age groups from open-ended ("N plus") counts
close_ages <- function(dt){
  dt %>%
    filter(year %in% 2020:2023) %>%
    mutate(dx0_1 = dxknw - dxo01,
           dx1_4 = dxknw - dx0_1 - dxo05,
           dx5_9 = dxknw - dx0_1 - dx1_4 - dxo10,
           dx10_14 = dxknw - dx0_1 - dx1_4 - dx5_9 - dxo15,
           dx15_19 = dxknw - dx0_1 - dx1_4 - dx5_9 - dx10_14 - dxo20,
           dx20_24 = dxknw - dx0_1 - dx1_4 - dx5_9 - dx10_14 - dx15_19 - dxo25,
           dx0_24 = dxknw - dxo25,
           dx25_34 = dxknw - dx0_24 - dxo35,
           dx35_44 = dxknw - dx0_24 - dx25_34 - dxo45,
           dx45_54 = dxknw - dx0_24 - dx25_34 - dx35_44 - dxo55,
           dx55_64 = dxknw - dx0_24 - dx25_34 - dx35_44 - dx45_54 - dxo65,
           dx65_74 = dxknw - dx0_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dxo75,
           dx75_84 = dxknw - dx0_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dx65_74 - dxo85,
           dx85p = dxknw - dx0_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dx65_74 - dx75_84) %>%
    select(-dx0_24, -all_of(cut_names)) %>%
    gather(starts_with("dx"), key = age, value = dx) %>%
    mutate(age = str_remove(age, "dx")) %>%
    separate(age, c("age", "trash"), sep = "_") %>%
    mutate(age = ifelse(age == "85p", 85, as.double(age))) %>%
    select(-trash)
}

# all races and Hispanic: columns year (3), deaths (4)
std_this <- function(x){
  x %>%
    select(year = 3,
           dx = 4) %>%
    drop_na(year) %>%
    mutate(dx = as.double(dx))
}

# non-Hispanic single race: columns year (3), race (4), deaths (6)
std_this_race <- function(x){
  x %>%
    select(year = 3,
           race = 4,
           dx = 6) %>%
    drop_na(year) %>%
    mutate(dx = as.double(dx))
}

read_group <- function(group, std_fun, keys){
  files <- paste0("data_input/c19/dts_c19_ages_", age_cuts, "_", group, "_2020_2023.txt")
  pieces <- map2(files, cut_names, function(f, nm){
    read_tsv(f, show_col_types = FALSE) %>% std_fun() %>% rename(!!nm := dx)
  })
  reduce(pieces, left_join, by = keys)
}

# all races
all_rcs <-
  read_group("all", std_this, keys = "year") %>%
  close_ages() %>%
  mutate(race = "all")

# Hispanic
hisp <-
  read_group("hisp", std_this, keys = "year") %>%
  close_ages() %>%
  mutate(race = "hsp")

# non-Hispanic by single race
oth_rcs <-
  read_group("nhsp", std_this_race, keys = c("year", "race")) %>%
  close_ages()

all_rcs2 <-
  oth_rcs %>%
  filter(race %in% c("Asian",
                     "Black or African American",
                     "White")) %>%
  bind_rows(hisp,
            all_rcs) %>%
  mutate(race = case_when(race == "Asian" ~ "nha",
                          race == "Black or African American" ~ "nhb",
                          race == "White" ~ "nhw",
                          TRUE ~ race))

# population by race/ethnicity and age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_hsp  <- read_tsv("data_input/population/pop_hisp_by_age_2020_2022.txt", show_col_types = FALSE)
pop_nhsp <- read_tsv("data_input/population/pop_single_race_no_hisp_by_age_2020_2022.txt", show_col_types = FALSE)
pop_usa  <- read_tsv("data_input/population/pop_all_races_by_age_2020_2022.txt", show_col_types = FALSE)

pop_usa2 <-
  pop_usa %>%
  select(age = 3, year = 5, pop = 6) %>%
  drop_na(year) %>%
  mutate(race = "all")

pop_hsp2 <-
  pop_hsp %>%
  select(age = 3, year = 5, pop = 6) %>%
  drop_na(year) %>%
  mutate(race = "hsp")

pop_nhsp2 <-
  pop_nhsp %>%
  select(age = 3, race = 4, year = 7, pop = 8) %>%
  filter(race %in% c("Black or African American",
                     "White",
                     "Asian")) %>%
  mutate(race = case_when(race == "Black or African American" ~ "nhb",
                          race == "White" ~ "nhw",
                          race == "Asian" ~ "nha",
                          TRUE ~ race))

pop <-
  bind_rows(pop_hsp2, pop_nhsp2, pop_usa2) %>%
  mutate(age = case_when(age == "1" ~ "0",
                         age == "85+" ~ "85",
                         TRUE ~ age)) %>%
  separate(age, c("age", "trash"), sep = "-") %>%
  select(-trash) %>%
  mutate(age = as.double(age),
         year = as.double(year))

# 2023 population not available by race at query time: carry 2022 forward
pop2 <-
  bind_rows(pop,
            pop %>%
              filter(year == 2022) %>%
              mutate(year = 2023)) %>%
  arrange(race, age, year)

out <-
  all_rcs2 %>%
  left_join(pop2, by = c("year", "age", "race")) %>%
  mutate(mx = 1e5 * dx / pop)

stopifnot(!anyNA(out$pop),
          n_distinct(out$race) == 5,
          nrow(out) == 5 * 4 * 13)

write_rds(out, "data_inter/c19_deaths_rates_by_race_and_age.rds")
