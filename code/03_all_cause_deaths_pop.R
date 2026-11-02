# All-cause deaths and populations, 2010-2023, for the US by sex and age
# (all ages) and for each state by sex and age (ages 0-24).
#
# WONDER was queried twice for each series: final data 2010-2020 (Underlying
# Cause of Death 1999-2020) and provisional data 2020-2023 (Provisional
# Mortality Statistics). The 2020 overlap is used to check that both sources
# agree; duplicates are then dropped.
#
# State age-specific counts for ages 0-24 are recovered from open-ended age
# groups as in script 01 (WONDER suppression of counts below 10).
#
# Input : data_input/all_cause/*.txt, data_input/population/pop_by_sex_age_2010_2023.txt,
#         data_input/population/pop_state_sex_age_0_24_2010_2023.txt
# Output: data_inter/pop_us_by_sex_age_2010_2023.rds
#         data_inter/ac_deaths_by_sex_age.rds
#         data_inter/ac_deaths_by_state_sex_age.rds

source("code/00_functions.R")

# must hold after binding the two WONDER periods and deduplicating
check_unique <- function(dt, ...){
  dup <- dt %>% count(...) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    print(dup)
    stop("2020 values differ between the final and provisional WONDER files")
  }
  invisible(dt)
}

# for all the US ====
# ~~~~~~~~~~~~~~~~~~~
allus1 <- read_tsv("data_input/all_cause/dts_ac_ages_all_sex_all_us_2010_2020.txt", show_col_types = FALSE)
allus2 <- read_tsv("data_input/all_cause/dts_ac_ages_all_sex_all_us_2020_2023.txt", show_col_types = FALSE)

std_us <-
  function(dt){
    dt %>%
      select(sex = 3,
             age = 5,
             year = 7,
             dx = 8,
             pop = 9) %>%
      drop_na(year) %>%
      mutate(sex = str_to_lower(sex),
             dx = as.double(dx)) %>%
      select(-pop)
  }

us <-
  bind_rows(std_us(allus1),
            std_us(allus2)) %>%
  unique() %>%
  check_unique(sex, age, year) %>%
  mutate(age = ifelse(age == '1', '0', age)) %>%
  separate(age, c("age", "trash"), sep = "-") %>%
  mutate(age = ifelse(age == "100+", 100, as.double(age))) %>%
  select(-trash) %>%
  filter(year %in% 2010:2023) %>%
  drop_na(age) %>%
  mutate(age = ifelse(age >= 85, 85, age)) %>%
  reframe(dx = sum(dx),
          .by = c(sex, age, year))

us_pop <- read_tsv("data_input/population/pop_by_sex_age_2010_2023.txt", show_col_types = FALSE)

us_pop2 <-
  us_pop %>%
  select(age = 3,
         sex = 5,
         year = 7,
         pop = 8) %>%
  drop_na(year) %>%
  replace_na(list(age = 85)) %>%
  mutate(sex = str_to_lower(sex),
         age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         TRUE ~ age - age %% 5)) %>%
  reframe(pop = sum(pop),
          .by = c(year, sex, age))

write_rds(us_pop2, "data_inter/pop_us_by_sex_age_2010_2023.rds")

us2 <-
  us %>%
  left_join(us_pop2, by = c("sex", "age", "year"))

us3 <-
  us2 %>%
  bind_rows(us2 %>%
              reframe(dx = sum(dx),
                      pop = sum(pop),
                      .by = c(age, year)) %>%
              mutate(sex = "t"))

stopifnot(!anyNA(us3$pop))
write_rds(us3, "data_inter/ac_deaths_by_sex_age.rds")

# US totals for ages 0-24 in the same shape as the state data (appended below)
all_us <-
  bind_rows(std_us(allus1),
            std_us(allus2)) %>%
  unique() %>%
  mutate(age = ifelse(age == '1', '0', age)) %>%
  separate(age, c("age", "trash"), sep = "-") %>%
  mutate(age = as.double(age)) %>%
  filter(age <= 20) %>%
  select(-trash) %>%
  filter(year %in% 2010:2023) %>%
  mutate(state = "us")


# by US state, ages 0-24, by sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
std_by_sex <-
  function(dt){
    dt %>%
      select(state = 2,
             sex = 5,
             year = 7,
             dx = 8,
             pop = 9) %>%
      drop_na(year) %>%
      mutate(sex = str_to_lower(sex),
             dx = as.double(dx)) %>%
      select(-pop)
  }

read_state_cut <- function(ages, nm){
  bind_rows(
    read_tsv(paste0("data_input/all_cause/dts_ac_ages_", ages, "_sex_all_2010_2020.txt"), show_col_types = FALSE) %>% std_by_sex(),
    read_tsv(paste0("data_input/all_cause/dts_ac_ages_", ages, "_sex_all_2020_2023.txt"), show_col_types = FALSE) %>% std_by_sex()
  ) %>%
    unique() %>%
    check_unique(state, sex, year) %>%
    rename(!!nm := dx)
}

knw  <- read_state_cut("kwn",    "dxknw")
d01p <- read_state_cut("01plus", "dxo01")
d05p <- read_state_cut("05plus", "dxo05")
d10p <- read_state_cut("10plus", "dxo10")
d15p <- read_state_cut("15plus", "dxo15")
d20p <- read_state_cut("20plus", "dxo20")
d25p <- read_state_cut("25plus", "dxo25")

dt <-
  knw %>%
  left_join(d01p, by = c("state", "sex", "year")) %>%
  left_join(d05p, by = c("state", "sex", "year")) %>%
  left_join(d10p, by = c("state", "sex", "year")) %>%
  left_join(d15p, by = c("state", "sex", "year")) %>%
  left_join(d20p, by = c("state", "sex", "year")) %>%
  left_join(d25p, by = c("state", "sex", "year")) %>%
  filter(year %in% 2010:2023) %>%
  mutate(dx00_01 = dxknw - dxo01,
         dx01_04 = dxknw - dx00_01 - dxo05,
         dx00_04 = dx00_01 + dx01_04,
         dx05_09 = dxknw - dx00_04 - dxo10,
         dx10_14 = dxknw - dx00_04 - dx05_09 - dxo15,
         dx0_14 = dx00_04 + dx05_09 + dx10_14,
         dx15_19 = dxknw - dx0_14 - dxo20,
         dx20_24 = dxknw - dx0_14 - dx15_19 - dxo25)

dt2 <-
  dt %>%
  select(state, sex, year,
         dx00_01, dx01_04, dx05_09, dx10_14, dx15_19, dx20_24) %>%
  gather(-state, -sex, -year, key = age, value = dx) %>%
  mutate(age = str_remove(age, "dx")) %>%
  separate(age, c("age", "trash"), sep = "_") %>%
  mutate(age = as.double(age)) %>%
  select(-trash) %>%
  bind_rows(all_us) %>%
  arrange(state, sex, age, year)

# adding exposures ====
# ~~~~~~~~~~~~~~~~~~~~~
pop <- read_tsv("data_input/population/pop_state_sex_age_0_24_2010_2023.txt", show_col_types = FALSE)

pop2 <-
  pop %>%
  select(age = 3,
         sex = 5,
         state = 6,
         year = 9,
         pop = 10) %>%
  drop_na(year) %>%
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         TRUE ~ age - age %% 5),
         sex = str_to_lower(sex)) %>%
  reframe(pop = sum(pop),
          .by = c(year, state, sex, age))

pop3 <-
  pop2 %>%
  bind_rows(pop2 %>%
              reframe(pop = sum(pop),
                      .by = c(age, sex, year)) %>%
              mutate(state = "us"))

dt3 <-
  dt2 %>%
  left_join(pop3, by = c("state", "sex", "year", "age"))

dt4 <-
  dt3 %>%
  bind_rows(dt3 %>%
              group_by(state, year, age) %>%
              summarise(dx = sum(dx),
                        pop = sum(pop),
                        .groups = "drop") %>%
              mutate(sex = "t")) %>%
  arrange(state, sex, age, year)

stopifnot(!anyNA(dt4$dx), !anyNA(dt4$pop), all(dt4$dx >= 0),
          n_distinct(dt4$state) == 52)   # 50 states + DC + "us"

write_rds(dt4, "data_inter/ac_deaths_by_state_sex_age.rds")
