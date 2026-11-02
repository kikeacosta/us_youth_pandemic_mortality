# COVID-19 deaths (UCD U07.1) by state, age group and year, 2020-2023.
#
# CDC WONDER suppresses counts below 10, so age-specific counts cannot be
# queried directly for small states. Instead we query deaths for open-ended
# age groups ("1+", "5+", "10+", ...) which are large enough not to be
# suppressed, and recover each closed age group by differencing.
#
# Input : data_input/c19/dts_c19_ages_*_state_2020_2023.txt (WONDER exports)
# Output: data_inter/c19_deaths_by_state_and_age.rds

source("code/00_functions.R")

std_this <- function(x){
  x %>%
    select(state = 2,
           year = 5,
           dx = 6) %>%
    drop_na(state) %>%
    mutate(dx = as.double(dx))
}

read_c19_state <- function(ages){
  read_tsv(paste0("data_input/c19/dts_c19_ages_", ages, "_state_2020_2023.txt"),
           show_col_types = FALSE) %>%
    std_this()
}

knw2  <- read_c19_state("knw")    %>% rename(dxknw = dx)   # all deaths with known age
d01p2 <- read_c19_state("01plus") %>% rename(dxo01 = dx)
d05p2 <- read_c19_state("05plus") %>% rename(dxo05 = dx)
d10p2 <- read_c19_state("10plus") %>% rename(dxo10 = dx)
d15p2 <- read_c19_state("15plus") %>% rename(dxo15 = dx)
d20p2 <- read_c19_state("20plus") %>% rename(dxo20 = dx)
d25p2 <- read_c19_state("25plus") %>% rename(dxo25 = dx)
d35p2 <- read_c19_state("35plus") %>% rename(dxo35 = dx)
d45p2 <- read_c19_state("45plus") %>% rename(dxo45 = dx)
d55p2 <- read_c19_state("55plus") %>% rename(dxo55 = dx)
d65p2 <- read_c19_state("65plus") %>% rename(dxo65 = dx)
d75p2 <- read_c19_state("75plus") %>% rename(dxo75 = dx)
d85p2 <- read_c19_state("85plus") %>% rename(dxo85 = dx)

dt <-
  knw2 %>%
  left_join(d01p2, by = c("state", "year")) %>%
  left_join(d05p2, by = c("state", "year")) %>%
  left_join(d10p2, by = c("state", "year")) %>%
  left_join(d15p2, by = c("state", "year")) %>%
  left_join(d20p2, by = c("state", "year")) %>%
  left_join(d25p2, by = c("state", "year")) %>%
  left_join(d35p2, by = c("state", "year")) %>%
  left_join(d45p2, by = c("state", "year")) %>%
  left_join(d55p2, by = c("state", "year")) %>%
  left_join(d65p2, by = c("state", "year")) %>%
  left_join(d75p2, by = c("state", "year")) %>%
  left_join(d85p2, by = c("state", "year")) %>%
  filter(year %in% 2020:2023) %>%
  mutate(dx00_01 = dxknw - dxo01,
         dx01_04 = dxknw - dx00_01 - dxo05,
         dx00_04 = dx00_01 + dx01_04,
         dx05_09 = dxknw - dx00_04 - dxo10,
         dx10_14 = dxknw - dx00_04 - dx05_09 - dxo15,
         dx0_14 = dx00_04 + dx05_09 + dx10_14,
         dx15_19 = dxknw - dx0_14 - dxo20,
         dx20_24 = dxknw - dx0_14 - dx15_19 - dxo25,
         dx25_34 = dxknw - dx0_14 - dx15_19 - dx20_24 - dxo35,
         dx35_44 = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dxo45,
         dx45_54 = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dx35_44 - dxo55,
         dx55_64 = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dx35_44 - dx45_54 - dxo65,
         dx65_74 = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dxo75,
         dx75_84 = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dx65_74 - dxo85,
         dx85p   = dxknw - dx0_14 - dx15_19 - dx20_24 - dx25_34 - dx35_44 - dx45_54 - dx55_64 - dx65_74 - dx75_84)

dt2 <-
  dt %>%
  select(state, year,
         dx00_01, dx01_04, dx05_09, dx10_14, dx15_19, dx20_24, dx25_34,
         dx35_44, dx45_54, dx55_64, dx65_74, dx75_84, dx85p) %>%
  gather(-state, -year, key = age, value = dx) %>%
  mutate(age = str_remove(age, "dx")) %>%
  separate(age, c("age", "trash"), sep = "_") %>%
  mutate(age = ifelse(age == "85p", 85, as.double(age))) %>%
  select(-trash)

# Integrity checks: 51 jurisdictions x 4 years x 13 age groups, no NAs
stopifnot(n_distinct(dt2$state) == 51,
          nrow(dt2) == 51 * 4 * 13,
          !anyNA(dt2$dx))

# Known issue: the open-ended queries were run on different days against the
# provisional 2023 file, which was still being updated. For three states the
# "1+" count exceeds the "known age" count by 1-2 deaths, giving -1/-2 infant
# deaths in 2023 (Arkansas, Nevada, Washington). Kept as queried; the effect on
# the 0-24 totals is at most 2 deaths.
neg <- dt2 %>% filter(dx < 0)
if (nrow(neg) > 0) {
  warning(nrow(neg), " negative cell(s) from differencing provisional counts:\n",
          paste(capture.output(print(as.data.frame(neg), row.names = FALSE)), collapse = "\n"),
          call. = FALSE)
}

write_rds(dt2, "data_inter/c19_deaths_by_state_and_age.rds")
