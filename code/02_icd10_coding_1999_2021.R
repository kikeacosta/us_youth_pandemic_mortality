# db <- read_rds("Data/sd_ucod4_race/2ucod_x_sex_race2018.rds")
# unique(db$age)

library(tidyverse)
# files <- paste0("data_inter/sd_ucod4_race/", list.files("data_inter/sd_ucod4_race/"))
files <- paste0("data_inter/sd_ucod4_race/2ucod_x_sex_race", 1999:2021, ".rds")
us_mort <- list()

open_ungr <- function(db){
  x <- read_rds(db) %>% 
    ungroup() %>% 
    filter(age >= 0)
}

us_mort <- lapply(files, open_ungr)

nams <- paste("y", gsub("\\.rds", "", files) %>% str_sub(-4), sep="_")
names(us_mort) <- nams

unique(us_mort$y_2020$sex)

icd_10 <- function(x){
  x <- 
    x %>%
    mutate(let_cause = as.character(str_sub(ucod, 1, 1)),
           num_cause = as.integer(str_sub(ucod, 2, 3)),
           num3_cause = as.integer(str_sub(ucod, 2, 4)))
  x <- 
    x %>% 
    mutate(sex = case_when(sex == 1 ~ "m",
                           sex == 2 ~ "f",
                           sex == "m" ~ "m",
                           sex == "f" ~ "f",
                           TRUE ~ "o"),
           cause = case_when(
             let_cause == "A" | let_cause == "B" ~ "infectious", 
             let_cause == "C" | (let_cause == "D" & num_cause <= 49) ~ "cancer",
             let_cause == "D" & num_cause >= 50 ~ "blood",
             let_cause == "E" ~ "endocrine",
             let_cause == "F" ~ "mental",
             let_cause == "G" ~ "nervous",
             let_cause == "H" & num_cause <= 59 ~ "eye",
             let_cause == "H" & num_cause >= 60 ~ "ear",
             let_cause == "I" ~ "circulatory",
             let_cause == "J" ~ "respiratory",
             let_cause == "K" ~ "digestive",
             let_cause == "L" ~ "skin",
             let_cause == "M" ~ "muscoloskeletal",
             let_cause == "N" ~ "genitourinary",
             let_cause == "O" ~ "pregnancy",
             let_cause == "P" ~ "perinatal",
             let_cause == "Q" ~ "congenital",
             let_cause == "S" | let_cause == "T" |
               let_cause == "V" | let_cause == "W" |
               let_cause == "X" | let_cause == "Y" ~ "external", 
             let_cause == "U" & num_cause %in% 7 ~ "covid19",
             TRUE ~ "other")) %>% 
    mutate(cause = case_when(
      # let_cause=='B' & num_cause>=20 & num_cause<=24 ~ 'hiv',
      # let_cause=='B' & (num3_cause==171 | num3_cause==182) ~ 'hep_c',
      # let_cause=='J' & num_cause>=40 & num_cause<=44 ~ 'copd',
      let_cause=='X' & num_cause>=60 & num_cause<=84 ~ 'suicide',
      let_cause=='F' & num_cause>=11 & num_cause<=19 ~ 'drugs',
      let_cause=='F' & num_cause==55 ~ 'drugs',
      let_cause=='X' & num_cause>=40 & num_cause<=44 ~ 'drugs',
      let_cause=='Y' & num_cause>=10 & num_cause<=14 ~ 'drugs',
      let_cause=='P' & num3_cause==961 ~ 'drugs',
      let_cause=='P' & num3_cause %in% c(041, 044) ~ 'drugs',
      let_cause=='E' & num3_cause==244 ~ 'alcohol',
      let_cause=='F' & num_cause==10 ~ 'alcohol',
      let_cause=='G' & num3_cause==312 ~ 'alcohol',
      let_cause=='G' & num3_cause==621 ~ 'alcohol',
      let_cause=='G' & num3_cause==721 ~ 'alcohol',
      let_cause=='I' & num3_cause==426 ~ 'alcohol',
      let_cause=='K' & num3_cause==292 ~ 'alcohol',
      let_cause=='K' & num3_cause>=700 & num3_cause<=709 ~ 'alcohol',
      let_cause=='K' & num3_cause==860 ~ 'alcohol',
      let_cause=='X' & num_cause==45 ~ 'alcohol',
      let_cause=='Y' & num_cause==15 ~ 'alcohol',
      let_cause=='Y' & num_cause>=90 & num_cause<=91 ~ 'alcohol',
      let_cause=='V' & num_cause>=00 & num_cause<=99 ~ 'transport',
      let_cause=='X' & num_cause>=85  ~ 'homicide',
      let_cause=='Y' & num_cause<=09  ~ 'homicide',
      TRUE ~ cause)
      )
}



list_10 <- c(paste0("y_", 1999:2021))

us_mort <- us_mort %>% map_at(list_10, icd_10)

# grouping by chapter
chapters <- function(x){
  x <- x %>% 
    group_by(year, race_r, sex, age, cause) %>%  
    summarise(d = sum(d))
}

deaths_chp <- us_mort %>% map(chapters)
deaths_chp <- deaths_chp %>% 
  bind_rows() %>% 
  arrange(year, age, sex, race_r, cause) %>% 
  filter(age >= 0)

unique(deaths_chp$sex)
head(deaths_chp)

rate_t <- 
  deaths_chp %>% 
  group_by(year, sex, age, cause) %>% 
  summarise(d = sum(d),
            race_r = "t") %>% 
  ungroup() %>% 
  bind_rows(deaths_chp)
  
sex_t <- 
  rate_t %>% 
  group_by(year, race_r, age, cause) %>% 
  summarise(d = sum(d),
            sex = "t") %>% 
  ungroup() %>% 
  bind_rows(rate_t)

cause_t <- 
  sex_t %>% 
  group_by(year, race_r, sex, age) %>% 
  summarise(d = sum(d),
            cause = "total") %>% 
  ungroup() %>% 
  bind_rows(sex_t) %>% 
  select(year, race = race_r, sex, age, cause, d)

out <- 
  cause_t %>% 
  complete(year, race, sex, age, cause, fill = list(d = 0)) 


write_rds(out, "data_inter/usa_mort_chapters_1999_2021.rds")
