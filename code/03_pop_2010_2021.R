library(tidyverse)

vars <- c("TOT_MALE",
          "TOT_FEMALE",
          "NHWA_MALE",
          "NHWA_FEMALE",
          "NHBAC_MALE",
          "NHBAC_FEMALE",
          "H_MALE",
          "H_FEMALE")

files <- paste0("data_input/population/", list.files("data_input/population"))
i <- 1
db_p <- tibble()
for(i in 1:length(files)){
  temp <- read_csv(files[i])
  
  temp2 <- 
    temp %>% 
    gather(-UNIVERSE, -MONTH, -YEAR, -AGE, key = race_sex, value = pop) %>% 
    select(race_sex, year = YEAR, month = MONTH, age = AGE, pop) %>% 
    filter(race_sex %in% vars) %>% 
    separate(race_sex, c("race", "sex")) %>% 
    mutate(sex = str_to_lower(str_sub(sex, 1, 1)),
           race = recode(race,
                         "TOT" = "t",
                         "NHWA" = "w",
                         "NHBAC" = "b",
                         "H" = "h")) %>% 
    spread(race, pop) %>% 
    mutate(o = t - (w + b + h)) %>% 
    gather(-sex, -age, -month, -year, key = race, value = pop) %>% 
    filter(age != 999)
  
  db_p <- db_p %>% 
    bind_rows(temp2)

}

db_p2 <- 
  db_p %>% 
  select(year, month, race, sex, age, pop) %>% 
  arrange(year, month, race, sex, age) %>% 
  filter(month == 6) %>% 
  select(-month)

db_p3 <- 
  db_p2 %>% 
  bind_rows(db_p2 %>% 
              group_by(year, race, age) %>% 
              summarise(pop = sum(pop)) %>% 
              ungroup() %>% 
              mutate(sex = "t"))

write_csv(db_p3, "data_inter/pop_race_sex_age_2010_2021.csv")
write_rds(db_p3, "data_inter/pop_race_sex_age_2010_2021.rds")

