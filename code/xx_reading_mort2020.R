library(tidyverse)
# https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple
# download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort2019us.zip", "Data/mort2019.zip")

db_2020 <- 
  read_fwf("data_input/VS20MORT.DUSMCPUB_r20220105",
           fwf_cols(sex2 = c(69, 69),
                    age2 = c(70, 73),
                    race = c(445, 446),
                    hisp = c(484, 486),
                    eth = c(488, 488),
                    ucod = c(146,149),
                    con01 = c(167,171),
                    con02 = c(174,178),
                    con03 = c(181,185),
                    con04 = c(188,192),
                    con05 = c(195,199),
                    con06 = c(202,206),
                    con07 = c(209,213),
                    con08 = c(216,220),
                    con09 = c(223,227),
                    con10 = c(230,234),
                    con11 = c(237,241),
                    con12 = c(244,248),
                    con13 = c(251,255),
                    con14 = c(258,262),
                    con15 = c(265,269),
                    con16 = c(272,276),
                    con17 = c(279,283),
                    con18 = c(286,290),
                    con19 = c(293,297),
                    con20 = c(300,304)), 
           col_types = "cdcccccccccccccccccccccccc")

write_rds(db_2020, "data_inter/mort2020_cont_causes.rds")






std_2020 <- 
  db_2020 %>% 
  mutate(age = case_when(age2 == 9999 | age2 == 1999 ~ -1,
                         age2 >= 2000 & age2 < 7000 ~ 0,
                         age2 >= 1110 & age2 <1200 ~ 110,
                         age2 >= 1000 & age2 <= 1110 ~ age2 - 1000,
                         TRUE ~ -2),
         sex = recode(sex2,
                      "M" = "m",
                      "F" = "f"),
         # Race = case_when(eth <= 5 ~ "h",
         #                  eth == 6 ~ "w",
         #                  eth == 7 ~ "b",
         #                  TRUE ~ "o"),
         race_r = case_when(race == "01" ~ "w",
                            race == "02" ~ "b",
                            TRUE ~ "o"),
         race_r = ifelse(hisp >= 200 & hisp <= 299, "h", race_r),
         year = 2020) %>% 
  group_by(year, race_r, sex, age, ucod) %>% 
  summarise(d = n()) %>% 
  ungroup()

write_rds(std_2020, "Data/sd_ucod4_race/2ucod_x_sex_race2020.rds")
