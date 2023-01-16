rm (list = ls())
source("Code/00_functions.R")

# https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple
# download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort2019us.zip", "Data/mort2019.zip")

db_2020 <- 
  read_fwf("data_input/VS20MORT.DUSMCPUB_r20220105",
           fwf_cols(sex2 = c(69, 69),
                    age2 = c(70, 73),
                    race = c(445, 446),
                    hisp = c(484, 486),
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
           col_types = "cdcccccccccccccccccccccccc") %>% 
  mutate(year = 2020)

write_rds(db_2020, "data_inter/mort2020_cont_causes.rds")


db_2021 <- 
  read_fwf("data_input/mort2021us.txt",
           fwf_cols(sex2 = c(69, 69),
                    age2 = c(70, 73),
                    race = c(489, 490),
                    hisp = c(484, 486),
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
           col_types = "cdcccccccccccccccccccccccc") %>% 
  mutate(year = 2021)

write_rds(db_2021, "data_inter/mort2021_cont_causes.rds")


db_2020 <- read_rds("data_inter/mort2020_cont_causes.rds")
db_2021 <- read_rds("data_inter/mort2021_cont_causes.rds")


db2 <- 
  bind_rows(db_2020, db_2021) %>% 
  mutate(age = case_when(age2 == 9999 | age2 == 1999 ~ -1,
                         age2 >= 2000 & age2 < 7000 ~ 0,
                         age2 >= 1110 & age2 <1200 ~ 110,
                         age2 >= 1000 & age2 <= 1110 ~ age2 - 1000,
                         TRUE ~ -2),
         sex = recode(sex2,
                      "M" = "m",
                      "F" = "f"),
         race_r = case_when(race == "01" ~ "w",
                            race == "02" ~ "b",
                            TRUE ~ "o"),
         race_r = ifelse(hisp >= 200 & hisp <= 299, "h", race_r))

rm (db_2020, db_2021); gc()

c19_ucod <- c("U049", "U071", "U072")

db3 <- 
  db2 %>% 
  mutate(c19_1 = ifelse(ucod %in% c19_ucod, 1, 0),
         c_19_cont = case_when(con01 %in% c19_ucod ~ 1,
                               con02 %in% c19_ucod ~ 2,
                               con03 %in% c19_ucod ~ 3,
                               con04 %in% c19_ucod ~ 4,
                               con05 %in% c19_ucod ~ 5,
                               con06 %in% c19_ucod ~ 6,
                               con07 %in% c19_ucod ~ 7,
                               con08 %in% c19_ucod ~ 8,
                               con09 %in% c19_ucod ~ 9,
                               con10 %in% c19_ucod ~ 10,
                               con11 %in% c19_ucod ~ 11,
                               con12 %in% c19_ucod ~ 12,
                               con13 %in% c19_ucod ~ 13,
                               con14 %in% c19_ucod ~ 14,
                               con15 %in% c19_ucod ~ 15,
                               con16 %in% c19_ucod ~ 16,
                               con17 %in% c19_ucod ~ 17,
                               con18 %in% c19_ucod ~ 18,
                               con19 %in% c19_ucod ~ 19,
                               con20 %in% c19_ucod ~ 20,
                               TRUE ~ 0),
         c19_2 = ifelse(c_19_cont == 0, 0, 1),
         cause = case_when(c19_1 == 1 & c19_2 == 0 ~ "c19 only ucod",
                         c19_1 == 0 & c19_2 == 1 ~ "c19 contr",
                         c19_1 == 1 & c19_2 == 1 ~ "c19",
                         TRUE ~ "no c19"),
         cause = factor(cause, levels = c("no c19", 
                                      "c19 only ucod", 
                                      "c19 contr", 
                                      "c19")))

write_rds(db3, "data_inter/mort_2020_2021_long.rds")
db3 <- read_rds("data_inter/mort_2020_2021_long.rds")

db4 <- 
  db3 %>% 
  group_by(year, race_r, sex, age, cause) %>% 
  summarise(dts = n()) %>% 
  ungroup()
 
db4 %>% 
  mutate(dts = ifelse(sex == "f", -dts, dts)) %>% 
  ggplot(aes(x = dts,
             y = factor(age),
             fill = cause)) + 
  geom_col()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  geom_text(aes(x = -Inf, y = 25), label = "Females", hjust = -1)+
  geom_text(aes(x = Inf, y = 25), label = "Males", hjust = 2)
  
ggsave("figures/c19_pyramid.png")

db4 %>% 
  mutate(dts = ifelse(sex == "f", -dts, dts)) %>% 
  filter(age %in% 0:30) %>% 
  ggplot(aes(x = dts,
             y = factor(age),
             fill = cause)) + 
  geom_col()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  geom_text(aes(x = -Inf, y = 25), label = "Females", hjust = -1)+
  geom_text(aes(x = Inf, y = 25), label = "Males", hjust = 2)

ggsave("figures/c19_pyramid_0_25.png")


