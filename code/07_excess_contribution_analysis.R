rm (list = ls())
source("Code/00_functions.R")

bsn <- 
  read_rds("data_inter/p_scores_excess_deaths.rds")

d2 <- 
  bsn %>% 
  filter(year %in% 2020:2021,
         cause != "total") %>% 
  mutate(exc = dts - bsn,
         exc_pos = ifelse(exc > 0, exc, 0)) %>% 
  group_by(year, race, sex, age) %>% 
  mutate(exc_tot = sum(exc),
         exc_pos_tot = sum(exc_pos)) %>% 
  ungroup() %>% 
  group_by(year, race, sex, cause, age) %>% 
  mutate(cont = ifelse(exc > 0, exc / exc_pos_tot, 0)) %>% 
  ungroup()

d2 %>% 
  filter(age <= 19) %>%
  ggplot(aes(x = factor(age), y = cont))+
  geom_bar(aes(fill = cause),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  facet_nested(sex ~ race, scales = "free_y")
  
dr <- 
  d2 %>% 
  filter(age <= 19,
         cause == "drugs")


dr %>% 
  ggplot()+
  geom_point(aes(cont, factor(age), col = factor(year)))+
  facet_nested(sex ~ race)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Contrib of drugs to excess")

ggsave("figures/excess_cont_drugs.png",
       w = 7,
       h = 7,
       dpi = 600)

main <- 
  d2 %>% 
  filter(age <= 19) %>%
  select(year, race, sex, age, cause, cont) %>% 
  group_by(year, race, sex, age) %>% 
  arrange(-cont) %>% 
  mutate(ord = 1:n()) %>% 
  ungroup() %>% 
  filter(ord <= 3)

main %>% 
  filter(year == 2020) %>% 
  ggplot(aes(x = factor(age), y = cont))+
  geom_bar(aes(fill = cause),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  facet_nested(sex ~ race, scales = "free_y")

main %>% 
  ggplot()+
  geom_point(aes(cont, cause, col = factor(year)), alpha = 0.7)+
  facet_nested(age ~ sex + race, scales = "free_y")
