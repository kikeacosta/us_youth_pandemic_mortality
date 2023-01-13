rm (list = ls())
source("Code/00_functions.R")

bsn <- 
  read_rds("data_inter/p_scores_excess_deaths.rds")

bsn %>% 
  filter(year %in% 2020:2021,
         cause != "covid19") %>% 
  ggplot()+
  geom_point(aes(psc, cause, col = factor(year)))+
  facet_wrap(age ~ sex, ncol = 3, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()

# ggsave("figures/pscores_age_sex.pdf",
#        w = 10,
#        h = 20)


bsn %>% 
  filter(year %in% 2020:2021,
         cause != "covid19") %>% 
  ggplot()+
  geom_point(aes(psc, cause, col = factor(year)))+
  facet_nested(age ~ sex + race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()

ggsave("figures/pscores_age_sex2.pdf",
       w = 20,
       h = 10)

ggsave("figures/pscores_age_sex2.png",
       w = 20,
       h = 10,
       dpi = 600)


# drug-related mortality
# ~~~~~~~~~~~~~~~~~~~~~~
bsn %>% 
  filter(year %in% 2020:2021,
         cause == "drugs") %>% 
  ggplot()+
  geom_point(aes(psc, factor(age), col = factor(year)))+
  facet_nested(sex ~ race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "drugs")

ggsave("figures/pscores_drugs_age_sex.pdf",
       w = 10,
       h = 5)

ggsave("figures/pscores_drugs_age_sex.png",
       w = 10,
       h = 5,
       dpi = 600)


bsn %>% 
  filter(year %in% 2020:2021,
         cause == "drugs",
         sex == "t",
         race == "t",
         age < 20) %>% 
  ggplot()+
  geom_point(aes(psc, factor(age), col = factor(year)))+
  geom_linerange(aes(xmin = lp, xmax = up, y = factor(age),
                     col = factor(year)))+
  facet_nested(sex ~ race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "drugs")

ggsave("figures/pscores_drugs_age_sex2.png",
       w = 4,
       h = 3,
       dpi = 600)

dr <- 
  bsn %>% 
  filter(cause == "drugs",
         sex == "t")

dr %>% 
  filter(age < 20) %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(year, dts))+
  geom_line(aes(year, bsn))+
  facet_nested(age ~ race, scales = "free_y")+
  theme_bw()

ggsave("figures/baselines_drugs_age_sex.png",
       w = 10,
       h = 5,
       dpi = 600)


# suicides
# ~~~~~~~~
bsn %>% 
  filter(year %in% 2020:2021,
         cause == "suicide") %>% 
  ggplot()+
  geom_point(aes(psc, factor(age), col = factor(year)))+
  facet_nested(sex ~ race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "suicides")

ggsave("figures/pscores_suicide_age_sex.pdf",
       w = 10,
       h = 5)

ggsave("figures/pscores_suicide_age_sex.png",
       w = 10,
       h = 5,
       dpi = 600)


# traffic accidents mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn %>% 
  filter(year %in% 2020:2021,
         cause == "transport") %>% 
  ggplot()+
  geom_point(aes(psc, factor(age), col = factor(year)))+
  facet_nested(sex ~ race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "traffic")


bsn %>% 
  filter(year %in% 2020:2021,
         cause == "transport",
         sex == "t",
         race == "t",
         age < 20) %>% 
  ggplot()+
  geom_point(aes(psc, factor(age), col = factor(year)))+
  geom_linerange(aes(xmin = lp, xmax = up, y = factor(age),
                     col = factor(year)))+
  facet_nested(sex ~ race, scales = "free_y")+
  scale_x_log10(breaks = c(0.85, 0.9, 1, 1.2,1.5))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "transport")

ggsave("figures/pscores_transport_age_sex2.png",
       w = 4,
       h = 3,
       dpi = 600)



tr <- 
  bsn %>% 
  filter(cause == "transport",
         sex == "t")

tr %>% 
  filter(age < 20) %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(year, dts))+
  geom_line(aes(year, bsn))+
  facet_nested(age ~ race, scales = "free_y")+
  theme_bw()

ggsave("figures/baselines_drugs_age_sex.png",
       w = 10,
       h = 5,
       dpi = 600)





unique(bsn$cause)

# comparison with covid mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# deaths classified as covid
cov <- 
  bsn %>% 
  filter(cause == "covid19",
         year %in% 2020:2021) %>% 
  select(year, race, sex, age, dts_c19 = dts)

comp <- 
  bsn %>% 
  filter(year %in% 2020:2021,
         cause != "covid19") %>% 
  mutate(exc = dts - bsn,
         exc = ifelse(cause == "suicide", 0, exc)) %>% 
  select(year, race, sex, age, cause, exc) %>% 
  left_join(cov) %>% 
  mutate(rel_c19 = ifelse(exc > 0, dts_c19 / exc, Inf))

comp %>% 
  ggplot()+
  geom_point(aes(rel_c19, cause, col = factor(year)))+
  facet_nested(age ~ sex + race, scales = "free_y")+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "comparison with covid-19 deaths")

ggsave("figures/comparison_covid19.pdf",
       w = 20,
       h = 15)

# ggsave("figures/comparison_covid19.png",
#        w = 20,
#        h = 40)

tot_exc <- 
  bsn %>% 
  filter(cause == "total",
         year %in% 2020:2021) %>% 
  mutate(exc = dts - bsn) %>% 
  select(year, race, sex, age, exc_tot = exc)

comp2 <- 
  tot_exc %>% 
  left_join(cov) %>% 
  mutate(rel_c19 = ifelse(exc_tot > 0, dts_c19 / exc_tot, Inf))

comp2 %>% 
  filter(year == 2020,
         age < 20) %>% 
  ggplot()+
  geom_point(aes(exc_tot, factor(age)), col = "black")+
  geom_point(aes(dts_c19, factor(age)), col = "red")+
  facet_wrap(sex ~ race, ncol = 5, scales = "free_y")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  labs(title = "comparison with covid-19 deaths")


comp3 <- 
  tot_exc %>% 
  left_join(cov) %>% 
  gather(exc_tot, dts_c19, key = cause, value = dts)

comp3 %>% 
  filter(age < 20,
         sex == "t",
         race == "t") %>% 
  ggplot()+
  geom_point(aes(dts, factor(age), col = cause))+
  scale_color_manual(values = c("red", "black"))+
  facet_nested( ~ year, scales = "free_y")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  labs(title = "C19 deaths vs total excess")+
  theme(legend.position = "bottom")

ggsave("figures/covid_vs_excess.png",
       w = 7,
       h = 3,
       dpi = 600)



comp3 %>% 
  filter(year == 2020,
         age <= 15) %>% 
  ggplot()+
  geom_point(aes(dts, factor(age), col = cause))+
  facet_wrap(sex ~ race, ncol = 5, scales = "free_y")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  labs(title = "total excess vs C19 deaths")

cov %>% 
  filter(age <= 15) %>% 
  ggplot()+
  geom_point(aes(dts_c19, factor(age), col = factor(year)))+
  facet_wrap(sex ~ race, ncol = 5, scales = "free_y")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  labs(title = "C19 deaths")

ggsave("figures/c19_deaths_age_sex.png",
       w = 10,
       h = 5,
       dpi = 600)


cov %>% 
  filter(age <= 15,
         sex == "t",
         race == "t") %>% 
  ggplot()+
  geom_point(aes(dts_c19, factor(age), col = factor(year)))+
  facet_wrap(sex ~ race, ncol = 5, scales = "free_y")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  labs(title = "C19 deaths")

ggsave("figures/c19_deaths_age_sex2.png",
       w = 4,
       h = 4,
       dpi = 600)


# tables comparing C19 and excess deaths by year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t1 <- 
  cov %>% 
  filter(age <= 15,
         sex == "t",
         race == "t") %>% 
  spread(year, dts_c19)
  
copy_this(t1)

t2 <- 
  comp2 %>% 
  filter(age <= 15,
         sex == "t",
         race == "t") %>% 
  select(-dts_c19, -rel_c19) %>% 
  spread(year, exc_tot)

copy_this(t2)




