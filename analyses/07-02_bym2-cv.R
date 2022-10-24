### #########################################

set.seed(12345)

library(tidyverse)
library(spdep)
library(sf)
library(INLA)

### data 2015-2019

data = read_rds("data/BfS-closed/monthly_deaths/w_deaths_2015_2020_year_fin.Rds") %>% 
  # testing df
  # filter(id_space <= 2) %>%
  select(-(ARGRNR:ARNAME)) %>%
  filter(age != "<40") %>% 
  # strata with double zeroes seem to be crashing !!!
  filter(pop_mid_poi > 0) %>% 
  # only data till 2019
  filter(year < 2020)

summary(data$year)

# year of interest

yoi = 2015

data = data %>% 
  mutate(deaths = if_else(year == yoi, NA_integer_, deaths))

table(is.na(data$deaths))

### INLA setup

# priors
# inspired by @gkonstantinoudis 
# https://github.com/gkonstantinoudis/TutorialExcess

hyper.iid = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))

hyper.bym2 = list(theta1 = list("PCprior", c(1, 0.01)), 
                  theta2 = list("PCprior", c(0.5, 0.5)))

control.family = inla.set.control.family.default()

threads = parallel::detectCores()

### #########################################
### sex stratified & adjusted for age

formula <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  year + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2)

gem_bym2_cv <- list()

for(s in c("Female", "Male")){
  
  data_sex <- data %>% 
    filter(sex == s) %>% 
    select(-sex) %>% 
    as.data.frame()
  
  model <- inla(formula = formula,
                data = data_sex,
                family = "Poisson",
                control.family = control.family,
                control.compute = list(config = TRUE,
                                       cpo = TRUE,
                                       dic = TRUE,
                                       waic = TRUE),
                quantiles = c(0.025, 0.5, 0.975),
                control.mode = list(restart = TRUE),
                num.threads = threads,
                control.predictor = list(compute = TRUE, link = 1)
  )
  
  gem_bym2_cv[[s]] <- model
  
  rm(model); gc()
  
}

rm(data_sex); gc()

write_rds(gem_bym2_cv, "results/gem_bym2_cv.Rds")

### #########################################
### sex stratified & adjusted for age + canton iid

formula <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  year + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_kt, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2)

gem_bym2_cv_kt <- list()

for(s in c("Female", "Male")){
  
  data_sex <- data %>% 
    filter(sex == s) %>% 
    select(-sex) %>% 
    as.data.frame()
  
  model <- inla(formula = formula,
                data = data_sex,
                family = "Poisson",
                control.family = control.family,
                control.compute = list(config = TRUE,
                                       cpo = TRUE,
                                       dic = TRUE,
                                       waic = TRUE),
                quantiles = c(0.025, 0.5, 0.975),
                control.mode = list(restart = TRUE),
                num.threads = threads,
                control.predictor = list(compute = TRUE, link = 1)
  )
  
  gem_bym2_cv_kt[[s]] <- model
  
  rm(model); gc()
  
}

rm(data_sex); gc()

write_rds(gem_bym2_cv_kt, "results/gem_bym2_cv_kt.Rds")
