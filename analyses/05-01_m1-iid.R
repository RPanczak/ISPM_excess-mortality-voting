set.seed(12345)

library(tidyverse)
library(INLA)

### data 
data <- read_rds("data/BfS-closed/monthly_deaths/w_deaths_2015_2020_year_fin.Rds") %>% 
  select(-(ARGRNR:ARNAME)) %>%
  filter(age != "<40") %>% 
  filter(year < 2020) %>% 
  mutate(id_year = year - 2014,
         observed = deaths,
         id_space = as.integer(as.factor(GMDNR)),
         id_age = as.integer(as.factor(age)),
  id_age = as.integer(as.factor(age))) %>% 
  mutate(deaths = if_else(year >= 2020, NA_integer_, observed)) %>% 
  relocate(id_space) %>% 
  relocate(id_age, .after = age) %>% 
  relocate(observed, .after = deaths) %>% 
  relocate(id_year, .after = year) 

### INLA setup

# priors
hyper.iid <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
hyper.bym <- list(theta1 = list("PCprior", c(1, 0.01)), 
                  theta2 = list("PCprior", c(0.5, 0.5)))

control.family <- inla.set.control.family.default()

threads = parallel::detectCores()

### Age adjusted, sex stratified

formula_sex <-
  
  deaths ~ 1 + offset(log(pop_mid_poi)) + 
  
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) +
  
  f(id_year, model = "iid", hyper = hyper.iid, constr = TRUE) +
  
  # temp solution to save time
  f(id_space, model = "iid", constr = TRUE, hyper = hyper.iid)
  # f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)

gem_sex_iid <- list()

for(j in c("Female", "Male")){
  
  print(j)
  
  data_sex <- data %>% 
    filter(sex == j) %>% 
    select(-sex) %>% 
    # testing df
    # filter(GMDNAME %in% c("Unterschächen", "Bern")) %>% 
    as.data.frame()
  
  model_sex <- inla(formula_sex,
                      data = data_sex,
                      family = "Poisson",
                      # family = "zeroinflatedpoisson0",
                      # family = "zeroinflatedpoisson1",
                      # family = "zeroinflatednbinomial0",
                      # family = "zeroinflatednbinomial1",
                      # verbose = TRUE,
                      control.family = control.family,
                      control.compute = list(config = TRUE, 
                                             # return.marginals.predictor = TRUE,
                                             # cpo = TRUE, 
                                             dic = TRUE, 
                                             waic = TRUE),
                      control.mode = list(restart = TRUE),
                      num.threads = threads,
                      control.predictor = list(compute = TRUE, link = 1),
                      control.inla = list(
                        strategy = "simplified.laplace", # default
                        # strategy = "adaptive",  
                        # strategy = "gaussian",  
                        # strategy = "laplace", #npoints = 21, 
                        int.strategy = "ccd" # default
                        # int.strategy = "grid", diff.logdens = 4
                      )
  )
  
  name <- paste(j)
  gem_sex_iid[[name]] <- model_sex
  
  rm(data_sex, model_sex); gc()
  
}

write_rds(gem_sex_iid, "results/gem_sex_iid.Rds")
