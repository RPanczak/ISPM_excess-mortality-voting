set.seed(12345)

library(tidyverse)
library(spdep)
library(sf)
library(INLA)

### data 
data <- read_rds("data/BfS-closed/monthly_deaths/w_deaths_2015_2020_year_fin.Rds") %>% 
  # testing df
  select(-(ARGRNR:ARNAME)) %>%
  filter(age != "<40") %>% 
  # strata with double zeroes seem to be crashing
  filter(pop_mid_poi > 0) 

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
  
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)

gem_sex_bym <- list()

for(j in c("Female", "Male")){
  
  print(j)
  
  data_sex <- data %>% 
    filter(sex == j) %>% 
    select(-sex) %>% 
    as.data.frame()
  
  model_sex <- inla(formula_sex,
                    data = data_sex,
                    # family = "Poisson",
                    # family = "zeroinflatedpoisson0",
                    family = "zeroinflatedpoisson1",
                    # family = "zeroinflatednbinomial0",
                    # family = "zeroinflatednbinomial1",
                    verbose = TRUE,
                    control.family = control.family,
                    control.compute = list(config = TRUE, 
                                           # return.marginals.predictor = TRUE,
                                           cpo = TRUE,
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
  gem_sex_bym[[name]] <- model_sex
  
  rm(data_sex, model_sex); gc()
  
}

write_rds(gem_sex_bym, "results/gem_sex_bym.Rds")
