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

# priors I
# inspired by @martablangiardo 
# https://github.com/martablangiardo/ExcessDeathsItaly
hyper.bym <- list(theta1 = list("loggamma", c(1, 0.1)), 
                  theta2 = list("loggamma", c(1, 0.1)))

control.family <- inla.set.control.family.default()

threads = parallel::detectCores()

### Age adjusted, sex stratified

formula <-
  
  deaths ~ 1 + offset(log(pop_mid_poi)) + 
  
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) +
  
  f(id_year, model = "iid", hyper = hyper.iid, constr = TRUE) +
  
  f(id_space, model = "bym", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, hyper = hyper.bym)

gem_sex_bym <- list()

for(j in c("Female", "Male")){
  
  data_sex <- data %>% 
    filter(sex == j) %>% 
    select(-sex) %>% 
    as.data.frame()
  
  # also "nbinomial", "zeroinflatednbinomial0", "zeroinflatednbinomial1"?
  # for(f in c("Poisson", "zeroinflatedpoisson0", "zeroinflatedpoisson1")) {
  for(f in c("Poisson", "zeroinflatedpoisson1")) {
    
    print(paste(j, f))
    
    model <- inla(formula,
                  data = data_sex,
                  family = f,
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
                    # strategy = "laplace", # npoints = 21, 
                    int.strategy = "ccd" # default
                    # int.strategy = "grid", diff.logdens = 4
                  )
    )
    
    sex <- paste(j)
    family <- paste(f)
    gem_sex_bym[[sex]][[family]] <- model
    
    rm(model); gc()
    
  }
  rm(data_sex); gc()
  
}
write_rds(gem_sex_bym, "results/gem_sex_bym2.Rds")
