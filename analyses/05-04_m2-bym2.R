set.seed(12345)

library(tidyverse)
library(spdep)
library(sf)
library(INLA)

### data 

data <- read_rds("data/BfS-closed/monthly_deaths/w_deaths_2015_2020_year_fin.Rds") %>% 
  # testing df
  # filter(id_space <= 2) %>%
  select(-(ARGRNR:ARNAME)) %>%
  filter(age != "<40") %>% 
  # strata with double zeroes seem to be crashing
  filter(pop_mid_poi > 0) %>% 
  # second id needed for ST interaction
  mutate(id_space2 = id_space) %>% 
  relocate(id_space2, .after = id_space)

### INLA setup

# priors
# inspired by @gkonstantinoudis 
# https://github.com/gkonstantinoudis/TutorialExcess

hyper.iid <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)))

hyper.bym2 <- list(theta1 = list("PCprior", c(1, 0.01)), 
                   theta2 = list("PCprior", c(0.5, 0.5)))

control.family <- inla.set.control.family.default()

threads = parallel::detectCores()

### Four models tested, all sex stratified & adjusted for age

# base
f1 <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_year, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2)

# adjsuting for 2015
f2 <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  id_2015 + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_year, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2)

# space time interaction from Bernardinelli et al. (1995)
# https://www.paulamoraga.com/book-geospatial/sec-arealdataexamplest.html
f3 <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2) + 
  f(id_space2, id_year, model = "iid", hyper = hyper.iid, constr = TRUE) + id_year


f4 <- deaths ~ 1 + offset(log(pop_mid_poi)) + 
  id_2015 + 
  f(id_age, model = "iid", hyper = hyper.iid, constr = TRUE) + 
  f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym2) + 
  f(id_space2, id_year, model = "iid", hyper = hyper.iid, constr = TRUE) + id_year


gem_sex_bym2 <- list()

for(sex in c("Female", "Male")){
  
  data_sex <- data %>% 
    filter(sex == sex) %>% 
    select(-sex) %>% 
    as.data.frame()
  
  i <- 1
  
  for(formula in c(f1, f2, f3, f4)) {
    
    mname <- paste0("m", i)
    
    print(paste(sex, mname))
    
    model <- inla(formula = formula,
                  data = data_sex,
                  family = "Poisson",
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

    gem_sex_bym2[[sex]][[mname]] <- model

    rm(model); gc()
    
    i <- i + 1
    
  }
  rm(data_sex); gc()
  
}

write_rds(gem_sex_bym2, "results/gem_sex_bym2.Rds")
