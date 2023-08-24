library(pacman)
p_load(tidyverse, magrittr, skimr, scales,
       sf, tmap,
       viridis,
       INLA)
source("R/helper_functions.R")

# data
kt = read_rds("data/BfS/kt.Rds")
tg3o = read_rds("data/BfS/tg3o.Rds")
shap = list(kt=kt,tg3o=tg3o)

exp_deaths_2020_year = read_rds("results/exp_deaths_2020_year.Rds") %>% 
  select(-cant_exp_deaths, -cant_observed, -p)  %>% 
  mutate(id_kt = as.integer(as.factor(canton))) 

# data management
exp_deaths_2020_year = exp_deaths_2020_year %>% 
  filter(!is.na(vote_yes_nov_cat)) %>% 
  filter(!is.na(vote_yes_jun_cat)) 
exp_deaths_2020_year = exp_deaths_2020_year %>% 
  filter(age_group != "<40") 
exp_deaths_2020_year = exp_deaths_2020_year %>% 
  mutate(id_space=as.numeric(as.factor(GMDNR)),
         id_space2=id_space,
         sex_fem=ifelse(sex=="Female",1,0),
         age_num=as.numeric(as.factor(age_group)),
         age_60s=ifelse(age_group=="60-69",1,0),
         age_70s=ifelse(age_group=="70-79",1,0),
         age_80s=ifelse(age_group=="80+",1,0),
         type_urban=ifelse(r_urban1=="Urban",1,0),
         type_rural=ifelse(r_urban1=="Rural",1,0),
         density_high=ifelse(r_urban2=="Dense",1,0),
         density_low=ifelse(r_urban2=="Low",1,0),
         sep5=ifelse(median_ssep3_q=="5th - highest",1,0),
         sep4=ifelse(median_ssep3_q=="4th",1,0),
         sep3=ifelse(median_ssep3_q=="3rd quintile",1,0),
         sep2=ifelse(median_ssep3_q=="2nd",1,0),
         sep1=ifelse(median_ssep3_q=="1st - lowest",1,0),
         lang_fr=ifelse(r_lang=="French",1,0),
         lang_it=ifelse(r_lang=="Italian",1,0),
         vote_nov_q5=ifelse(vote_yes_nov_cat==5,1,0),
         vote_nov_q4=ifelse(vote_yes_nov_cat==4,1,0),
         vote_nov_q3=ifelse(vote_yes_nov_cat==3,1,0),
         vote_nov_q2=ifelse(vote_yes_nov_cat==2,1,0),
         vote_nov_q1=ifelse(vote_yes_nov_cat==1,1,0),
         vote_jun_q5=ifelse(vote_yes_jun_cat==5,1,0),
         vote_jun_q4=ifelse(vote_yes_jun_cat==4,1,0),
         vote_jun_q3=ifelse(vote_yes_jun_cat==3,1,0),
         vote_jun_q2=ifelse(vote_yes_jun_cat==2,1,0),
         vote_jun_q1=ifelse(vote_yes_jun_cat==1,1,0),
         E=log(ifelse(munici_exp_deaths==0,1e-4,munici_exp_deaths)))

# inla setup
hyper.iid = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
hyper.bym2 = list(theta1 = list("PCprior", c(1, 0.01)), 
                  theta2 = list("PCprior", c(0.5, 0.5)))
threads = parallel::detectCores()

# iterate
if(FALSE) {
  n_iter = 50
  s_iter = sample(unique(exp_deaths_2020_year$it),size=n_iter)
  model1.5_full = list()
  for(i in 1:n_iter) {
    data_it = exp_deaths_2020_year %>% 
      filter(it == s_iter[i]) 
    model1.5_full[[i]] = INLA::inla(munici_observed ~ - 1 + offset(E) +
                                      sex:age_group +
                                      f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, 
                                        hyper = hyper.bym2, constr=TRUE) +
                                      age_group:border +
                                      age_group:type_rural + age_group:type_urban +
                                      age_group:sep1 + age_group:sep2 + age_group:sep3 + age_group:sep4 +
                                      age_group:vote_jun_q1 + age_group:vote_jun_q2 + age_group:vote_jun_q3 + age_group:vote_jun_q4,
                                    data = data_it,
                                    family = "Poisson",
                                    control.compute = list(config = TRUE, waic = TRUE),
                                    quantiles = c(0.025, 0.5, 0.975),
                                    num.threads = threads,
                                    safe = TRUE)
    cat(i)
  }
  saveRDS(model1.5_full,file="results_inla/model1.5_full.rds")
}

# merge
if(FALSE) {
  model1.5_full = readRDS("results_inla/model1.5_full.rds")
  model1.5_merg = inla.merge(loo=model1.5_full)
  saveRDS(model1.5_merg,file="results_inla/model1.5_merg.rds")
}

# analyse
if(FALSE) {
  model1.5_merg = readRDS("results_inla/model1.5_merg.rds")
  summary(model1.5_merg)
  
  exp(model1.5_merg$summary.fixed)
  drivers_plot_age(model1.5_merg,data1)
  
  map_munic(model1.5_merg,data1,shap)
}




# iterate
if(TRUE) {
  n_iter = 50
  s_iter = sample(unique(exp_deaths_2020_year$it),size=n_iter)
  model1.4b_full = list()
  for(i in 1:n_iter) {
    data_it = exp_deaths_2020_year %>% 
      filter(it == s_iter[i]) 
    model1.4b_full[[i]] = INLA::inla(munici_observed ~ - 1 + offset(E) +
                                       sex:age_group +
                                       f(id_space, model = "bym2", graph = "data/nb/gg_wm_q.adj", scale.model = TRUE, 
                                         hyper = hyper.bym2, constr=TRUE) +
                                       age_group:sep1 + age_group:sep2 + age_group:sep3 + age_group:sep4,
                                     data = data_it,
                                     family = "Poisson",
                                     control.compute = list(config = TRUE, waic = TRUE),
                                     quantiles = c(0.025, 0.5, 0.975),
                                     num.threads = threads,
                                     safe = TRUE)
    cat(i)
  }
  saveRDS(model1.4b_full,file="results_inla/model1.4b_full.rds")
}

# merge
if(TRUE) {
  model1.4b_full = readRDS("results_inla/model1.4b_full.rds")
  model1.4b_merg = inla.merge(loo=model1.4b_full)
  saveRDS(model1.4b_merg,file="results_inla/model1.4b_merg.rds")
}

# analyse
if(FALSE) {
  model1.4b_merg = readRDS("results_inla/model1.4b_merg.rds")
  summary(model1.4b_merg)
  
  exp(model1.4b_merg$summary.fixed)
  drivers_plot_age(model1.4b_merg,data1)
  
  map_munic(model1.4b_merg,data1,shap)
}
