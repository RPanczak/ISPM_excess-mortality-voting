# sample data 
# canton_data = exp_deaths_2020_kt %>%
#   filter(it <= 100)
# munici_data = w_deaths_2020_year_fin

downscale_year = function(canton_data, munici_data) {
  
  # left_join
  mm = munici_data %>%
    tidyr::expand_grid(it = unique(canton_data$it)) %>%
    dplyr::left_join(canton_data, by = c("canton", "age_group", "sex", "it"))
  
  # compute downscale factor p
  # ie. proportion of canton deaths within strata and community (mind the iteration)
  pp = mm %>%
    dplyr::group_by(canton, age_group, sex, it) %>%
    dplyr::mutate(p = observed / sum(observed))
  
  # function to distribute deaths deaths from canton to community using weights 
  distribute_deaths = function(n, d, w) {
    l = length(d)
    r = rmultinom(n, size = d, prob = w + 1e-10) # add small number to avoid numerical issues with pop=0
    return(r)
  }
  
  # distribute
  dd = pp %>%
    dplyr::group_by(canton, age_group, sex, it) %>%
    dplyr::filter(exp_deaths > 0, p > 0) %>%
    dplyr::mutate(munici_exp_deaths = distribute_deaths(n = 1, exp_deaths, p))
  
  # compute excess
  ee = mm %>%
    dplyr::left_join(dd, by = c(
      "canton", "GMDNR",
      "age_group", "sex", "observed", "pop_mid_poi",
      "it", "exp_deaths", "observed2"
    )) %>%
    tidyr::replace_na(list(munici_exp_deaths = 0)) %>%
    dplyr::mutate(munici_excess = observed - munici_exp_deaths)
  
  return(ee)
}

if (FALSE) {
  dd %>%
    filter(
      canton == "BE",
      age_group == "80+",
      sex == "Female",
      it == 3
    ) %>%
    summarise(
      exp_deaths = max(exp_deaths),
      munici_exp_deaths = sum(munici_exp_deaths)
    )
}