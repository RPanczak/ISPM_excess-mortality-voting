modelfit1 = function(mod,dat,nf=30) {
  tt = mod$summary.fitted.values %>% 
    as_tibble() %>% 
    bind_cols(dat) %>% 
    head(nf*8)
  g1 = tt %>% 
    ggplot(aes(x=GMDNAME)) +
    geom_point(aes(y=munici_observed)) +
    geom_pointrange(aes(y=mean,ymin=`0.025quant`,ymax=`0.975quant`),colour="firebrick",alpha=.5) +
    facet_grid(age_group~sex,scales="free") +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    labs(x="Municipality name",y="Deaths",title="Model fit")
  g2 = tt %>% 
    ggplot(aes(x=GMDNAME)) +
    geom_hline(yintercept=0,linetype=2) +
    geom_pointrange(aes(y=mean-munici_observed,ymin=`0.025quant`-munici_observed,ymax=`0.975quant`-munici_observed),colour="forestgreen",alpha=.5) +
    facet_grid(age_group~sex) +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    labs(x="Municipality name",y="Deaths",title="Residuals")
  
  return(cowplot::plot_grid(g1,g2))
}

map_munic = function(mod,dat,shape) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
    as_tibble() %>% 
    rename(id_space=ID) %>% 
    left_join(cc,by = join_by(id_space)) %>% 
    arrange(-mean) 

  tt2 = shape$tg3o %>% 
    select(GMDNR) %>% 
    right_join(tt,by = join_by(GMDNR)) %>% 
    mutate(munic_effect=exp(mean))
  
  g1 = ggplot() +
    geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
    geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
    scale_fill_viridis_c() +
    labs(title="Municipality effect",fill="Relative excess mortality")
  
  
  return(g1)
}


map_munic_lang = function(mod,dat,shape) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space,lang_fr,lang_it) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
    as_tibble() %>% 
    rename(id_space=ID) %>% 
    left_join(cc,by = join_by(id_space)) %>% 
    arrange(-mean) 
  
  tt2 =  shape$tg3o  %>% 
    select(GMDNR) %>% 
    right_join(tt,by = join_by(GMDNR)) %>% 
    mutate(munic_effect=exp(mean)) %>% 
    mutate(lang_fr_it=as.factor(ifelse(lang_fr==1 | lang_it==1, 1, 0)))
  
  tt3 = tt2 %>% 
    filter(lang_fr_it==1) %>% 
    st_union()
  
  g1 = ggplot() +
    geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
    geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
    geom_sf(data=tt3,fill=NA,aes(colour="French/Italian")) +
    scale_fill_viridis_c() +
    scale_colour_manual(values=c("French/Italian"="red")) +
    labs(title="Municipality effect",fill="Relative excess mortality",colour="Language")
  
  return(g1)
}

map_munic_age = function(mod,dat,shape) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space,age_num,age_group) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  tt = mod$summary.random$id_space[1:(max(dat$id_space)*4),] %>% 
    as_tibble() %>% 
    mutate(age_num=rep(1:4,each=max(dat$id_space))) %>% 
    rename(id_space=ID) %>% 
    left_join(cc,by = join_by(id_space,age_num)) %>% 
    arrange(-mean) 
  
  tt2 = shape$tg3o %>% 
    select(GMDNR) %>% 
    right_join(tt,by = join_by(GMDNR)) %>% 
    mutate(munic_effect=exp(mean))
  
  g1 = ggplot() +
    geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
    geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
    scale_fill_viridis_c() +
    labs(title="Municipality effect",fill="Relative excess mortality") +
    facet_wrap(~age_group)
  
  
  return(g1)
}


rank_munic = function(mod,dat,nf=30) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
    as_tibble() %>% 
    rename(id_space=ID) %>% 
    left_join(cc,by = join_by(id_space)) %>% 
    arrange(-mean) %>% 
    head(nf)
  g1 = tt %>%
    ggplot() +
    geom_hline(yintercept=1,linetype=2) +
    geom_pointrange(aes(x=gmd_canton,y=exp(mean),ymin=exp(`0.025quant`),ymax=exp(`0.975quant`),fill=canton,shape=canton),shape=21) +
    scale_x_discrete(limits=tt$gmd_canton) +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    labs(x="Municipality name",y="Deaths",title=paste0("Municipality effect (top ",nf,")"))

  return(g1)
}

drivers_plot = function(mod,dat) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q3)",4),
           rep("November referendum (ref: Q3)",4)),
    lab=c("High population density","Low population density",
          "SEP Q1","SEP Q2","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5"
  ))
  tt = exp(mod$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab))
  
  g1 = tt %>% 
    ggplot() +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`)) +
    geom_hline(yintercept=1,linetype=2) +
    labs(y="Relative excess mortality",x=NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    facet_grid(~type,scales="free_x",space="free") +
    labs(x="Relative excess mortality",title="Association with relative excess mortality") 
  
  return(g1)
}
  
drivers_plot2 = function(mod,dat) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q3","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q3","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q1)",5),
           rep("November referendum (ref: Q1)",5)),
    lab=c("High population density","Low population density",
          "SEP Q1","SEP Q2","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5"
    ))
  tt = exp(mod$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab))
  
  g1 = tt %>% 
    ggplot() +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`)) +
    geom_hline(yintercept=1,linetype=2) +
    labs(y="Relative excess mortality",x=NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    facet_grid(~type,scales="free_x",space="free") +
    labs(x="Relative excess mortality",title="Association with relative excess mortality") 
  
  return(g1)
}

interactions_plot = function(mod,dat) {
  nn = tibble(
    rowname2=c("density_high","density_low",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q3)",4),
           rep("November referendum (ref: Q3)",4)),
    lab=c("High population density","Low population density",
          "SEP Q1","SEP Q2","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5"
    ))
  
  mm = tibble(
    rowname1=c("age_group40-59","age_group60-69","age_group70-79","age_group80+"),
    age_group=c("40-59","60-69","70-79","80+")
  )
  tt = exp(mod$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    # as_tibble() %>%
    separate(col=rowname,sep=":",into=c("rowname1","rowname2")) %>% 
    filter(!grepl("sex",rowname1)) %>% 
    left_join(nn,by = join_by(rowname2)) %>% 
    left_join(mm,by = join_by(rowname1)) %>% 
    filter(!is.na(lab))
  
  g1 = tt %>% 
    ggplot() +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=age_group),position=position_dodge(.5)) +
    geom_hline(yintercept=1,linetype=2) +
    labs(y="Relative excess mortality",x=NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    facet_grid(~type,scales="free_x",space="free") +
    labs(x="Relative excess mortality",title="Association with relative excess mortality") 
  
  return(g1)
}