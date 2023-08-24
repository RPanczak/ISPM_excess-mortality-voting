

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

map_munic = function(mod,dat,shape,interactive=FALSE,compute_cri=FALSE) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  if(compute_cri) {
    tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
      as_tibble() %>% 
      rename(id_space=ID) %>% 
      left_join(cc,by = join_by(id_space)) %>% 
      arrange(-mean) %>% 
      mutate(`0.025quant`=mean-1.96*sd,
             `0.975quant`=mean+1.96*sd)
  } else {
    tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
      as_tibble() %>% 
      rename(id_space=ID) %>% 
      left_join(cc,by = join_by(id_space)) %>% 
      arrange(-mean) 
  }
  tt2 = shape$tg3o %>% 
    select(GMDNR) %>% 
    right_join(tt,by = join_by(GMDNR)) %>% 
    mutate(munic_effect=exp(mean),
           munic_effect_lb=exp(`0.025quant`),
           munic_effect_ub=exp(`0.975quant`))
  
  if(interactive) {
    tt2 = tt2 %>% 
      mutate(tooltip = paste0(gmd_canton,
                              "\n",
                              sprintf('%.2f',munic_effect)," (",sprintf('%.2f',munic_effect_lb),"-",sprintf('%.2f',munic_effect_ub),")"))
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf_interactive(data=tt2,aes(fill=munic_effect,tooltip=tooltip),colour="transparent") +
      scale_fill_viridis_c(trans="pseudo_log") +
      labs(title="Residual municipality effect",fill="Relative excess mortality")
    g1 = ggiraph::girafe(ggobj = g1)
    g1 = ggiraph::girafe_options(g1,opts_zoom(min = 1, max = 5))
    
  } else {
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
      scale_fill_viridis_c(trans="pseudo_log") +
      labs(title="Residual municipality effect",fill="Relative excess mortality")
    
  }
  
  
  return(g1)
}


map_munic = function(mod,dat,shape,interactive=FALSE,compute_cri=FALSE) {
  cc = dat %>% 
    dplyr::select(canton,GMDNR,GMDNAME,id_space) %>% 
    dplyr::mutate(gmd_canton=paste0(GMDNAME," (",canton,")")) %>% 
    dplyr::distinct()
  if(compute_cri) {
    tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
      as_tibble() %>% 
      rename(id_space=ID) %>% 
      left_join(cc,by = join_by(id_space)) %>% 
      arrange(-mean) %>% 
      mutate(`0.025quant`=mean-1.96*sd,
             `0.975quant`=mean+1.96*sd)
  } else {
    tt = mod$summary.random$id_space[1:max(dat$id_space),] %>% 
      as_tibble() %>% 
      rename(id_space=ID) %>% 
      left_join(cc,by = join_by(id_space)) %>% 
      arrange(-mean) 
  }
  tt2 = shape$tg3o %>% 
    select(GMDNR) %>% 
    right_join(tt,by = join_by(GMDNR)) %>% 
    mutate(munic_effect=exp(mean),
           munic_effect_lb=exp(`0.025quant`),
           munic_effect_ub=exp(`0.975quant`))
  
  if(interactive) {
    tt2 = tt2 %>% 
      mutate(tooltip = paste0(gmd_canton,
                              "\n",
                              sprintf('%.2f',munic_effect)," (",sprintf('%.2f',munic_effect_lb),"-",sprintf('%.2f',munic_effect_ub),")"))
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf_interactive(data=tt2,aes(fill=munic_effect,tooltip=tooltip),colour="transparent") +
      scale_fill_viridis_c(trans="pseudo_log") +
      labs(title="Residual municipality effect",fill="Relative excess mortality")
    g1 = ggiraph::girafe(ggobj = g1)
    g1 = ggiraph::girafe_options(g1,opts_zoom(min = 1, max = 5))
    
  } else {
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
      scale_fill_viridis_c(trans="pseudo_log") +
      labs(title="Residual municipality effect",fill="Relative excess mortality")
    
  }
  
  
  return(g1)
}
map_munic_final = function(mod,dat,shape,fillname="Relative excess\nmortality") {
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
    scale_fill_viridis_c(limits=c(.89,1.14)) +
    labs(title=NULL,fill=fillname)
  
  
  return(g1)
}

map_munic_lang = function(mod,dat,shape,interactive=FALSE) {
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
    mutate(munic_effect=exp(mean),
           munic_effect_lb=exp(`0.025quant`),
           munic_effect_ub=exp(`0.975quant`)) 
  
  tt3 = tt2 %>% 
    filter(lang_fr==1) %>% 
    st_union()
  
  tt4 = tt2 %>% 
    filter(lang_it==1) %>% 
    st_union()
  
  if(interactive) {
    tt2 = tt2 %>% 
      mutate(tooltip = paste0(gmd_canton,
                              "\n",
                              sprintf('%.2f',munic_effect)," (",sprintf('%.2f',munic_effect_lb),"-",sprintf('%.2f',munic_effect_ub),")"))
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf_interactive(data=tt2,aes(fill=munic_effect,tooltip=tooltip),colour="transparent") +
      geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
      geom_sf(data=tt3,fill=NA,aes(colour="French")) +
      geom_sf(data=tt4,fill=NA,aes(colour="Italian")) +
      scale_fill_viridis_c() +
      scale_colour_manual(values=c("French"="red","Italian"="orange")) +
      labs(title="Municipality effect",fill="Relative excess mortality",colour="Language")
    g1 = ggiraph::girafe(ggobj = g1)
    g1 = ggiraph::girafe_options(g1,opts_zoom(min = 1, max = 5))
    
  } else {
    g1 = ggplot() +
      geom_sf(data=shape$kt,fill="grey95",colour="grey70") +
      geom_sf(data=tt2,aes(fill=munic_effect),colour="transparent") +
      geom_sf(data=tt3,fill=NA,aes(colour="French/Italian")) +
      scale_fill_viridis_c() +
      scale_colour_manual(values=c("French/Italian"="red")) +
      labs(title="Municipality effect",fill="Relative excess mortality",colour="Language")
    
    
  }
  
  
 
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
    labs(y="Relative excess mortality",title="Association with relative excess mortality") 
  
  return(g1)
}

drivers_plot_age = function(mod,dat,compute_cri=FALSE) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "type_rural","type_urban","type_periurban",
              "sep1","sep2","sep3","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q3","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q3","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Urban/rural (ref: rural)",3),
           rep("Quintile of SEP (ref: Q5)",5),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q5)",5),
           rep("November referendum (ref: Q5)",5)),
    lab=c("High population density","Low population density",
          "Rural","Urban","Peri-urban",
          "SEP Q1","SEP Q2","SEP Q3","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5"
    ))
  if(compute_cri) {
    nn = tibble(
      rowname=c("density_high","density_low",
                "type_rural","type_urban",
                "sep1","sep2","sep3","sep4","sep5",
                "border",
                "lang_fr","lang_it",
                "vote_jun_q1","vote_jun_q2","vote_jun_q3","vote_jun_q4","vote_jun_q5",
                "vote_nov_q1","vote_nov_q2","vote_nov_q3","vote_nov_q4","vote_nov_q5"),
      type=c(rep("Pop. density (ref: medium)",2),
             rep("Urban/rural (ref: rural)",2),
             rep("Quintile of SEP (ref: Q5)",5),
             rep("Geography"),
             rep("Language region (ref: German)",2),
             rep("June referendum (ref: Q5)",5),
             rep("November referendum (ref: Q5)",5)),
      lab=c("High population density","Low population density",
            "Rural","Urban",
            "SEP Q1","SEP Q2","SEP Q3","SEP Q4","SEP Q5",
            "International border",
            "French","Italian",
            "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5",
            "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5"
      ))
  }
  mm = tibble(
    strata=c("age_group40-59","age_group60-69","age_group70-79","age_group80+"),
    age=c("40-59","60-69","70-79","80+")
  )
  if(compute_cri) {
    tt = mod$summary.fixed %>% 
      tibble::rownames_to_column() %>% 
      as_tibble() %>% 
      tidyr::separate(rowname,sep=":",into=c("age","rowname")) %>% 
      left_join(nn,by = join_by(rowname)) %>% 
      mutate(age=gsub("age_group","",age)) %>% 
      filter(!is.na(lab)) %>% 
      mutate(`0.025quant`=exp(mean-1.96*sd),
              `0.975quant`=exp(mean+1.96*sd),
             mean=exp(mean))
  } else {
    tt = exp(mod$summary.fixed) %>% 
      tibble::rownames_to_column() %>% 
      as_tibble() %>% 
      tidyr::separate(rowname,sep=":",into=c("age","rowname")) %>% 
      left_join(nn,by = join_by(rowname)) %>% 
      mutate(age=gsub("age_group","",age)) %>% 
      filter(!is.na(lab))
  }
  g1 = tt %>% 
    ggplot() +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=age),
                    position=position_dodge(.4)) +
    geom_hline(yintercept=1,linetype=2) +
    labs(y="Relative excess mortality",x=NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.position=c(.77,.87),
          legend.background = element_blank(),
          legend.direction="horizontal") +
    facet_grid(~type,scales="free_x",space="free") +
    labs(y="Relative excess mortality",title="Association with relative excess mortality",colour=NULL) 
  
  return(g1)
}

drivers_plot_age_final = function(mod,dat,compute_cri=FALSE) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "type_rural","type_urban",
              "sep1","sep2","sep3","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q3","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q3","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Urban/rural (ref: peri-urban)",2),
           rep("Quintile of SEP (ref: Q5)",5),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q5)",5),
           rep("November referendum (ref: Q5)",5)),
    lab=c("High population density","Low population density",
          "Rural","Urban",
          "SEP Q1","SEP Q2","SEP Q3","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q3","Vote yes Q4","Vote yes Q5"
    ))
  order_type = levels(factor(nn$type))[c(7,6,1,2,4,3,5)]
  mm = tibble(
    strata=c("age_group40-59","age_group60-69","age_group70-79","age_group80+"),
    age=c("40-59","60-69","70-79","80+")
  )
  if(compute_cri) {
    tt = mod$summary.fixed %>% 
      tibble::rownames_to_column() %>% 
      as_tibble() %>% 
      tidyr::separate(rowname,sep=":",into=c("age","rowname")) %>% 
      left_join(nn,by = join_by(rowname)) %>% 
      mutate(age=gsub("age_group","",age)) %>% 
      filter(!is.na(lab)) %>% 
      mutate(`0.025quant`=exp(mean-1.96*sd),
             `0.975quant`=exp(mean+1.96*sd),
             mean=exp(mean))
  } else {
    tt = exp(mod$summary.fixed) %>% 
      tibble::rownames_to_column() %>% 
      as_tibble() %>% 
      tidyr::separate(rowname,sep=":",into=c("age","rowname")) %>% 
      left_join(nn,by = join_by(rowname)) %>% 
      mutate(age=gsub("age_group","",age)) %>% 
      filter(!is.na(lab))
  }
  g1 = tt %>% 
    mutate(type=factor(type,levels=order_type)) %>% 
    ggplot() +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=age),
                    position=position_dodge(.4)) +
    geom_hline(yintercept=1,linetype=2) +
    labs(y="Relative excess mortality",x=NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.position=c(.82,.87),
          legend.background = element_blank(),
          legend.direction="horizontal") +
    facet_grid(~type,scales="free_x",space="free") +
    labs(y="Relative excess mortality",colour=NULL) 
  
  return(g1)
}

drivers_plot_final = function(mod1,mod2,mod3,mod4,dat) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "type_rural","type_urban",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Urban or rural (ref: intermediate)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q3)",4),
           rep("November referendum (ref: Q3)",4)),
    lab=c("High population density","Low population density",
          "Rural","Urban",
          "SEP Q1","SEP Q2","SEP Q4","SEP Q5",
          "International border",
          "French","Italian",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5",
          "Vote yes Q1","Vote yes Q2","Vote yes Q4","Vote yes Q5"
    ))
  tt = mod1$summary.fixed %>% 
    mutate(`0.025quant`=exp(mean-1.96*sd),
           `0.975quant`=exp(mean+1.96*sd),
           mean=exp(mean)) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab)) %>% 
    mutate(model="Final model")
  
  tt2 = exp(mod2$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab),
           grepl("lang",rowname))  %>% 
    mutate(model="Additional covariates")
  
  tt3 = exp(mod3$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab),
           grepl("vote",rowname))  %>% 
    mutate(model="Additional covariates")
  
  tt4 = exp(mod4$summary.fixed) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    left_join(nn,by = join_by(rowname)) %>% 
    filter(!is.na(lab),
           grepl("vote",rowname))  %>% 
    mutate(model="Additional covariates")
  
  tt_f = bind_rows(tt,tt2,tt3,tt4)
  
  type_order = unique(tt_f$type)
    tt_f2 = tt_f %>% 
      mutate(type2=factor(type,levels=type_order),
             model2=factor(model,levels=c("Final model","Additional covariates")))
  
  g1 = tt_f2 %>% 
    ggplot() +
    geom_hline(yintercept=1,linetype=2) +
    geom_pointrange(aes(x=lab,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=model2,shape=model2),fill="white") +
    labs(y="Relative excess mortality",x=NULL) +
    scale_colour_manual(values=c("dodgerblue","orange")) +
    # scale_y_log10(limits=c(.9,1.25)) +
    scale_shape_manual(values=c(21,22)) +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.position=c(.89,.8),
          legend.background =element_blank(),
          strip.text = element_text(size=6.5)) +
    facet_grid(~type2,scales="free_x",space="free") +
    labs(y="Relative excess mortality",colour=NULL,shape=NULL,alpha=NULL) 
  
  return(g1)
}

drivers_plot2 = function(mod,dat) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "type_rural","type_urban",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Urban or rural (ref: intermediate)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q3)",4),
           rep("November referendum (ref: Q3)",4)),
    lab=c("High population density","Low population density",
          "Rural","Urban",
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

interactions_plot = function(mod,dat) {
  nn = tibble(
    rowname=c("density_high","density_low",
              "type_rural","type_urban",
              "sep1","sep2","sep4","sep5",
              "border",
              "lang_fr","lang_it",
              "vote_jun_q1","vote_jun_q2","vote_jun_q4","vote_jun_q5",
              "vote_nov_q1","vote_nov_q2","vote_nov_q4","vote_nov_q5"),
    type=c(rep("Pop. density (ref: medium)",2),
           rep("Urban or rural (ref: intermediate)",2),
           rep("Quintile of SEP (ref: Q3)",4),
           rep("Geography"),
           rep("Language region (ref: German)",2),
           rep("June referendum (ref: Q3)",4),
           rep("November referendum (ref: Q3)",4)),
    lab=c("High population density","Low population density",
          "Rural","Urban",
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

summary_table = function(x) {
  t1 = x %>% 
    group_by(it) %>% 
    summarise(obs=sum(munici_observed),
              exp=sum(munici_exp_deaths),.groups="drop_last") %>% 
    summarise(`Observed`=median(obs),
              `Expected (median)`=round(median(exp)),
              `Expected (lower bound)`=round(quantile(exp,0.025)),
              `Expected (upper bound)`=round(quantile(exp,0.975)),.groups="drop") %>% 
    mutate(`Ratio (median)`=round(`Observed`/`Expected (median)`,2),
           `Ratio (lower bound)`=round(`Observed`/`Expected (upper bound)`,2),
           `Ratio (upper bound)`=round(`Observed`/`Expected (lower bound)`,2)) 
  t2 = x %>% 
    group_by(age_group,sex,it) %>% 
    summarise(obs=sum(munici_observed),exp=sum(munici_exp_deaths),.groups="drop_last") %>% 
    summarise(`Observed`=median(obs),
              `Expected (median)`=round(median(exp)),
              `Expected (lower bound)`=round(quantile(exp,0.025)),
              `Expected (upper bound)`=round(quantile(exp,0.975)),.groups="drop") %>% 
    mutate(`Ratio (median)`=round(`Observed`/`Expected (median)`,2),
           `Ratio (lower bound)`=round(`Observed`/`Expected (upper bound)`,2),
           `Ratio (upper bound)`=round(`Observed`/`Expected (lower bound)`,2)) 
  tt = bind_rows(t2,t1) %>% 
    replace_na(list(age_group="Total",sex="Total")) %>% 
    rename(`Age group`=age_group,`Sex`=sex)
  return(tt)
}

correlogram = function(x) {
  library(ggcorrplot)
  xx = x %>% 
    dplyr::filter(it==1) %>% 
    dplyr::select(r_urban1,median_ssep3_q,r_lang,vote_yes_nov_cat,vote_yes_jun_cat) %>% 
    dplyr::transmute(`Urban/rural`=as.numeric(as.factor(r_urban1)),
                  `Median SEP`=as.numeric(as.factor(median_ssep3_q)),
                  `Language`=as.numeric(as.factor(r_lang)),
                  `June referendum`=as.numeric(as.factor(vote_yes_nov_cat)),
                  `November referendum`=as.numeric(as.factor(vote_yes_jun_cat)))
  cc = cor(xx)
  g = ggcorrplot::ggcorrplot(cc,
                             ggtheme = ggplot2::theme_bw,
                         hc.order = TRUE,
                         lab = TRUE,
                         legend.title="Correlation") 
  return(g)
}