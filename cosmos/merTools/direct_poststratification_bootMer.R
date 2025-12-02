direct_poststratification_bootMer = function(reference_data = ps,model,
                                             group.vars = "FIPS", 
                                             n.sims = 5,seed=123){
  gc();
  set.seed(seed)
  preds <- bootMer(model,FUN=function(x) predict(x,newdata=reference_data,nsim=n.sims,allow.new.levels=TRUE) )
  
  # Transform simulation results into manageable format
  simResults <- attributes(preds)$sim.results |> 
    invlogit() %>% 
    as.data.frame()
  
  ps_updated    <- ps %>% 
    bind_cols(simResults)
  
  weighted_means <- ps_updated %>% 
    group_by(across(one_of(group.vars))) %>% 
    dplyr::summarize(across(one_of(paste0("V", 1:n.sims)),.fns=~weighted.mean(.,n))) %>% 
    ungroup() %>% 
    mutate(prevalence = apply(.[,str_detect(names(.),"V[0-9]+")],1,mean),
           missing_counts = apply(.[,str_detect(names(.),"V[0-9]+")],1,function(x) sum(is.na(x))),
           median_prevalence = apply(.[,str_detect(names(.),"V[0-9]+")],1,function(x) quantile(x,0.5,na.rm=TRUE)),
           prevalence_lci = apply(.[,str_detect(names(.),"V[0-9]+")],1,function(x) quantile(x,0.025,na.rm=TRUE)),
           prevalence_uci = apply(.[,str_detect(names(.),"V[0-9]+")],1,function(x) quantile(x,0.975,na.rm=TRUE)))
  
  weighted_means %>% 
    dplyr::select(one_of(group.vars),missing_counts,
                  prevalence,median_prevalence,prevalence_lci,prevalence_uci) %>% 
    return(.)
}

