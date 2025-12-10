source(".Rprofile")

dec_an02 = read_csv(paste0(path_endotypes_folder,"/working/processed/dec_an02_clean_kmeans_5var_mi_knn_cluster.csv"))

means = dec_an02 %>% 
  bind_rows({.} %>% mutate(cluster="Pooled")) %>% 
  group_by(cluster) %>% 
  summarize(across(one_of("bmi","hba1c","dmagediag","homa2b","homa2ir"),.fns=~mean(.)))

sds = dec_an02 %>% 
  bind_rows({.} %>% mutate(cluster="Pooled")) %>% 
  group_by(cluster) %>% 
  summarize(across(one_of("bmi","hba1c","dmagediag","homa2b","homa2ir"),.fns=~sd(.)))


write_csv(means,"analysis/kmeans/dec_an01_cluster means.csv")
write_csv(sds,"analysis/kmeans/dec_an01_cluster sd.csv")
