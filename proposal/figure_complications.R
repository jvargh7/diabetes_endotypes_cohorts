


df = readxl::read_excel("proposal/HR.xlsx") %>% 
  mutate(across(HR:uci,~as.numeric(.)))


cluster_colors_ehr = c("MOD (ref)"="#F8BDA4","Mixed"="darkgreen","SIDD"="#4682b4","MARD"="#D0ACC9")

fig = df %>% 
  mutate(outcome = factor(outcome,levels=c("Retinopathy","Stroke","HFpEF","Any Dementia")),
         subtype = factor(subtype,levels=c("MOD","SIDD","MARD","Mixed"),
                          labels=c("MOD (ref)","SIDD","MARD","Mixed"))) %>% 
  ggplot(data=.,
         aes(x=HR,xmin=lci,xmax=uci,col=subtype,y=outcome)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbarh(height=0.1,position=position_dodge(width=0.9)) +
  theme_bw() +
  scale_color_manual("",values=cluster_colors_ehr) +
  xlab("Hazard Ratio (95% CI)") +
  ylab("") +
  theme(legend.position = "bottom",
        legend.margin=margin(c(-3,5,5,5)),
        axis.text.y=element_text(size=14)) +
  geom_vline(xintercept=1.0,col="black",linetype=2) + 
  guides(color = guide_legend(nrow=2,ncol=2))

fig %>% 
  ggsave(.,filename=paste0("C:/Cloud/OneDrive - Emory University/Proposals/R01 T2DM Subphenotype Surveillance/figures/complications.jpg"),width = 4,height = 4)
