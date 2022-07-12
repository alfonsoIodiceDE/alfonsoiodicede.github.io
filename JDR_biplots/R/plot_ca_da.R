plot_ca_da <- function(ca_sol,  selected_dims=c(1,2), thresh=.9){
  library(tidyverse)
  library(janitor)
  library(ggrepel)


  
  ### coordinates preparation
  
  r_stand_co = bind_cols(ca_sol$rowcoord[,selected_dims],what="st_co", 
                         label = ca_sol$rownames,
                         alpha_txt=TRUE,
                         alpha_arrow=FALSE)%>% clean_names()
  
  r_prin_co = bind_cols(ca_sol$rowcoord[,selected_dims]* ca_sol$sv[selected_dims],what="pr_co",
                        label = ca_sol$rownames,
                        alpha_txt=TRUE,
                        alpha_arrow=FALSE)%>% clean_names()
  
  r_contr_co  = bind_cols((ca_sol$rowcoord * (ca_sol$rowmass^.5))[,selected_dims],
                          what = "con_co",
                          label = ca_sol$rownames,
                          ) %>% clean_names() %>% 
                          mutate(dist_o=sqrt(dim1^{2} + dim2^{2}),
                                 alpha_txt = ifelse(dist_o > quantile(dist_o,thresh),TRUE,FALSE),
                                 alpha_arrow = alpha_txt
                          ) %>% 
                          select(-dist_o)
                          
  # r_stand_co$top=r_contr_co$top
  
  row_coords = bind_rows(r_stand_co, r_prin_co, r_contr_co) %>% clean_names() 
  
  
  c_stand_co = bind_cols(ca_sol$colcoord[,selected_dims],what="st_co", 
                         label = ca_sol$colnames)
  c_prin_co = bind_cols(ca_sol$colcoord[,selected_dims]* ca_sol$sv[selected_dims],what="pr_co", 
                        label = ca_sol$colnames)
  c_contr_co  = bind_cols((ca_sol$colcoord * (ca_sol$colmass^.5))[,selected_dims],what = "con_co", 
                          label = ca_sol$colnames)
  
  
  col_coords = bind_rows(c_stand_co, c_prin_co, c_contr_co) %>% clean_names() 
    
  ### plots
  
  contribution_biplot = ggplot(data=col_coords %>% filter(what=="pr_co"),
                               aes(x=dim1,y=dim2)) +
    theme_minimal()+
    geom_text(aes(label=label),color="red") +
    geom_segment(data = row_coords %>% filter(what=="con_co",alpha_arrow),aes(x=0,xend=dim1,y=0,yend=dim2),
                 arrow=arrow(length = unit(.1,"inches")),color="dodgerblue")+
    geom_label_repel(data = row_coords %>% filter(what=="con_co",alpha_txt),
                    aes(x = dim1, y = dim2, label = label),color="dodgerblue",inherit.aes = "FALSE")
  
  col_biplot = ggplot(data = col_coords %>% filter(what=="pr_co"),aes(x=dim1,y=dim2)) +
    theme_minimal()+
    geom_text(aes(label=label),color="red") +
    geom_label_repel(data=row_coords %>% filter(what=="st_co"),
              aes(x=dim1, y=dim2, label = label),color="dodgerblue",inherit.aes = "FALSE")

  symmetric_map = ggplot(data = col_coords %>% filter(what=="pr_co"),aes(x=dim1,y=dim2)) +
    theme_minimal()+
    geom_text(aes(label=label),color="red") +
    geom_label_repel(data=row_coords %>% filter(what=="pr_co"),
              aes(x=dim1, y=dim2, label = label),color="dodgerblue",inherit.aes = "FALSE") 
  
    
  ani_row_coords = row_coords %>% filter(what %in% c("st_co","con_co")) %>% 
    mutate(what=fct_relevel(what,"st_co"))
  
  ani_biplot = ggplot(data = ani_row_coords,aes(x=dim1,y=dim2)) + theme_minimal()+
    geom_text(aes(label=label,group = 1L, alpha = alpha_txt),color="dodgerblue") +
    geom_segment(aes(x=0,xend=dim1,y=0,yend=dim2,group = 1L,alpha=alpha_arrow),
                 arrow=arrow(length = unit(.1,"inches")),color="dodgerblue")+
    scale_alpha_manual(values = c(.01,1))+
    geom_text(data=col_coords %>% filter(what=="pr_co") %>% select(-what),
              aes(x=dim1, y=dim2, label = label),color = "red",inherit.aes = "FALSE") + 
    transition_states(what,wrap = FALSE,transition_length = 5) +
    view_follow() + theme(legend.position = "none")
  
  # anim_save(animation=ani_biplot,file="standard_to_contribution_biplot.gif")
    
  
  # ani_biplot = animate(ani_biplot,rewind = FALSE)
  
  plots=list()
  plots$contribution_biplot = contribution_biplot
  plots$col_biplot = col_biplot
  plots$symmetric_map = symmetric_map
  plots$ani_biplot = ani_biplot
  return(plots)
  }
