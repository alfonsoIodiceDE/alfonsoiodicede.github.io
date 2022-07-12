library("tidyverse")
library("clustrd")
library("mlbench")
library("ggrepel")
data(Zoo)



Zoo = as_tibble(Zoo) %>% filter(type %in% c("mammal","bird","fish","insect")) %>% 
  mutate(across(everything(),as_factor))

animal_names=rownames(Zoo)

type = Zoo %>% mutate(type= fct_drop(type)) %>% pull(type)


Zoo = Zoo %>% select(-type)

zoo_cca = clusmca(Zoo,nclus=4,ndim = 3,nstart=100,gamma = TRUE)
table(type,zoo_cca$cluster)

ani_coords = as_tibble(zoo_cca$obscoord) %>% mutate(animal_names) %>% 
  rename(d1=X1,d2=X2) %>% mutate(type=type,cluster=as_factor(zoo_cca$cluster))

var_coords = tibble(var_names=names(zoo_cca$odata)) %>%  
mutate (levs=map(zoo_cca$odata,~levels(.x))
        ) %>% unnest(levs) %>% cbind(zoo_cca$attcoord) %>% 
  rename(d1=X1,d2=X2)

var_coords_sel = var_coords %>% filter(levs!="FALSE")


var_coords_sel %>%
  unite("long_names", var_names:levs,sep = "_") %>% 
  mutate(labs = str_remove(long_names,"_TRUE") ) %>% 
  ggplot(aes(x=d1,y=d2)) + geom_text_repel(aes(label=labs))+
  geom_jitter(data=ani_coords,aes(x=d1,y=d2, color=type,shape=cluster),alpha=.5,inherit.aes = FALSE)
  
  
