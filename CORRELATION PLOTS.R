library(tidyverse)
data("mtcars")
mtcars_cormat <- cor(mtcars) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname)

mtcars_cormat %>% 
  ggplot(aes(x = rowname, y = name, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value,2)), color = "white")+
  scale_fill_gradient2(low ="red",
                      high = "darkgreen",
                      mid = "white",midpoint = 0)

carrs_corr <- cor(mtcars) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname) 

