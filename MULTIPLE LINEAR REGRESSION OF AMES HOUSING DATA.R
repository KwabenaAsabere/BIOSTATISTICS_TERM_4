library(tidyverse)
library(finalfit)
library(broom)
library(ggsci)

ames <- read_csv("ames_housing.csv")




ames %>% 
  ggplot(aes(x =Sale_Price))+
  geom_histogram()+
  scale_color_brewer(palette= PiYG)



head(ames, n= 5)
ames %>% 
  ggplot(aes(x =Sale_Price))+
  geom_histogram()+
  theme_bw()


ames %>% 
  ggplot(aes(x= Gr_Liv_Area,y= Sale_Price))+
  geom_point()+
  theme_minimal()


ames %>% 
  filter(Gr_Liv_Area > 4000) %>% 
  arrange(Sale_Price)


