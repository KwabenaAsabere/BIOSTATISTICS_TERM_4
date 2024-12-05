library(tidyverse)
library(gapminder)
theme_set(theme_bw())



data("gapminder")
glimpse(gapminder)
str(gapminder)
gapminder$year %>% unique()
gapminder$country %>% n_distinct()
gapminder$continent %>% unique()

gapdata2007 <- gapminder %>% 
  filter(year == 2007)
gapdata2007

gapdata <- gapminder

gapdata2007 %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent))+
  geom_point( shape = 1)+
  facet_wrap(~continent)


gapdata2007 %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent))+
  geom_point( shape = 1)+
  facet_wrap(~ pop > 50000000)+
  theme_bw()


## bubble plot
gapdata2007 %>% 
  ggplot(aes(x = gdpPercap/1000, y = lifeExp, size = pop))+
  geom_point(shape = 1, alpha = 0.5)

### Line plots/ Time series plots

gapdata %>% 
  filter(country == "United Kingdom") %>% 
  ggplot(aes(x =year, y  = lifeExp))+
  geom_line()


gapdata %>% 
  ggplot(aes(x =year, y  = lifeExp,group = country))+
  geom_line(aes(color = continent)) +
  facet_wrap(~continent)+
  scale_color_brewer(palette = "Paired")

### Bar plots

gapdata2007 %>% 
  filter( country %in% c("United Kingdom" ,"France", "Germany")) %>% 
  ggplot(aes( x = country, y = lifeExp)) +
  geom_col()

## if data is already summarised or includes values for y used geom_col
## if you want ggplot() to count up the numb of rows used geom_bar()

## countable data
## geom bar requires a single  variable aes( x = ) where x is a categorical variable

gapdata2007 %>%
  count(continent)

gapdata2007 %>% 
  ggplot(aes (x = continent))+
  geom_bar()

  
gapdata2007 %>% 
  ggplot(aes (x = continent, colour = country))+
  geom_bar(fill = NA)+
  theme(legend.position = "none")

  
gapdata2007 %>% 
  ggplot(aes (x = continent))+
  geom_bar( fill = "#FF0099")
  

## filling the bar up by proportions
gapdata2007 %>% 
  ggplot(aes (x= "Global", fill= continent))+
  geom_bar()


gapdata2007 %>% 
  ggplot(aes (x = continent))+
  geom_bar( fill = "steelblue")+
  coord_flip()



gapdata2007 %>% filter(continent == "Europe") %>% 
  ggplot(aes (x = fct_reorder(country,lifeExp),y= lifeExp, colour = country ))+
  geom_col( fill = "NA")+
  coord_flip()+
  theme(legend.position = "none")


###  Histograms
gapdata2007 %>% 
  ggplot(aes(x = lifeExp))+
  geom_histogram( binwidth = 10)

gapdata2007 %>% 
  ggplot(aes(x = lifeExp))+
  geom_density()

gapdata2007 %>% 
  ggplot(aes(x = lifeExp))+
  geom_freqpoly()


## Box Plots

gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()
  
 ## Multiple Geoms

gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_point()



gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_jitter()


gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp, colour = continent))+
  geom_boxplot()+
  geom_jitter()
continent

gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_jitter(aes(color = continent))


label_data <- gapdata2007 %>% 
  group_by(continent) %>% 
  filter( lifeExp == max(lifeExp)) %>% 
  select( country, continent, lifeExp)

label_data


gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_jitter(aes(color = continent))+
  geom_label(data = label_data, aes(label = country))


gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_jitter(aes(color = continent))+
  geom_label( aes(label = country))

gapdata2007 %>% 
  ggplot(aes( x = continent, y = lifeExp))+
  geom_boxplot()+
  geom_jitter(aes(color = continent))+
  geom_text(data = label_data, aes(label = country))



gapdata %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes( y = fct_reorder(country,lifeExp,.fun = max), x = lifeExp,
              color = year))+
  geom_point( shape = 15, size = 2) +
  scale_color_distiller(palette = "Greens", direction = 1)
































































  
  
  
  
  
  
  
  
  

















































  
  
  
  
  











































































































