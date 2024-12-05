library(tidyverse)
install.packages("viridis")
library(scales)

         
          options(ggplot2.continuous.colour = "viridis")

         ggplot(mtcars,aes(x=mpg,y = disp,color = gear))+
           geom_point(size = 3)+
           theme_bw()+
           scale_color_viridis_c(option ="viridis")

         
          data(diamonds)         

ds %>%
  ggplot(aes(x = price))+
  geom_histogram()+
  theme_bw()+
  scale_color_viridis_b(option = "viridis")

ds <- diamonds[seq(1,50000,100),] ## sequence from 1 to 50000 with a step of 100
## select every 100th row

monkeymem <- read_csv("https://dcgerard.github.io/stat_412_612/data/monkeymem.csv")
monkeymem

monkeymem_tidy <-  monkeymem %>% 
  pivot_longer(cols = Week2:Week16,
               names_to = "Week",
               values_to = "Value")




monkeymem_tidy %>% 
  ggplot(aes(x = Week, y= Value,color = Treatment))+
  geom_boxplot() +
  scale_color_viridis_d(option = "turbo") +
  theme_bw()   


monkeymem_tidy %>% 
  ggplot(aes(x = Week, y= Value,color = Treatment))+
  geom_boxplot() +
  theme_bw()


monkeymem_tidy %>% 
  filter(Monkey == "Spank") %>% 
  ggplot(aes(x = Week, y = Value,group = Treatment))+
  geom_line(aes(color = Treatment)) 


 data(Orange) 
 
Orange %>% 
  ggplot(aes(x = age, y = circumference, colour = Tree))+
  geom_point()+
  geom_line(linewidth = 1)+
  theme_bw()


flowers1<- read_csv2("https://dcgerard.github.io/stat_412_612/data/flowers1.csv")
flowers1
folwers_tidy <- flowers1 %>%
  pivot_wider(id_cols = c(Time,replication),
              names_from = Variable,
              values_from = Value)

flowers2<- read_csv2("https://dcgerard.github.io/stat_412_612/data/flowers2.csv")

flowers2_tidy <- flowers2 %>% 
  separate(
    col = `Flowers/Intensity`,
    into =c("flowers","intensity"),
    sep = "/"
  )


flowers2_tidy


data(flights)
library(nycflights13)

flights_time <- flights %>% 
  select(month,day,hour,minute) %>% 
  unite(col = "time",
        c(hour,minute),
        sep = ":")

flights_time
flights_time <-  flights_time %>% 
  mutate(time = as_datetime(time))

flights_time$time


edwina_full <- read_csv("District-GradeGender_2016-2017.csv")

edwina_full_wide <- edwina_full %>% na.omit() %>% 
  pivot_wider(id_cols = c(ORG_CODE:COUNTY),
              names_from = GENDER,
              values_from = Gr.10)













