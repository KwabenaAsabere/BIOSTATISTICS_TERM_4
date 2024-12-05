library(tidyverse)


df_summary <- function(df,var){
  df %>% 
    summarise(
      mean = mean({{var}},na.rm = TRUE),
      sd = sd({{var}},na.rm = TRUE),
      min = min({{var}}),
      max = max({{var}})
    )
}

 mtcars %>% group_by(car) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(
      mean = ~mean(.x,na.rm = TRUE),
      sd = ~sd(.x,na.rm = TRUE),
      min = ~min(.x),
      max = ~max(.x)
      
    ),
    .names = "{col}_{fn}"
  )) %>%  
   as_tibble() %>% 
    pivot_longer(cols = everything())
 
 list <- list(
   mean = ~mean(.x,na.rm = TRUE),
   sd = ~sd(.x,na.rm = TRUE))
 
 map(mtcars,)


summ_fxn <- function(df){  df %>% 
   summarise(across(
     .cols = where(is.numeric),
     .fns = list(
       mean = ~mean(.x,na.rm = TRUE),
       sd = ~sd(.x,na.rm = TRUE)
       
     ),
     .names = "{col}_{fn}"
   )) %>% 
   as_tibble() %>% 
   pivot_longer(cols = everything())
}

summ_fxn(mtcars) 
framingham <- read_csv("framingham.csv")
summ_fxn(framingham)




 map_dfr(mtcars,mean) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Mean")

Sd <- map_dfr(mtcars,sd) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "sd")

min_cars <- map_dfr(mtcars,min) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "min")
max <-  map_dfr(mtcars,max) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "max")
df1 <- full_join(mean,sd,by = "Variable")
df2 <- full_join(max,min,by ="Variable")


df <- full_join(df1,df2,by = "Variable")
df
df %>% 
  ggplot(aes(x =Variable,y= Mean))+
  geom_errorbar(aes(ymin = min,ymax = max))+
  coord_flip()

mtcar <- mtcars
mtcar


              









