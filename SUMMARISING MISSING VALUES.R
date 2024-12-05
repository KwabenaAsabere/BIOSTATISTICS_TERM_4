library(tidyverse)
framingham <- read_csv("framingham.csv")

framingham %>% 
  summarise(
    across(
      .cols= everything(),
      .fns = list(~sum(is.na(.x))
      
    )
  )) %>% 
  t() 
  
  
  summary_table <- data.frame(
    Counts = sapply(framingham, function(x) length(x)),
    Missing = sapply(framingham, function(x) sum(is.na(x))),
    Percentage_Missing = sapply(framingham, function(x) mean(is.na(x)) * 100)
  )
  
 
  
  
  summ_table <- map_dfr(framingham, ~ data.frame(
    Counts = length(.x),
    Missing = sum(is.na(.x)),
    Percentage_Missing = mean(is.na(.x)) * 100
  ), .id = "Variable")

  
  framingham %>% 
    map_dfr( ~ tibble(
    Counts = length(.x),
    Missing = sum(is.na(.x)),
    Percentage_Missing = mean(is.na(.x)) * 100
  ), .id = "Variable")

    
    
  
  
  
  
  
  
  
  
  
  
  
  