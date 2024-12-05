library(tidyverse)
 predictions %>% 
  filter(
    year == 2020
  )
 
 predictions %>% 
   filter(
     year >= 1900 & year <= 2000
   )

 
 predictions %>% 
   distinct(year,.keep_all = TRUE) %>% 
   arrange(desc(year))
 
 

  predictions %>% 
   filter(
     between(year,1900,2000)
       )
  
  
 predictions %>% 
    group_by(id) %>% 
    summarise(n_predictions = n(),
      n_shadow = sum(shadow == TRUE)) %>% 
  arrange(desc(n_predictions))

     
    
   
 

