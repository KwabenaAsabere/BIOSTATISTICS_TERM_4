library(tidyverse)

df <- tibble(
  grp = sample(2,10,replace = TRUE),
  a = rnorm(10),
  b =rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


## using across()
df %>% 
  summarise(
    n = n(),
    across(a:d,median)
  )

df %>% 
  group_by(grp) %>% 
  summarize(across(everything(),median))

nepal <- read_csv("nepal_anthro.csv")

nepal %>% 
  drop_na() %>% 
  group_by(sex) %>% 
  summarise(across(everything(),mean))










