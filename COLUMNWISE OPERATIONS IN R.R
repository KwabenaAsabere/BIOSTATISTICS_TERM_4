library(tidyverse)
df_xyz

df_xyz %>% 
  summarise(
    across(
      .cols = c(x:z),
      .fns = mean,
      .names = "{col}_mean"
    )
  )



## pass a list of name-function pairs to get output of names

df_xyz %>% 
  summarise(
    across(
      .cols  = everything(),
      .fns   = list(mean = mean,sd = sd),
      na.rm  = TRUE,
      .names = "{col}_{fn}"
    )
  )

df_xyz %>% 
  summarise(
    across(everything(),\ (x) mean(x,na.rm = TRUE))
  )


## purr-style lambda syntax

df_xyz %>% 
  summarise(
    across(
      .cols  = everything(),
      .fns   = ~ mean(.x, na.rm = TRUE),
      .names = "{col}_mean"
    )
  )


## Across function with mutate
set.seed(123)
demographics <- tibble(
  id       = 1:10,
  age      = c(sample(1:30, 9, TRUE), NA),
  race     = c(1, 2, 1, 4, 7, 1, 2, 9, 1, 3),
  hispanic = c(7, 0, 1, 0, 1, 0, 1, 9, 0, 1),
  edu_4cat = c(4, 2, 9, 1, 2, 3, 4, 9, 3, 3),
  inc_6cat = c(1, 4, 1, 1, 5, 3, 2, 2, 7, 9)
) %>% 
  print()


## to replace all 9s and 7s with NAs

demographics %>% 
  mutate(
    across(
      .cols = c(-age , -id),
      .fns = ~if_else(.x == 9|.x ==7,NA_real_, .x)
    )
  )



set.seed(123)
drug_trial <- tibble(
  id           = 1:10,
  se_headache  = sample(0:1, 10, TRUE),
  se_diarrhea  = sample(0:1, 10, TRUE),
  se_dry_mouth = sample(0:1, 10, TRUE),
  se_nausea    = sample(0:1, 10, TRUE)
) %>% 
  print()




summary_stats <- study %>% 
  summarise(
    across(
      .cols = c(age, ht_in, wt_lbs, bmi),
      .fns  = list(
        n_miss = ~ sum(is.na(.x)),
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE)
      )
    ) 
  ) %>% 
  print()
summary_stats



# across() with filter() --------------------------------------------------







































