library(tidyverse)

df <- tibble(
  grp = sample(2,10,replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df %>% 
  group_by(grp) %>% 
  summarise(across(everything(),median))


rnorm_na <- function(n,n_na,mean = 0,sd = 1){
  sample(c(rnorm(n- n_na,mean = mean, sd = sd), rep(NA,n_na)))
}

df_miss <- tibble(
  a = rnorm_na(5,1),
  b = rnorm_na(5,1),
  c = rnorm_na(5,2),
  d = rnorm(5)
)

df_miss %>% 
  summarize(
    across(a:d,median,na.rm = TRUE),
    n = n()
  )


df_miss %>% 
  summarize(
    across(a:d, \(x) median(x, na.rm = TRUE)),
    n = n()
  )


df_miss %>% 
  summarise(
    across(a:d,list(
    median = \(x) median(x, na.rm = TRUE),
    n_miss = \(x) sum(is.na(x))
    
  ),
  .names = "{.fn}_{.col}"
  ),
  n = n())

## always use the .names when you use the 

## coalesce() is used to replace NAs with 0
df_miss %>% 
  mutate(
    across(a:d,\(x) coalesce(x,0))
  )


# USING across() WITH  filter(O -------------------------------------------
## filter out a row if any of it's column value is NA
df_miss %>% 
  filter(if_any(a:d,is.na))

## filters out a row with all column  values NA
df_miss %>% 
  filter(if_all(a:d,is.na))


summarize_means <- function(df,summary_vars = where(is.numeric)){
  df %>% 
    summarise(
      across({{summary_vars}}, \(x) mean(x, na.rm = TRUE)),
             
      n = n()
    )
}

diamonds %>% 
  group_by(cut) %>% 
  summarize_means()

df %>% 
  summarize_means()



diamonds %>% 
  group_by(cut) %>% 
  summarize_means(c(carat,x:z))

z_score <- function(x){
  (x-mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE)
}

df
long <- df %>% 
  pivot_longer(a:d) %>% 
  group_by(name) %>% 
  summarise(
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    max = max(value),
    min = min(value),
    IQR = IQR(value)
  )
long

df %>% 
  pivot_longer(a:d)

## u can then pivot_wider back

long %>% 
  pivot_wider(
    names_from = name,
    values_from = c(median,mean,sd,max,min,IQR),
    names_vary = 'slowest',
    names_glue = "{name}_{.value}"
  )



# READING MULTIPLE FILES --------------------------------------------------

paths <- list.files("data/gapminder",pattern = "[.]xlsx$",full.names = TRUE)

paths
















  
