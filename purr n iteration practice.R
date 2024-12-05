library(tidyverse)

four_cyl <- filter(mpg,cyl==4)


four_cyl <- mpg %>% 
  filter(cyl==4)


ninety <- four_cyl %>% 
  filter(cty >= quantile(cty, probs=0.9))

ninety


map(1:3,rnorm)

 cars <- mtcars %>% 
  map(\(x) length(unique(x)))

 cars

 mtcars %>% 
   map( ~length(unique(.x)))
 
## Extracting elements 
 l <- list(
   list(-1,x = 1,y = (2), z = "a"),
   list(-2, x =4, y = c(5,6), z = "b"),
   list(-3, x = 8,y = c(9,10,11))
 )

 map(l ,2)

 map(l, "y") 
 
 
 sample(1:20,10,TRUE)
age <- sample(20:30,10,TRUE) %>% 
  print()

df <- tibble(
  age = sample(20:30,10,TRUE),
  height = sample(150:190,10,TRUE),
  weight = sample(60:100,10,TRUE)
) %>% 
  print()

bm <- function(weigh,eight){
  weight/((height *0.01)^2)
} 


bmi_fxn<- function(data,weight_var,height_var){
  data %>% 
    mutate(bmi = {{weight_var}}/(({{height_var}}*0.01)^2))
}

df <- df %>% 
  bmi_fxn(weight,height)

df %>% 
  summarise(
    across(
      .cols = everything(),
      .fns= list(
        mean = mean,
        sd = sd,
        max = max,
        min =min
        
      ),
      .names = '{col}_{fn}'
    )
  ) %>% 
  pivot_longer(names_to = "Stats",
               cols =everything()) %>% 
  as_tibble()


df %>% 
  summarise(across(where(is.numeric),
                   list(~mean(.x,na.rm =TRUE),
                        ~sd(.x,na.rm = TRUE)
                        )))




df %>% 
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = list(mean = mean,sd = sd),
      na.rm =TRUE,
      .names = "{col}_{fn}"
    )
  )

df <- df %>% 
  mutate(
    bmi_cat = if_else(bmi<18.5,"underweight",
                      if_else(bmi<25,"normal",
                              if_else(bmi<30,"overweight","obese")
                                      ))
  )

skim(df)
skimr::skim(df)
df


df %>% 
  reframe(length(unique(age)))

df %>% 
  count(
    across(age
      
    )
  )
  
df

lab_data <- read_csv("Lab 3 data.csv")

df %>% 
  map(
    .x = age:bmi,
    .f = mean
  )

  
  









 
 
 
 
 