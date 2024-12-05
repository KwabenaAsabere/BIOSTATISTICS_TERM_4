library(tidyverse)


bike_chicago <- read_csv("202301-divvy-tripdata.csv")

str(bike_chicago)

head(bike_chicago)
names(bike_chicago)

dim(bike_chicago)

bike_chicago %>% 
  select(ride_id,ended_at)

 
bike_chicago %>% 
  summarise(length(unique(ride_id)))

length_variable <- function(df,var){
  df %>% 
    summarise(length(unique({{var}})))
}                        

length_variable(bike_chicago,start_station_id)
length_variable(bike_chicago,ended_at)

temp <- bike_chicago %>% 
  group_by(start_station_id) %>% 
  filter(row_number()==1) %>% select(start_station_id)

dim(temp)
rm(temp)

bike_chicago <- bike_chicago %>% 
  mutate(end_time = mdy_hm(ended_at),
         start_time = mdy_hm(started_at),
         duration = difftime(start_time,end_time),units =c("mins"))

class(bike_chicago$duration)

bike_chicago %>% 
  select(start_time,end_time,duration)


summary_function <- function(df,var,x){
  df %>% 
    group_by({{var}}) %>% 
    summarise(mean = mean({{x}},na.rm = TRUE),
              median = median({{x}},na.rm = TRUE),
              min = min({{x}}),
              count = n())
}

summary_function(bike_chicago,rideable_type,duration)



bike_chicago %>% 
  group_by(rideable_type) %>% 
  summarise(across(
    .cols = duration,
    .fns= list(
    mean =  ~mean(.x,na.rm = TRUE),
    median=  ~median(.x,na.rm = TRUE),
    min=  ~min(.x,na.rm = TRUE),
   count =   ~n()
    ),
    .names = "{fn}_{col}"
  ))


summary_fxn <- function(df,var,y){
  df %>% 
    group_by({{var}}) %>% 
    summarise(
      across(
        .cols = {{y}},
        .fns=list(
          mean =  ~mean(.x,na.rm = TRUE),
          median=  ~median(.x,na.rm = TRUE),
          min=  ~min(.x,na.rm = TRUE),
          count =   ~n()
      ),
      .names ="{fn}_{col}"
    )) 
}

summary_fxn(bike_chicago,rideable_type,duration)




















































