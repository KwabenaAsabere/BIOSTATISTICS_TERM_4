library(tidyverse)
date_example <- read_csv("R _data_example1.csv")
date_example <- read_csv("R _data_example1.csv")
head(date_example)

date_example <- date_example %>% 
  mutate(
    vdate = mdy(visitdate),
    bdate = mdy(birthdate)
  )
date_example  
str(date_example)

date_example %>% 
  mutate(age_days = difftime(vdate,bdate),
         ade_days2 = vdate-bdate)

## time intervals
date_example <-  date_example %>% 
  mutate(time_dur = vdate %--%bdate,
         age_dur = as.duration(vdate-bdate),
       age_days = vdate-bdate )

date_example$age_dur %>% 
  summary()

summary(as.numeric(date_example$age_days))
temp <- date_example %>% 
  filter(is.na(age_dur))
temp
 
bike_chicago <- read_csv("202301-divvy-tripdata.csv")

head(bike_chicago)

bike_chicago <- bike_chicago %>% 
  mutate(start_daytime = mdy_hm(started_at),
         end_daytime = mdy_hm(ended_at)) %>% 
  relocate(14:15,.before = 3)


head(bike_chicago)

bike_chicago <- bike_chicago %>% 
  mutate(ride_duration = difftime(end_daytime,start_daytime,units=c("mins")),
         ride_dur = as.duration(end_daytime - start_daytime)) %>% 
           relocate(c(ride_duration,ride_dur),.after = end_daytime)

summary(bike_chicago$ride_dur)

summary(as.numeric(bike_chicago$ride_duration))

bike_chicago %>% 
  filter(ride_duration > 10000)

bike_chicago %>% 
  filter(ride_dur > 60* 10000)

bike_chicago %>% 
  reframe(quantile(ride_duration,c(0.90,0.95,0.99,0.995)))


quantile(bike_chicago$ride_duration,c(0.90,0.95,0.99,0.995))

##remove extremes for analysis  >120 mins

bike_chicago2 <- bike_chicago %>% 
  filter(ride_duration >0 & ride_duration <= 120)
bike_chicago2 
summary(bike_chicago2$ride_dur)


bike_chicago %>% 
  ggplot(aes(x = ride_dur))+
  geom_histogram()

bike_chicago2 %>% 
  ggplot(aes(x=ride_dur))+
  geom_histogram()+
  theme_bw()


bike_chicago2 %>% 
  ggplot(aes(ride_dur))+
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)))+
  theme_bw()

## extract week day and make a boxplots of duration by weekday

bike_chicago2 <- bike_chicago2 %>% 
  mutate(day = wday(start_daytime,label = TRUE)) %>% ##label = TRUE tells us actual day
  relocate(day,.after = ride_dur)

## boxplot of duration by day of the week
bike_chicago2 %>% 
  ggplot(aes(x = day,y = ride_duration,fill = day))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(0,120),breaks = seq(0,120,10))

## boxplot of duration by bike type
bike_chicago2 %>% 
  ggplot(aes(x = rideable_type,y = ride_duration,fill = rideable_type))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(0,120),breaks = seq(0,120,10))

##boxplot of duration by member type
bike_chicago2 %>% 
  ggplot(aes(x = member_casual,y = ride_duration,fill = member_casual))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(0,120),breaks = seq(0,120,10))




























