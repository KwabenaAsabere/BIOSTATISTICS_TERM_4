library(tidyverse)
library(broom)
bikes <- read_csv("bikes_chicago_sample.csv")
names(bikes)

class(bikes$duration)

bikes %>% 
  summarise(
    mean_dur = mean(duration,na.rm = TRUE),
    sd_dur = sd(duration,na.rm = TRUE),
    median_dur = median(duration,na.rm = TRUE),
    pct95_dur = quantile(duration,c(0.95)),
    n = n()
  )


bikes %>% 
  group_by(member_casual) %>% 
  summarise( n = n()) %>% 
  summarise( pct = n/sum(n))


bikes %>% 
  group_by(member_casual) %>% 
  reframe( n = n()) %>% 
  reframe( pct = n/sum(n))


bikes %>% 
  group_by(member_casual) %>% 
  summarise(
    mean_dur = mean(duration,na.rm = TRUE),
    sd_dur = sd(duration,na.rm = TRUE),
    median_dur = median(duration,na.rm = TRUE),
    pct95_dur = quantile(duration,c(0.95)),
    n = n()
  )


sum_func <- function(df,grp_var,var){
  df %>% 
    group_by({{grp_var}}) %>% 
    summarise(across(
      .cols = {{var}},
      .fns = list(
  mean=  ~mean(.x,na.rm =TRUE),
  sd =  ~sd(.x,na.rm =TRUE),
        median = ~median(.x,na.rm =TRUE),
     n=  ~n(),
     pct95=  ~quantile(.x,c(0.95))
      ),
      .names = "{col}_{fn}"
    )
      
    )
}

sum_func(bikes,member_casual,duration)
sum_func(bikes,rideable_type,duration)

bikes %>% 
  ggplot(aes(x= rideable_type,y = duration,fill = rideable_type))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(~member_casual)


##distribution of duration by bike and member type
bikes %>% 
  ggplot(aes(sample= duration)) +
  geom_qq()+
  geom_qq_line(color = "navyblue")+
  facet_grid(member_casual ~ rideable_type)+
  theme_bw()

##
ttest_result <- bikes %>% 
  t.test(duration ~ member_casual, data =.)
ttest_result  
tidy(ttest_result)

t.test(duration ~ member_casual, data =bikes,var.equal =FALSE)

## by bike type

bikes %>% 
  aov(duration ~ rideable_type,data =.) %>%
  summary()
  

anova1 <- bikes %>% 
  aov(duration ~ rideable_type,data =.) %>%   
  TukeyHSD() %>% 
  tidy()


bikes %>% 
  aov(duration ~ rideable_type,data =.) %>%
  TukeyHSD() %>% 
  plot()





anova1 %>% 
  ggplot(aes(x=contrast,y= estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax = conf.high),width = 0.3,linewidth = 0.8,color = "navyblue")+
  geom_hline(yintercept = 0,linetype = 2 ,linewidth = 0.3)+
  coord_flip()+
  theme_bw()+
  labs(x=NULL,y = NULL)
  
  
  
  
  
  
  
  
  












