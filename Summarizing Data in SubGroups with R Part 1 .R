########
# Grouping with R Part 1   
# load libraries

library(tidyverse)

# set working directory

setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio 624 2023/Videos/R/Common Operations in R/data")

# import csv and save to tibble
bikes<-read_csv("bikes_chicago_sample.csv")
names(bikes)
class(bikes$duration)

# Summary Statistics

# overall duration of rides

bikes%>%summarise(meandur= mean(duration, na.rm=T))

bikes%>%summarise(meandur= mean(duration, na.rm=T), sddur=sd(duration,na.rm=T))
bikes%>%summarise(meandur= mean(duration, na.rm=T), sddur=sd(duration,na.rm=T),n=n() , mediandur=median(duration,na.rm=T) ,
                  pct95dur=quantile(duration,c(.95)))

# by casual rider vs rideshare member status

# first, look at distribtution of member_casual

# Base R
table(bikes$member_casual) 
prop.table(  table(bikes$member_casual))

# Tidyverse
bikes%>%group_by(member_casual)%>%summarise(n = n())%>%mutate(pct = n/sum(n))

# why does this work?
# in pieces
temp1<- bikes%>%group_by(member_casual)%>%summarise(n = n())
head(temp1)
temp2<-temp1%>%mutate(pct=n/sum(n))
head(temp2)

# summary stats by member status, and p-value from t-test

# tidyverse
bikes%>%group_by(member_casual)%>%summarise(meandur= mean(duration, na.trm=T), sddur=sd(duration,na.rm=T),n=n() , mediandur=median(duration,na.rm=T) ,
                                            pct95dur=quantile(duration,c(.95)))

# t.test from base R
t.test(duration~member_casual, data=bikes)
t.test(duration~member_casual, data=bikes,var.equal = FALSE)

# by bike type

table(bikes$rideable_type) 
prop.table(  table(bikes$rideable_type))
bikes%>%group_by(rideable_type)%>%summarise(n = n())%>%mutate(pct = n/sum(n))

# summary stats by ride type, and p-value from ANOVA
bikes%>%group_by(rideable_type)%>%summarise(meandur= mean(duration, na.trm=T), sddur=sd(duration,na.rm=T),n=n() , mediandur=median(duration,na.rm=T) ,
                                            pct95dur=quantile(duration,c(.95)))
summary(aov(duration~rideable_type, data=bikes))
anova1<-aov(duration~rideable_type, data=bikes)

