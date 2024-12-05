# Dates and Times with 4 Digit Years ############################################################################################
### Example 2 based on file "202301-divvy-tripdata.csv", has date with four-digit years, and hour and minutes as well
### data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html on 02/27/2023, 1:00 PM EST

  # Load necessary packages
  
  library(tidyverse) # general functions for working with data
  library(lubridate) # package for working with dates : this is actually part of tidyverse for does not need to be loaded independently per se
  
  
  # Set working directory
  # setwd("your working directory")
  
  ## example: setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio624_2023/Videos/R/Dates_with_R/data")
  ####### bike share data
  
  # read in data from csv
  bike_chicago<-read_csv("202301-divvy-tripdata.csv")
  str(bike_chicago)
  head(bike_chicago)
  
  # subset to include only several columns
  bike_chicago<-bike_chicago%>%
    select(ride_id,rideable_type,started_at,ended_at)
  head(bike_chicago)
  
  # create date versions of bikeshare start and end times
  bike_chicago<-bike_chicago%>%mutate(start_daytime=mdy_hm(started_at), end_daytime=mdy_hm(ended_at))
  names(bike_chicago)
  head(bike_chicago)
  head(bike_chicago[,c(3,5,4,6)])
  
  # compute duration of bikeshare 
  bike_chicago<-bike_chicago%>%mutate(duration =difftime(end_daytime,start_daytime,units=c("mins")))
  summary(bike_chicago$duration)
  head(bike_chicago)
  summary(as.numeric(bike_chicago$duration))
  
  
  # trouble shooting/investigating durations of 0, and really large values of duration to make sure these are correct
  
  # 0 minute rides check
  test<-bike_chicago%>%
    filter(duration ==0)%>%
    select(start_daytime, end_daytime, duration)
  head(test)
  dim(test)
  rm(test)
  
  
  # realy long rides check
  
  test<-bike_chicago%>%
    filter(duration >10000 )%>%
    select(start_daytime, end_daytime, duration)
  head(test)
  dim(test)
  rm(test)
  
  # get some upper quantiles of ride lengths
  
  quantile(bike_chicago$duration,c(.90,.95,.99,.995))
  
  # actually, to get real picture about non0-zer extremes, should remove all rides of length 0 first
  
  test<-bike_chicago%>%
    filter(duration >0)
  quantile(test$duration,c(.90,.95,.99,.995))
  
  # remove extremes (> 120 minutes) for analytical purposes
  
  bike_chicago2<-bike_chicago%>%filter(duration <=120 & duration >0)
  dim(bike_chicago2)
  
  
  # summarize duration on dataset that exludes rides of 0 minute length, and greater than 120 minutes in length
  summary(bike_chicago2$duration)
  summary(as.numeric(bike_chicago2$duration))
  
  # some graphical summaries and comparisosn of bike rental/ride duration values
  
  # histogram of duration times
  
  # including the  extreme values
  ggplot(data= bike_chicago, aes(duration)) +
    geom_histogram(aes(y=100*..count../sum(..count..)))+
    labs(title="Rides Duration ", subtitle  = "Divvy Share Rides, Chicago, February 2023", 
         x = "Duration (Minutes)",
         y= "Percentage of Rides")+
    theme(plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5))
  
  # after extremes removed 
  
  ggplot(data= bike_chicago2, aes(duration)) +
    geom_histogram(aes(y=100*..count../sum(..count..)))+
    labs(title="Ride Durations for Rides Less than 2-Hours (> 99% of Original Sample", subtitle  = "Divvy Bike Share Share  , Chicago, February 2023", 
         x = "Duration (Minutes)",
         y= "Percentage of Rides")+
    theme(plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5))
  
  # side-by-side boxplots of duration by day of week bike checked out
  
  bike_chicago_2<-bike_chicago%>%mutate(day=wday(start_daytime, label=TRUE))
  
  ggplot( bike_chicago_2, aes(x=day ,y=duration),alpha = 0.2) + 
    geom_boxplot()+
    labs(title="Ride Durations for Rides Less than 2-Hours (> 99% of Original Sample", subtitle= "Divvy Bike Share Share  , Chicago, February 2023", y = "Duration (mins)", x ="Type of Bike" )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
          axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 20))+
    scale_y_continuous(limits=c(0,120), breaks=seq(0,120,10))+
    theme(axis.line = element_line(color = 'black'))
  
  
  # side-by-side boxplots of duration by type of bike
  ggplot(  bike_chicago_2, aes(x=rideable_type, y=duration,fill=rideable_type,alpha = 0.2)) + 
    geom_boxplot()+
    labs(title="Ride Durations for Rides Less than 2-Hours (> 99% of Original Sample", subtitle= "Divvy Bike Share Share  , Chicago, February 2023  (n=115,609 )", y = "Duration (mins)", x ="Type of Bike" )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
          axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 20))+
    scale_y_continuous(limits=c(0,120), breaks=seq(0,120,10))+
    theme(axis.line = element_line(color = 'black'))+
    scale_fill_manual( values = c("blue","blue", "blue"))+
    theme(legend.position="none")
  
  
  
  
