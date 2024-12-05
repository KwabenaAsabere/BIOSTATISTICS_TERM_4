# Basic operations on dataframes
# accessing elements, creating new columns, subsetting, and summarizing columns
# the non-tidy verse and tidyverse approaches

# load  tidyverse
library(tidyverse)

# set working directory and import the .csv data into R and store as a data frame object

# Set working directory
# setwd("your working directory")

# example, setting my working directory: setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio 624 2023/Videos/R/Dates with R/data")

# what files are the working directory?
dir()

bike_chicago<-read_csv("202301-divvy-tripdata.csv")
str(bike_chicago)
head(bike_chicago)

dim(bike_chicago)

## Some common operations via both base R and tidyverse

  # accessing specific columns either by name or number
  names(bike_chicago)
  
    # listing specific columns
  
       # ride_id (column 1) and ended_at (column 4)
     
          # old school, non tidy-verse approach
           bike_chicago$ride_id #or#
           bike_chicago[,1]
           
           bike_chicago[1:5,1:2]
           
           bike_chicago$ended_at
           bike_chicago[,4]
           1:4
           bike_chicago[,c(1,4)]

               # just fyi
               class(bike_chicago)
               
               temp<-as.data.frame(bike_chicago)
               temp$ride_id
               temp[,1]
               bike_chicago[,1]
               temp[1:5,1]
              
         
          # tidyverse approach
          bike_chicago%>%select(ride_id)
          bike_chicago%>%select(ride_id,ended_at)
      
    # counting unique values in a column
       dim(bike_chicago)
       dim( bike_chicago[,1])
      
       
       # old school base R, non tidy-verse approach
       length(unique( bike_chicago$ride_id))
       length(unique( bike_chicago$start_station_id))
       
       # tidyverse
       
        # could assign result to another tibble. or...
        temp<- bike_chicago%>%group_by(start_station_id)%>%filter( row_number()==1)
        dim(temp)
        temp<- bike_chicago%>%group_by(start_station_id)%>%filter( row_number()==1)%>%select(start_station_id)
        dim(temp)
        rm(temp)
        
        # can ascertain directly
        dim(bike_chicago%>%group_by(start_station_id)%>%filter( row_number()==1)%>%select(start_station_id))
        
    
    # creating new columns (i.e. new variables)
       
        # old school base R, non tidy-verse approach
       names(bike_chicago)
       
       bike_chicago$endtime<-mdy_hm(bike_chicago$ended_at)
       class(bike_chicago$endtime)
       bike_chicago$duration<-difftime(mdy_hm(bike_chicago$ended_at),mdy_hm(bike_chicago$started_at),units=c("mins"))
       class(bike_chicago$duration)
      
        # tidyverse: mutate command to create and change column values
       bike_chicago<-bike_chicago%>%mutate(endtime=mdy_hm(ended_at),duration=difftime(mdy_hm(ended_at),mdy_hm(bike_chicago$started_at),units=c("mins")))
       
    # apply functions to columns in a dataframe
       
       # old school base R, non tidy-verse approach

       summary(as.numeric(bike_chicago$duration))
       table(bike_chicago$rideable_type)
       
       # tidyverse
       bike_chicago%>%summarise(meand = mean(duration, na.rm=T), mediand=median(duration, na.rm=T), mind= min(duration, na.rm=T))
       
       bike_chicago%>%group_by(rideable_type)%>%summarise(count=n())
       bike_chicago%>%group_by(rideable_type)%>%summarise(meand = mean(duration, na.rm=T), mediand=median(duration, na.rm=T), mind= min(duration, na.rm=T))
    
    # subsetting columns: suppose I want a dataframe/tibble containing only some columns from bike_chicago
       
       # old school base R, non tidy-verse approach
        bike_col_subset<-bike_chicago[,c("ride_id", "started_at", "ended_at")]
       
      # tidyverse: select commmand
        bike_col_subset2<-bike_chicago%>%select("ride_id", "started_at", "ended_at")
        
    # subsetting observations based on conditions
      # all rides between 1 and 120 minutes on bikes of "classic_bike" type

       # old school base R, non tidy-verse approach: subset command
        bike_sub1<-subset(bike_chicago, duration>0 & duration <=120 & rideable_type=="classic_bike")
        dim(bike_sub1)
        
      # tidy verse: filter command
        bike_sub2<-bike_chicago%>%filter(duration>0 & duration <=120 & rideable_type=="classic_bike")
        dim(bike_sub2)
    

