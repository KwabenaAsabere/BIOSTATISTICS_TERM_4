# Handling Dates with R, Part 1

#### Dates with 4 Digit Years
### Example 1 based on file "R _data_example1.csv", 4 digit years (i.e. "2008", "1999")
  
      # Load necessary packages
      
      library(tidyverse) # general functions for working with data
      library(lubridate) # package for working with dates : this is actually part of tidyverse for does not need to be loaded independently per se
    
  
      # Set working directory
      # setwd("your working directory")
  
      ## example: setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio624_2023/Videos/R/Dates_with_R/data")
      
      # read in data from .csv, and get overview of dataset
      
      date_example <- read_csv("R _data_example1.csv")
      str(date_example)
      head(date_example)
      
      # create date versions of string dates using "mdy" function since this is the format these dates are in
      
      date_example <- date_example %>%
      mutate(vdate = mdy(visitdate),
             bdate = mdy(birthdate))
      
      # assess characteristics of vdate and bdate
      str(date_example)
      
      head(date_example)
      date_example[1:10,]
      
      # what is the class of the new variables?
      class(date_example$vdate)
      class(date_example$bdate)
      
      # use the difftime function to get the time elapse between the visit date and birth date: default computation is reported in days (can also directly subtract
      # date class variables) 
      # create version of age in years by taking difference in days and dividing by 365.24
      
      date_example <- date_example %>%
      mutate( age_days = difftime(vdate, bdate),
              age_days2 = vdate - bdate,
            age_years =age_days/365.25)
      
      
      str(date_example)
      date_example[1:10,]
          
          # just FYI: while taking direct difference between date measure will also yield a result in days, the untis of the result from "difftime" can be changed from default of days
          date_example <- date_example %>%
            mutate(age_weeks = difftime(vdate, bdate,units=c("weeks")))
          str(date_example)
          date_example[1:10,]
  
      # let's look at age years
      summary(date_example$age_years)
      summary(as.numeric(date_example$age_years))
      
      # what is the dituations with birth and/or visit dates for those with a missing age value?
      
      temp<-date_example%>%filter(is.na(age_years))
      head(temp)
  
##