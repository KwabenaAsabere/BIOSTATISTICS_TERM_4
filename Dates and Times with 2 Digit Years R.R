# Two digit dates example R

# Load necessary packages

library(tidyverse) # general functions for working with data
library(lubridate) # package for working with dates : this is actually part of tidyverse for does not need to be loaded independently per se


# Set working directory
# setwd("your working directory")

## example: setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio624_2023/Videos/R/Dates_with_R/data")

# read in data and store in dataframe/tibble
  dates<-read_csv("R_two_digit_year_example.csv")
  
  str(dates)
  head(dates)
  
  dates <- dates %>%
    mutate(vdate = mdy(visit),
           bdate = mdy(birth))
  
  str(dates)
  dates[1:10,]
  
  table(year(dates$bdate))

  # how to correct?
  # let's assume that all dates with two-digit year of 2021 or greater were in the 1900s
  # for years > 2022, replace years with original value - 100
    
  dates <- dates %>%
    mutate(bdate2 = if_else(year(bdate)> 2020, make_date(year=year(bdate)-100,month =month(bdate), day=day(bdate)),bdate))
    
  dates[1:10,]
  table(year(dates$bdate))
  table(year(dates$bdate2))
  
  
  # what about dates/times?
  dates<-dates%>%mutate(vdatet = mdy_hm(vdate_time))
  table(year(dates$vdatet))
  
  
  
  
  