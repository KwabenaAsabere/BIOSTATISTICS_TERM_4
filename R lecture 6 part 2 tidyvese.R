# R lecture 6 part 2
# Saving .csv and .Rda files, and saving and entire R session

library(tidyverse)

# first setworking directory to where data is stored
setwd("~/Dropbox/JMCGREADY/PH Statistics 1, 2019/New R Videos/lecture 6/data")

# read in HH dataset
hhdata<-read.csv("HH for analysis working.csv", header=T)
names(hhdata)

 # create age indicator as factor variable
  
  hhdata<-hhdata%>%mutate(agecat= factor(age>40, labels = c("<= 40 years", " > 40 years")))

# now that I've altered the original data, I may want to save it again
  
  # as a .csv file
  write.csv(hhdata,"HH working data.csv")
  
  # as a .Rda file
   saveRDS(hhdata,"HH working data.Rda")
   
   # to read back in to R session
   hhdata2<-readRDS("HH working data.Rda")

# now suppose you wish to do other things in this session as well
   library(SpatialEpi)
   # load data
   data(pennLC)
   names(pennLC)
   # created data frame of pennLC lung cancer counts data
   plc<-pennLC$data
   
   # to get total cases and total population counts aggregated males and females
   
   irssex<-plc%>%
     group_by(gender)%>%
     summarise(ir=sum(cases)/sum(population))
   
  # now you want to take a break, but need to shutdown your computer
  # you want to return to this session without having to rerun all of the "start - up"
  # code.  You can save the entire session to be rejoined later in R.
  
  save.image("friday april 19.Rdata")

  #### to reload saved session
  
  
  setwd("~/Dropbox/JMCGREADY/PH Statistics 1, 2019/New R Videos/lecture 6/data")
  load("friday april 19.Rdata")
