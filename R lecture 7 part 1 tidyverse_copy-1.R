# R lecture 7
# Confidence Intervals for single population parameters using R

library(tidyverse)
# R lecture 7 part 1
# Means

  # Blood pressure example: sample of 113 men
  # xbar = 123.6, s=12.9, n=113
  
   # using R as a calculator
   
   # approach 1
    123.6 - 2*(12.9/sqrt(113))  # lower limit
    123.6 + 2*(12.9/sqrt(113))  # upper limit
  
   # approach 2
    123.6 - qt(.975,113-1)*(12.9/sqrt(113))  # lower limit
    123.6 + qt(.975,113-1)*(12.9/sqrt(113))  # upper limit
  
    
  # Heritage Health LOS data
    # working directory for importing data
    setwd("~/Dropbox/JMCGREADY/PH Statistics 1, 2019/New R Videos/lecture 6/data")
    hhdata<- read.csv("HH for analysis working.csv", header=T)
    names(hhdata)
    
    # 95% CI for LOS
    mean(hhdata$los_final)
    mean(hhdata$los_final,na.rm=T)-qt(.975,length(hhdata$los_final)-1)*(sd(hhdata$los_final)/sqrt(length(hhdata$los_final))) # lower limit
    mean(hhdata$los_final)+qt(.975,length(hhdata$los_final)-1)*(sd(hhdata$los_final)/sqrt(length(hhdata$los_final))) # lower limit
  
    # We could write a function to do this! (for situations where we have the data in R)
    
      mean95ci<-function(x){
    
      lower<- mean(x,na.rm=T) - qt(.975,length(x)-1)*(sd(x,na.rm=T)/sqrt( length(x)))
      upper<- mean(x) + qt(.975,length(x)-1)*(sd(x)/sqrt( length(x)))
      result<- c(mean(x), lower, upper)
      return(result)
      }
      
      # now evaluate this with the hhdata length of stay data
      mean95ci(hhdata$los_final)
      
##################################
#  Proportions
      
   # mother/infant transmission example, AZT group: 13 transmissions, 180 children
      phatazt<-13/180
      
      # 95% CI
      phatazt-1.96*sqrt( phatazt*(1- phatazt)/180) # lower limit
      phatazt+1.96*sqrt( phatazt*(1- phatazt)/180) # upperlimit
      
      # function for when we only have summary stats
      
      prop95ci1<-function(x,n){
        phat<-x/n
        lower<-phat-1.96*sqrt(phat*(1-phat)/n)
        upper<-phat+1.96*sqrt(phat*(1-phat)/n)
        result<- c(phat, lower, upper)
        return(result)
      }
    
      prop95ci1(13,180)
   
    # proportion of responders, citywide population HIV+, sample of n=1000
     setwd("~/Dropbox/JMCGREADY PH Statistics 2019/lecture slides/lecture set 4/data/Excel")
    
     kd<-read.csv("kaggle hiv.csv")
     names(kd)     table(kd$resp)
     mean(kd$resp)
     length(kd$resp)

     
     prop95ci1( sum(kd$resp),length(kd$resp))
     

##########################
# Incidence Rates
     
# see if you can do on your own :-)!

     
