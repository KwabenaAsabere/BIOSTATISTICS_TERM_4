# R script for R lecture 4

setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 4/data/Excel")

library(tidyverse)
library(ggplot2)

  # get kaggle HIV data
  hivdata<-read.csv("kaggle hiv.csv",header=T)
  class(hivdata)

  # examine data
  names(hivdata)
  head(hivdata)
  
  hivdata$resp

  # create indicator if CD4 count group, and convert response variable to a factor variable with descriptive levels

  hivdata<-hivdata %>%
     mutate(cd4cat=factor(as.numeric(cd4t0<250),labels=c("CD4 >= 250", "CD4<250")) ) %>%
     mutate( resp=factor(resp,levels=c(0,1), labels=c("Not Respond", "Respond")))
 

# 2X2 table

  # dplyr (tideyverse)  approach to get counts in each group combination
  
    hivdata%>%
     count(resp, cd4cat)%>%
      spread(key = cd4cat, value = n)
    
    (# can also use table command from "base r")
      table(hivdata$resp, hivdata$cd4cat)

  # to get proportions of hiv positive within each cd4count group
    hivdata%>%count(resp, cd4cat) %>%
      group_by(cd4cat) %>%
      mutate(prop = n / sum(n)) %>%
      select(-n) %>%
      spread(cd4cat, prop)
     
     #(can also assign the result to an object that gets saved)
      table1<-hivdata%>%count(resp, cd4cat) %>%
        group_by(cd4cat) %>%
        mutate(prop = n / sum(n)) %>%
        select(-n) %>%
        spread(cd4cat, prop)
      
      (# can also use table command from "base r")
    tab1<-table(hivdata$resp,hivdata$cd4cat)
    class(tab1)
    prop.table(tab1.2)

# Measures of Association

  # Approach 1 using the results form the table created above, "table"
  table1
  table1[2,2]
 # now for RD, RR, OR
 #RD:
  table1[2,3]-table1[2,2]

 #RR:
  table1[2,3]/table1[2,2]
 #OR:
 (table1[2,3]/(1-table1[2,3])/(table1[2,2]/(1-table1[2,2])))
   
  # Another approach, just FYI
  
  phats<- hivdata%>% 
    group_by(cd4cat) %>% 
    summarise(phat= mean(resp, na.rm=TRUE))
  
  class(hivdata$resp)
  table(hivdata$resp)
  table(as.numeric(hivdata$resp))
  
  phats<- hivdata%>% 
    group_by(cd4cat) %>% 
    summarise(phat= mean(as.numeric(resp)-1, na.rm=TRUE))
  phats
  
  # now for RD, RR, OR
  #RD:
  phats[2,2]-phats[1,2]
  #RR:
  phats[2,2]/phats[1,2]
  #OR:
  (phats[2,2]/(1-phats[2,2])/(phats[1,2]/(1-phats[1,2])))