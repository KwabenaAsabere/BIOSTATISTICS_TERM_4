## Script for Lecture 3:  more on organizing and summarizing data with dplyr utilities

# add tidyverse and ggplot2 
to session library
library(tidyverse)
library(ggplot2)

# set working directory
setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 3/data/Excel")


  # read in lyme disease cases counts in "wide" format
  lymew<-read.csv("lyme disease counts by year and state wide.csv", header=T)
  names(lymew)
  head(lymew)
  lymew
  

  # for certain analyses, it may be helpful to have these data in this format, but for the 
  # tidyverse world, if we want to sumarize and graph these results by groups (states, years)
  # it may be helpful to have the in what's called "long format"

  # read in lyme disease cases counts in "long" format
  lymel<-read.csv("lyme disease by year and state long.csv", header=T)
  names(lymel)
  head(lymel)
  
      #(FYI: here is some code that shows how to convert from wide to long. i.e. how to convert the file "lymew" to "lymel")
      lymel2<-lymew%>%gather(year, cases, y2005:y2014)
      
      head(lymel2)
      lymel2<-lymel2[order(lymel2$state),]
      head(lymel2)

      # to pull the "y" off the year number
      lymel2$year<-gsub("y","",lymel2$year)
      
      head(lymel2)

  # now to some summarization
  
  # yearly means across all years, sd etc..
  lymel%>%
   group_by(year)%>%
  summarise(mean=mean(cases, na.rm=T), sd=sd(cases,na.rm=T), median=median(cases,na.rm=T), max= max(cases, na.rm=T),n=n())
  
  # state means across all states
  lymel%>%
    group_by(state)%>%
    summarise(mean=mean(cases, na.rm=T), sd=sd(cases,na.rm=T), median=median(cases,na.rm=T), max= max(cases, na.rm=T),n=n())
  
  lyme1_tibble<-lymel%>%
    group_by(state)%>%
    summarise(mean=mean(cases, na.rm=T), sd=sd(cases,na.rm=T), median=median(cases,na.rm=T), max= max(cases, na.rm=T),n=n())
  
  lyme1_tibble
  
  dim(lyme1_tibble) 
  
  lyme1_tibble%>%print(n=51)
  
# some "fun" graphics
    # basic, no "frills" histogram of case counts across all states, all years
     
    ggplot(data=lymel, aes(cases))+
     geom_histogram()
    
    # boxplot cases by  year
    
    # intentional mistake
    ggplot(lymel,aes(x=year, y=cases))+ 
      geom_boxplot(fill="gray")
    
     class(lymel$year)
    
    # one way to "fix" this
    ggplot(lymel,aes(x=as.factor(year), y=cases))+ 
      geom_boxplot(fill="gray")
    
    box_cases_yr <- ggplot(lymel,aes(x=as.factor(year), y=cases)) + 
      geom_boxplot(fill="gray")
    
    # boxplot cases by state
     class(lymel$state)
  
    box_cases_state <- ggplot(lymel,aes(x=state, y=cases)) + 
      geom_boxplot(fill="gray")
    
############################################################
# Heritage Health Data

    names(hhdata)
    
    hhdata%>%
      summarise(mean=mean(los_final, na.rm=T), StandDev=sd(los_final,na.rm=T), median=median(los_final,na.rm=T),
                min= min(los_final, na.rm=T),max= max(los_final, na.rm=T),p10=quantile(los_final,c(.1)),
                p90=quantile(los_final,c(.9)),n=n())
    
    #  histogram, with added  normal curve w/same mean and SD
    ggplot(data=hhdata, aes(los_final))+
     geom_histogram()
    
    
    ggplot(data=hhdata, aes(los_final))+
      geom_histogram(aes(y=..count../sum(..count..)))+
      stat_function(fun=dnorm,
                    color="red",
                    args=list(mean=mean(hhdata$los_final), 
                              sd=sd(hhdata$los_final)))
    
    # now for the "loaded version" that is close to what appears in the lecture
    ggplot(data=hhdata, aes(los_final))+
      geom_histogram(aes(y=..count../sum(..count..)),fill="gray", col="black",binwidth=1) +
      stat_function(fun=dnorm,
                    color="black", lwd=1.5,
                    args=list(mean=mean(hhdata$los_final), 
                              sd=sd(hhdata$los_final)))   +
      labs(title="Total Length of Stay (LOS), 2011",x = "LOS (Days)", y = "Percentage of Sample") +
   
      xlim(c(0,40))+
      ylim(c(0,.5))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title =element_text(face="bold", hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
      # removing top and right borders - this removes all borders, actually
      theme(panel.border = element_blank())+
      # so to then add back the axes
      theme(axis.line = element_line(color = 'black'))+
      # add subtitle in specific postion
      annotate("text", label = "Entire Sample of 12,928 Claims (with at least on inpatient visit)", x = 20, y = .5, size=4.5)
      
    