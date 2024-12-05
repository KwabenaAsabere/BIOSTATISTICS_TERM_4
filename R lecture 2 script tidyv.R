# working directory for importing data

setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 2/data/Excel")

# R video lecture 2, part 1

# blood pressure, 113 men

# read in the blood pressues values and assign these data to a dataframe names "bp113m"

bp113m<-read.csv("bp113men.csv")
class(bp113m)
names(bp113m)
head(bp113m)


# add gglplot and the tidyverse to session library

library(ggplot2)
library(tidyverse)

#############################
# HISTOGRAMS, SBP

  # basic histogram of the 113 SBP measurments

 ggplot(data=bp113m, aes(sbp)) +
  geom_histogram()

  ########
  # histogram with frequencies on y-axis as decimals
 ggplot(bp113m, aes(sbp)) + 
  geom_histogram(aes(y=(..count../sum(..count..))))

  # histogram with frequencies on y-axis as proportions
  ggplot(bp113m, aes(sbp)) + 
    geom_histogram(aes(y=100*(..count../sum(..count..))))
 
 # changing bin width  ex 1
  ggplot(bp113m, aes(sbp)) + 
    geom_histogram(aes(y=..count../sum(..count..)),binwidth=10)
 
 # changing bin width  ex 2
 ggplot(bp113m, aes(sbp)) + geom_histogram(aes(y=..count../sum(..count..)),binwidth=1)
 
 # changing bin breaks ex 1
 ggplot(bp113m, aes(sbp)) + 
   geom_histogram(aes(y=..count../sum(..count..)),breaks=c(80,100,120,140,160))
 
 # changing breaks example 2
 
 ggplot(bp113m, aes(sbp)) + 
   geom_histogram(aes(y=..count../sum(..count..)),breaks=seq(80,160,by=2))
 
 # adding titles and labels
 ggplot(bp113m, aes(sbp)) +
   geom_histogram(aes(y=..count../sum(..count..)),binwidth=2) +
   labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Proportion of Men")
   
 # adding titles and labels, and specifying axis characteristics
 ggplot(bp113m, aes(sbp)) + geom_histogram(aes(y=..count../sum(..count..)),binwidth=2) +
   labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Proportion of Men",caption = "Random Sample of 113 Men")+
   ylim(c(0,.10))+
   xlim(80,160)
 
 # now for the final version in the lecture notes
 ggplot(bp113m, aes(sbp)) +
   geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=5, col="black", fill="gray") +
   labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Percentage of Men", caption = "Random Sample of 113 Men")+
   ylim(c(0,20))+
   xlim(c(80,160))+
   theme_bw()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
 
dev.off()
# can store as a named object in R

hist_sbp<-  ggplot(bp113m, aes(sbp)) +
  geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=5, col="black", fill="gray") +
  labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Percentage of Men", caption = "Random Sample of 113 Men")

  hist_sbp

dev.off()

hist_sbp +
  ylim(c(0,20))+
  xlim(c(80,160))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))

  # (now to save the graphic as a .png file)
  # change directory for graphic saving

  setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 2/new slides OPAL/graphs/")
  
  # set up the .png file for the graph to be stored
  
  png("histsbp_1.png")
  
  ggplot(bp113m, aes(sbp)) +
    geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=5, col="black", fill="gray") +
    labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Percentage of Men", caption = "Random Sample of 113 Men")+
    ylim(c(0,20))+
    xlim(c(80,160))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
  

  # close the graph to save the .png file
  dev.off()
  
################################################

# R Lecture 2 Video, Part 2
# Boxplots, SBP
  
  # histogram with frequencies on y-axis as decimals
  ggplot(data=bp113m, aes(x=sbp)) + 
    geom_histogram(aes(y=(..count../sum(..count..))))
  
  # "no frills" boxplot
  ggplot(bp113m, aes(x=1,y=sbp)) + 
    geom_boxplot()
    
  # more detailed boxplot
  ggplot(bp113m, aes(x=1,y=sbp)) + 
    geom_boxplot()+
    labs(title="Systolic Blood Pressure (SBP) Measurements", y = "SBP (mmHg)", caption = "Random Sample of 113 Men", x="")+
    ylim(c(80,160))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5),
          axis.text.x=element_blank(),axis.ticks.x=element_blank())
  

### Summary Statistics SBP: some "base R functions to start
  sd(bp113m$sbp)
  length(bp113m$sbp)

  ## more customizability with dplyr approach(es)
  summarise(bp113m, mean=mean(sbp), stand_dev=sd(sbp), median=median(sbp), max= max(sbp), count= n(),p25=quantile(sbp,c(.25)))
  
     # alternate approach, "pipe-based" syntax
  bp113m%>%summarise( mean=mean(sbp), stdv=sd(sbp), median=median(sbp), max= max(sbp), n= n())

######################################################################################################################################
  #####################################################################################################################################
  
  
# R lecture 2 video  part 3
# histograms and boxplots split out for subgroups
  
# random sample of n=236 children 12 months old

  # working directory for importing data
  setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 2/data/Excel")

  nepal12mos<- read.csv("nepali anthro at 12 months.csv", header=T)
  names(nepal12mos)
  summary(nepal12mos$weight)
  length(nepal12mos$weight)
  
  # summarizing weight using pipe grammar
  nepal12mos%>%
    summarise(mean=mean(weight,na.rm=T), sd=sd(weight,na.rm=T), median=median(weight,na.rm=T), max= max(weight, na.rm=T),n=n())
  
  # getting summary statistics of weight by sex 
  wbysex<-nepal12mos%>%
    group_by(sex)%>%
    summarise(mean=mean(weight, na.rm=T), sd=sd(weight,na.rm=T), median=median(weight,na.rm=T), max= max(weight, na.rm=T),n=n())
  
  # assign the sex labels correspnding labels do they can be identified
  nepal12mos$sex
  
  nepal12mos$sex<-factor(nepal12mos$sex, levels =c(1,2),c("Male", "Female"))
  table(nepal12mos$sex)
  
# histograms of weight by sex

  ggplot(data=nepal12mos, aes(weight, fill=sex)) + 
    geom_histogram(aes(y=100*(..count../sum(..count..))))
  
  ggplot(data=nepal12mos, aes(weight, col=sex)) + 
    geom_histogram(aes(y=..count../sum(..count..)) , fill="white") 
  
  # let's add vertical lines for the mean height of each sex
  # first, need to compute mean for each sex and  store in a data frame
  
   wbar<- nepal12mos%>% 
    group_by(sex) %>% 
    summarise(meanw= mean(weight, na.rm=TRUE))

   wbar
  
  ggplot(data=nepal12mos, aes(weight, col=sex)) + 
    geom_histogram(aes(y=..count../sum(..count..)) , fill="white")+
    labs(title="Systolic Blood Pressure (SBP) Measurements",x = "SBP (mmHg)", y = "Proportion of Men", caption = "Random Sample of 113 Men")+
    geom_vline(data=wbar, aes( xintercept=meanw, color=sex),
             linetype="dashed")
    
    # add tiles and axes labels 
    p<-ggplot(data=nepal12mos, aes(weight, col=sex)) + 
      geom_histogram(aes(y=..count../sum(..count..)) , fill="white")+
      labs(title="Weight (kg) by Sex, 236 Children at 12 Months",x = "Weight (kg)", y = "Percentage of Sample")
     geom_vline(data=wbar, aes( xintercept=meanw, color=sex),
               linetype="dashed")
                            
     dev.off()
     p
     
     p+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
     
                                                 
# now do the stacked histograms (which is more faithful to the histograms shown in the lecture notes!)
     
     install.packages("gridExtra")
     library(gridExtra)
     
     # remember wbar?
     
     wbar
     
     # to isolate number value of mean for females

     wbar[2,2]
     
     nf<-nepal12mos%>%filter(as.integer(sex)==2)
     
     pf<-ggplot(data=nf, aes(weight)) + geom_histogram(aes(y=100*..count../sum(..count..)),breaks=seq(0,12,by=1), col="black", fill="gray") +
       labs(title="Weight (kg) by Sex, 236 Children at 12 Months\n Females",x = "Weight (kg)", y = "Percentage of Sample")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
        ylim(c(0,40))+
       scale_x_continuous(breaks=seq(0,12,2))+
       geom_vline(data=wbar, aes( xintercept=meanw[2]),
                  linetype="solid", lwd=2)

     nm<-nepal12mos%>%filter(as.integer(sex)==1)
     pm<-ggplot(data=nm, aes(weight)) + geom_histogram(aes(y=100*..count../sum(..count..)),breaks=seq(0,12,by=1), col="black", fill="gray") +
       labs(title="Males",x = "Weight (kg)", y = "Percentage of Sample")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
       scale_x_continuous(breaks=seq(0,12,2))+
       ylim(c(0,40))+
       geom_vline(data=wbar, aes( xintercept=meanw[1]),
                  linetype="solid", lwd=2)
     
     # now to put these two graphs on the same "canvas", stacked on top of each other
     grid.arrange(pf, pm, nrow = 2)

# to go the extra mile to make these identical to those in lecture
     nf<-nepal12mos%>%filter(as.integer(sex)==2)
     pf<-ggplot(data=nf, aes(weight)) + geom_histogram(aes(y=100*..count../sum(..count..)),breaks=seq(0,12,by=1), col="black", fill="gray") +
       labs(title="Weight (kg) by Sex, 236 Children at 12 Months\n Females",x = "Weight (kg)", y = "Percentage of Sample")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
       scale_x_continuous(breaks=seq(0,12,2))+
       # be more speific about where vertical line for mean weight starts and end
       geom_segment(aes( x = meanw[2], y = 0, xend = meanw[2], yend = 40),data=wbar, linetype="solid", lwd=2)+
       # remove space between histogram and x-axis
       scale_y_continuous(expand = c(0,0),breaks=seq(0,45,10))+
       # removing top and right borders - this removes all borders, actually
       theme(panel.border = element_blank())+
       # so to then add back the axes
       theme(axis.line = element_line(color = 'black'))
      
     
     nm<-nepal12mos%>%filter(as.integer(sex)==1)
     pm<-ggplot(data=nm, aes(weight)) + geom_histogram(aes(y=100*..count../sum(..count..)),breaks=seq(0,12,by=1), col="black", fill="gray") +
       labs(title="Males",x = "Weight (kg)", y = "Percentage of Sample")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
       scale_x_continuous(breaks=seq(0,12,2))+
       # be more specific about where vertical line for mean weight starts and end
       geom_segment(aes( x = meanw[2], y = 0, xend = meanw[2], yend = 40),data=wbar, linetype="solid", lwd=2)+
       # remove space between histogram and x-axis
       scale_y_continuous(expand = c(0,0),breaks=seq(0,45,10))+
       # removing top and right borders - this removes all borders, actually
       theme(panel.border = element_blank())+
       # so to then add back the axes
       theme(axis.line = element_line(color = 'black'))
     
     # now to put these two graphs on the same "canvas", stacked on top of each other
     grid.arrange(pf, pm, nrow = 2)
     

#####################
# boxplots, by sex

  # basic syntax
    box_wt_sex <- ggplot(nepal12mos, aes(x=sex, y=weight)) + 
       geom_boxplot()
    
     # to make it a little more visually pleasing, at least to me!
     
     box_wt_sex <- ggplot(nepal12mos, aes(x=sex, y=weight)) + 
       geom_boxplot(fill="gray")

  # now with extras, taken directly from the command for histograms from before
     
       # code from before for reference
        p<-ggplot(data=nepal12mos, aes(weight, col=sex)) + 
        geom_histogram(aes(y=..count../sum(..count..)) , fill="white")+
        labs(title="Weight (kg) by Sex, 236 Children at 12 Months",x = "Weight (kg)", y = "Percentage of Sample")
          geom_vline(data=wbar, aes( xintercept=meanw, color=sex),linetype="dashed")+
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
     
  # add the "extras" to the object box_wt_sex

      box_wt_sex+
      labs(title="Weight (kg) by Sex, 236 Children at 12 Months", y = "Weight (kg)", x = "Sex of Child")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))+
        theme(axis.line = element_line(color = 'black'))
          


