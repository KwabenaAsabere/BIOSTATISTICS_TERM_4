# R lecture 6 part 1
# Random samples, simulated sampling distribution of sample means in R, and associated graphics
# a lesson in loops
# loops in R

# add tidyverse to session library
library(tidyverse)

# setworking directory to where data is stored
setwd("~/Dropbox/JMCGREADY/PH Statistics 1 2019/lecture slides/lecture set 6/data/Excel")

# read in HH dataset
hh<-read.csv("HH for analysis working.csv")
names(hh)

# for simulation, filter out missing values

hh<-hh%>%filter(!is.na(los_final))

# set seed for reproducibility of sample
set.seed(1310496317)

# random sample n= 50 and histogram 
samp1<-sample(hh$los_final,50)
mean(samp1)
sd(samp1)

# hitogram of this first random sample of 50 records
  
  sdf<-as.data.frame(samp1)

  ggplot(sdf, aes(samp1)) +
    geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=1, col="black", fill="gray") +
    labs(title="Length of Stay (days) \n Random Sample of 50 Observations, Heritage Health 2011",x = "LOS (days)", y = "Percentage of Sample")+
    scale_y_continuous(limits =c(0,30),breaks=seq(0,30,by=3))+
    scale_x_continuous(limits =c(0,50),breaks=seq(0,50,by=5))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
  

  # using a for loop in R
  # compute means from 5,000 random sample of sizes 50, 250 and 400
  
  # samples of size n =50
  means50<-rep.int(0, 5000)
  sd50<-rep.int(0, 5000)
  ss50<-data.frame(means50,sd50)
  
  for (i in 1:5000) {
    s<-sample(hh$los_final,50)
    ss50$means50[i]<-mean(s)
    ss50$sd50[i]<-sd(s)
  }
  # histogram of the 5,000 sample means from 5,000 random samples of n=50
  # x and y scales chosen purposely so can use same scaling for estimated
  # sampling dist's of sample means for other sample sizes
  
  ggplot( ss50, aes(means50)) +
    geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=.2, col="black", fill="gray") +
  labs(title="Distribution of 5,000 Sample LOS Values \nFrom 5,000 Samples, Each Sample n= 50",x = "LOS (days)", y = "Percentage of Sample Means",
       caption ="all samples taken from same Heritage Health Dataset")  +
    scale_y_continuous(limits =c(0,35),breaks=seq(0,35,by=5))+
    scale_x_continuous(limits =c(2,8),breaks=seq(2,8,by=1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
  

### now to estimate sampling distribtuion of means from samples of n=250

  means250<-rep.int(0, 5000)
  sd250<-rep.int(0, 5000)
  ss250<-data.frame(means250,sd250)
  
  for (i in 1:5000) {
    s<-sample(hh$los_final,250)
    ss250$means250[i]<-mean(s)
    ss250$sd250[i]<-sd(s)
  }
  summary(ss50$means50)
  summary(ss250$means250)
  
  # histogram of the 5,000 sample means from 5,000 random samples of n=250
  # x and y scales chosen purposely so can use same scaling for estimated
  # sampling dist's of sample means for other sample sizes
  
  ggplot( ss250, aes(means250)) +
    geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=.2, col="black", fill="gray") +
    labs(title="Distribution of 5,000 Sample LOS Values \nFrom 5,000 Samples, Each Sample n= 250",x = "LOS (days)", y = "Percentage of Sample Means",
         caption ="all samples taken from same Heritage Health Dataset")  +
    scale_y_continuous(limits =c(0,35),breaks=seq(0,45,by=5))+
    scale_x_continuous(limits =c(2,8),breaks=seq(2,8,by=1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
  
  
 ### now to estimate sampling distribtuion of means from samples of n=250
    means400<-rep.int(0, 5000)
    sd400<-rep.int(0, 5000)
    ss400<-data.frame(means400,sd400)
    
    for (i in 1:5000) {
      s<-sample(hh$los_final,400)
      ss400$means400[i]<-mean(s)
      ss400$sd400[i]<-sd(s)
    }
    
    # histogram of the 5,000 sample means from 5,000 random samples of n=250
    # x and y scales chosen purposely so can use same scaling for estimated
    # sampling dist's of sample means for other sample sizes
    
    ggplot( ss400, aes(means400)) +
      geom_histogram(aes(y=100*(..count../sum(..count..))),binwidth=.2, col="black", fill="gray") +
      labs(title="Distribution of 5,000 Sample LOS Values \nFrom 5,000 Samples, Each Sample n= 400",x = "LOS (days)", y = "Percentage of Sample Means",
           caption ="all samples taken from same Heritage Health Dataset")  +
      scale_y_continuous(limits =c(0,35),breaks=seq(0,45,by=5))+
      scale_x_continuous(limits =c(2,8),breaks=seq(2,8,by=1))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
    
    

# side by side boxplots of the 3 estimated sampling distributions for means based on samples of
# n=50, n=250, n=400
    
  bplot<-data.frame(ss50$means50,ss250$means250,ss400$means400)
  colnames(bplot)<-c("m50","m250","m400")

  # easier to do side=by-sides if we reshape
  bplot_long<-bplot%>% gather(ss, mean,c(m50,m250,m400))
  bplot_long<-bplot_long%>%mutate(ss=gsub("m","",ss))
  
  # side by side boxies
  ggplot(bplot_long, aes(x=ss,y=mean)) + 
    geom_boxplot(fill="gray")+
    labs(title="Estimated Sampling Dist'n,Sample Mean LOS Values \n5,000 Random Samples of n=50, n=250", y ="Length of Stay Mean (Days)", x="Sample Size")  +
    scale_y_continuous(limits =c(2,8),breaks=seq(2,8,by=1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
  
  
  bplot_long$ss <- factor(bplot_long$ss, levels =c("50","250","400"))
  
