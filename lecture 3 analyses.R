# Class 3 Script


# Getting Started
# load packages needed for managment and analysis

library(tidyverse)
library(expss)  


setwd("you preferred path")

# picking up with dataset saved after Class Session 2 
load("data/wmclass2.RData")

# set working directory to main directory this session (mine is /...../Class_3")

# Effect of Exposure to Visual Campaigns and Narrative Vignettes data from Class 2 (and a sub set of variables was used in the first part of Assignment 1)
  # data, boxlots of age at enrollment  by binarized exposure groups
    names(wm)
    boxage_exp<- ggplot(wm, aes(x=exposed, y=ppage)) + 
      geom_boxplot()
    boxage_exp
  
  # more detailed boxplot
  ggplot(wm, aes(x=exposed, y=ppage,fill=exposed,alpha = 0.2)) + 
    geom_boxplot()+
  labs(title="Age Distributions by Exposure Status", y = "Age (Years)", subtitle= "Effect of Exposure to Visual Campaigns and Narrative Vignettes", caption
       ="Subjects randomized to all 8 campaigns grouped into  Exposed;  Subjects randomized to control arm grouped into Unexposed", x = "")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
          axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 20))+
    scale_x_discrete(labels=c("Unexposed (n=210)" , "Exposed (n=1,641)"))+
    scale_y_continuous(limits=c(20,100), breaks=seq(20,100,10))+
    theme(axis.line = element_line(color = 'black'))+
    scale_fill_manual( values = c("blue","blue"))+
    theme(legend.position="none")

   # basic, overlaid histograms of age distributions by exposure
  
    ggplot(data=wm , aes(ppage,col=exposed)) +
    geom_histogram(aes(y = stat(count) / sum(count)))
                 
## stigma scores data
    phq<-read_csv("data/UAS_august_PHQ_agecat.csv")
    # means sds ns 
    
    # overall histogram of PHQ4 scores
    
    ggplot(data=phq, aes(phq4_total)) +
      geom_histogram(aes(y=100*..count../sum(..count..)))+
      labs(title="Patient-Health Questionaire (PHQ4) Scores ", subtitle  = "August 2020 Wave of The Understanding America Survey (n=6,371)", 
           x = "PHQ4 Total Score",
           y= "Presentage of Sample")+
      theme(plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5))
  
    # means sd n of PHQ4 scores by age ccategory
    phq%>%group_by(agecat)%>%
      summarise(mean_phq = mean( phq4_total, na.rm=TRUE), sd_phq = sd( phq4_total,na.rm=TRUE),n_phq= sum(!is.na( phq4_total)))
  
    # anova of PHQ scores by age categories
    phq_age_anova <- aov(phq4_total~ agecat, data =phq)
    summary(phq_age_anova)
    
    # Simle linear regression of PHQ scores by age category
    # first make factor version of agecat class(phq$agecat
    class(phq$agecat)
    phq<-phq%>%mutate(agecat=factor(agecat,levels = c(1,2,3,4),labels=c("[18, 30)","[30, 50)","[50,65)",">=65")))
    class(phq$agecat)
    
    slr_phq_age<-lm(phq4_total~agecat,data=phq)
    summary(slr_phq_age)
    
    # boxplots of PHQ4 scores by age category with some titles, and aesthetic updates from ggplot defaults
    ggplot(phq, aes(x=agecat, y=phq4_total,fill=agecat,alpha = 0.2)) + 
      geom_boxplot()+
      labs(title="Patient-Health Questionaire (PHQ4) Scores by Age", y = "PHQ4 SCore", subtitle= "August 2020 Wave of The Understanding America Survey", x = "Age Range (Years)")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
            axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 20),axis.title.x = element_text(size = 20)) +
      scale_x_discrete()+
      scale_y_continuous(limits=c(0,12), breaks=seq(0,12,2))+
      theme(axis.line = element_line(color = 'black'))+
      scale_fill_manual( values = c("blue","blue","blue", "blue"))+
      theme(legend.position="none")
    

                 
##### restaurant inspections scores, Austin TX

  austin<-read_csv("Food_Establishment_Inspection_Scores.csv")
  names(austin)<-make.names(names(austin),unique = TRUE)
   
  # get year of inspection
  library(lubridate())
  austin<-austin%>%mutate(year = year(mdy(Inspection.Date)))
  
  austin<-austin%>%mutate(score= Score)%>%select(-Score)
  austin<-austin%>%select(Inspection.Date,score,Facility.ID,year)
  class(austin$year)
  # redo year as factor
  austin<-austin%>%mutate(year=factor(year))

  write_csv(austin, "data/austin_scores.csv")

  # Histogram of Scores, combine for all 4 years 
  ggplot(data=austin , aes(score)) +
    geom_histogram(aes(y=100*..count../sum(..count..)),binwidth=2)     +
  labs(title="Inspection Scores 2019- 2020 , Restaurants in Austin TX USA", y = "Percentage of Sample", caption
       ="2022 Scores For January through March", x = "Inspection Scores")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
          ,axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))+
  xlim(40,100)+
  scale_y_continuous(limits=c(0,20))+
  theme(axis.line = element_line(color = 'black'))+
  scale_fill_manual( values = alpha(c("black"),.3))
  
# means sds ns of inspection scores by year
  
  austin%>%group_by(year)%>%
    summarise(mean_score = mean(score, na.rm=TRUE), sd_score = sd(score,na.rm=TRUE),n_score= sum(!is.na(score)))
           

  # anova of restaurant scores by year
  score_yr_anova <- aov(score~ year, data = austin)
  summary(score_yr_anova)

  # slr of restaurant scores by year
  slr<-lm(score~year,data=austin)
  summary(slr)
  summary(slr, cluster=c("Facility.ID"))
  
  # boxplot of restaurant scores by year
 ggplot(austin, aes(x=year, y=score)) + 
  geom_boxplot()+
  labs(title="Inspection Scores by Year Restaurants in Austin TX USA* ", y = "Inspection Scores", 
       subtitle= "Restaurants in Austin TX USA, 2019- 2022", x = "Year") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), plot.title =element_text(size=30,hjust = 0.5),plot.subtitle = element_text(size=24,hjust=0.5),plot.caption =element_text(size=12,hjust = 0,color="purple"),
        axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 20))+
  scale_x_discrete()+
  scale_y_continuous(limits=c(00,100), breaks=seq(00,100,10))+
  theme(axis.line = element_line(color = 'black'))+
  scale_fill_manual( values = c("blue","blue"))+
  theme(legend.position="none")

# DPCA trial, patient with primary biliary cirrhosis

  
  pbc<-read.csv("pbc survival.csv")
  
  # select only several variables
  
  pbc<-pbc%>%select(id,ftime,death,trt)
  
  table(pbc$trt)
  table(as.numeric(pbc$trt))
  
  # create factor indicator of DPCA/Placebo
  pbc<-pbc%>%mutate(drug= as.numeric(trt)-1)
  pbc<-pbc%>%mutate(DPCA=factor(drug, levels =c(0,1),labels =c("Placebo","DPCA")))
  pbc<-pbc%>%select(id,ftime,death,DPCA)
  class(pbc)
  names(pbc)
  
  # look at first several values
  head(pbc)
  
  # datafrmame structure
  str(pbc)
  pbc[1:5,]
  
  # get # missing values in drug, ftime and death variables

  pbc%>%summarise(misstrt=sum(is.na(DPCA)),misstime=sum(is.na(ftime)),missdeath=sum(is.na(death)))

# incidence rates by treatment, and irr
  irstrt<-pbc%>%
  group_by(DPCA)%>%
  summarise(ir=sum(death, na.rm=TRUE)/sum(ftime, na.rm=TRUE))

  irstrt
  # incidence rate ratio
  irstrt[1,2]/irstrt[2,2]

  # now for KM curve estimates, and Cox PH

  library(survminer)
  
  # create version of follow-up time in years
  pbc<-pbc%>%mutate(survyr = ftime/365.25)
  library(survival)
  
  pbc<-pbc%>%mutate(survset= Surv(survyr, death))
  
  #Cox
  coxph <- coxph(survset ~ DPCA, data =  pbc)
  summary(coxph)
  
  # simple, no frills KM curves
  kmpbc2<-survfit(Surv(survyr, death) ~  DPCA, data =pbc)
  ggsurvplot(kmpbc2,conf.int = F,risk.table = TRUE, censor=F)


                 

