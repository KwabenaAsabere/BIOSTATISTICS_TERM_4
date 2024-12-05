#  R Script for R Lecture 3

# load library tidyverse
library(tidyverse)

# need to load the library "SpatialEpi"
  library(SpatialEpi)

# load data
  data(pennLC)
  names(pennLC)
  
# created data frame of pennLC lung cancer counts data
  
  plc<-pennLC$data
  class(plc)
  names(plc)
  head(plc)
  length(plc$county)
  length(unique(plc$county))

# poisson regression, sex
  
  class(plc$gender)
  table(plc$gender)
  
  spr1mtf<- glm(cases ~ gender+offset(log(population)), family="poisson"(link="log"), data=plc)
  summary(plc$population)
  
  # how many observations have a population count of 0?
  test1<-(plc$population==0)
  table(test1)
  
  # replace the one observation with a population value of 0 to a population value of 1
  # could also remove this observatin from dataset
  
  plc<-plc%>%mutate(population=ifelse(population==0,1,population))

  # Now do a Poisson regression on sex
  spr1mtf<- glm(cases ~ gender+offset(log(population)), family="poisson"(link="log"), data=plc)


  # IR in females, and 95% CI, per 100,000 PYS
  
      c1<-coef(spr1mtf)
      c1[1]
      # per person year
      exp(c1[1])
      # per 100,000 PY
      exp(c1[1])*100000
  
      # IR in males, and 95% CI, per 100,000 PYS
      
      # per person year
      exp(c1[1]+c1[2])
      # per 100,000 PY
      exp(c1[1]+c1[2])*100000
      
      # IRR males to females and 95% CI
      
        #IRR is exponentiated slope
       exp(c1[2]) 
       
      # confidence interval for lIRRR
       confint1<-confint(spr1mtf)
       confint1[2,]
       
      # to get result on IRR scale
       exp(  confint1[2,])

      # note about reversing direction
       
  ### Poisson Regression with age (categorized) as a predictor
      
       # poisson regression, age cat

       class(plc$age)
       table(plc$age)
       table(as.numeric(plc$age))
       spr2 <- glm(cases ~ age+offset(log(population)), family="poisson"(link="log"), data=plc)
       
       # want to recreate age so that < 40 is the reference
       
       plc<-plc%>%mutate(agecat=ifelse(as.numeric(age)==4,1,as.numeric(age)+1))%>%
            mutate(agecat=factor(agecat,labels= c("< 40", "40-59","60-69",">=70")))
      
       class(plc$agecat)
       table(plc$agecat)
       
       spr2 <- glm(cases ~ agecat+offset(log(population)), family="poisson"(link="log"), data=plc)
       summary(spr2)
       
       # IRR and 95% CI for > 70 years old compared to < 40 years old
       c2<-coef(spr2)
       c2[4]
       confint2<-confint(spr2)
       confint2
       confint2[4,]
       
       exp(c2[4])
       exp(confint2[4,])

###############################
# Fast food restaurant density and percentage of families living below poverty line in Baltimore
       
       setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2019/lecture slides/lecture set 3/data sets")
       ff<-read.csv("fast food outlets baltimore msa.csv", header=T)
       names(ff)
       head(ff)
       summary(ff$Count)
       length(ff$Count)
       summary(ff$TotPop)

       # calculate rate in each of the 615 censues tracts
       ff<-ff%>%mutate(rate=Count/TotPop)
       
       # boxplot of the rates
       
       ggplot(ff,aes(y=rate*10000))+
         geom_boxplot()
       
      # boxplots of percentage of hh below poverty level
       ggplot(ff,aes(y=Perc_BelPo))+
         geom_boxplot()
     
      # scatterplot of rates vs poverty
       ggplot(ff,aes(x=Perc_BelPo,y=rate))+
         geom_point()+
         geom_smooth(se=F)

       
       #  fit Poisson regression with percentage of households below poverty level as predictor
       spr3 <- glm(Count ~ Perc_BelPo+offset(log(TotPop)), family="poisson"(link="log"), data=ff)
       summary(spr3)
       
       # turns out one track has a population of zero
       ff<-ff%>%filter(TotPop>0)
       spr3 <- glm(Count ~ Perc_BelPo+offset(log(TotPop)), family="poisson"(link="log"), data=ff)
       summary(spr3)
       
       
       # IRR (and 95% CI) of fast food restaurants per 1 % difference in housholds living below the poverty level
       
       c3<-coef(spr3)
       c3[2]
       confint3<-confint(spr3)
       confint3
       confint3[2,]
       
       exp(c3[2])
       exp(confint3[2,])
       
       # IRR (and 95% CI) of fast food restaurants per 10 % difference in housholds living below the poverty level
       
       exp(10*c3[2])
       
       #Notice, though:
         exp(c3[2])^10
         
       exp(10*confint3[2,])
  
################################################################################################
# R Lecture 3, PArt 3
  # UMARU relapse data
       
       setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2017/lecture slides/lecture set 3/data sets")
       # get data from .csv into a dataframe
       umaru<-read.csv("umaru relapse data.csv", header=T)
       
       # what columns are in these data?  How many rows and columns are in the dataframe?
       names(umaru)
  
       # look at the first few rows
       head(umaru)
       
       # How many persons were randomized to each treatment grouo?
       umaru%>%group_by(treat)%>%
         summarise(total=n())
       
       # types of data for the time and relapse values
       class(umaru$time)
       class(umaru$relapse)
       

       # total number of relapses and followup time by treatment group; create incidence rates
       irs<-umaru%>%group_by(treat)%>%
          summarise(total_relapse=sum(relapse), total_fup=sum(time))%>%
          mutate(ir=total_relapse/total_fup)

       
       # IRR
       irs[2,4]/irs[1,4]
       
       # Poisson Regressions
       
       
       # treatment as a predictor
       table(umaru$treat)
       spr4 <- glm(relapse ~ treat+offset(log(time)), family="poisson"(link="log"), data=umaru)
       
       coef(spr4)
       exp(coef(spr4))
    