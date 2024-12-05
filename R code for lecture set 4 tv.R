# Lecture 4: Simple Cox Regression

#  Section C (and E , ie 95% CIs and pvalues)
  library(tidyverse)
  library(survival)  
  library(survminer)

### PBC trial survial
  setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2019/lecture slides/lecture set 4/datasets/Excel")
  pbc<-read.csv("pbc survival.csv")
  names(pbc)
  
  # followup time, pbc$ftime, is in days.  I want to create new version in years
  pbc<-pbc%>%mutate(survyr=ftime/365.25)

  # create the "survival object" and add column to pbc: does not seem to work in dplyr
  
  pbc$SurvObj <- Surv(pbc$survyr, pbc$death)
  
  # create kaplan-meier curve
  
  kmtrt<-survfit(SurvObj ~trt, data = pbc)
  ggsurvplot(kmtrt,conf.int = F, risk.table = FALSE, censor=F)

  # Cox regression:  survival by drug group
  coxph1 <- coxph(SurvObj ~ trt, data =  pbc)
  summary(coxph1)

  # hmm, as it turns out treatment is code the opposite of what is needed if we want comparison in direction of DPCA vs placebo
  
  table(pbc$trt)
  class(pbc$trt)
  table(as.numeric(pbc$trt))
  
  pbc<-pbc%>%mutate(dpca=2-as.numeric(trt))
  
  coxph1 <- coxph(SurvObj ~ dpca, data =  pbc)
  summary(coxph1)
  
  
### Vitamin Supplementation Trial
  
  infantm<-read.csv("nepal infant mort.csv")
  
  # km curves by treatment
  
  infantm$SurvObj <- with(infantm, Surv(stime, death == 1))
  
  kmtrt<-survfit(SurvObj ~treat, data = infantm)
  ggsurvplot(kmtrt,conf.int = F, risk.table = FALSE, censor=F)
  
  # truncate y asis to 0.9 to 1.0
  ggsurvplot(kmtrt,conf.int = F, risk.table = FALSE, censor=F,ylim=c(.9,1))

  # Cox regression
  # treatment is set up so that beta carotence is the reference:  need to recode to make placebo ref
  table(infantm$treat)
  table(as.numeric(infantm$treat))
  
  infantm<-infantm%>%mutate(treat2=as.factor(0+as.numeric(infantm$treat=="beta carotene")+2*as.numeric(infantm$treat=="vitamin A")))
  coxph2 <- coxph(SurvObj ~ treat2, data = infantm)
  summary(coxph2)

# Section D (and E , ie 95% CIs and pvalues)

#  baseline bilirubin and mortality: DPCS trial
  names(pbc)
  
  # boxplot of bilirubin
   ggplot(pbc,aes(y=bil))+
    geom_boxplot()
   
  # Cox regression, bilirubin as continous
  coxph3 <- coxph(SurvObj ~ bil, data =  pbc)
  summary(coxph3)
  
  # Cox regression, bilirubin as in quartiles
  library("gtools")
  
  pbc<-pbc%>%mutate(bq=quantcut(pbc$bil, q=c(0,0.25,.5,.75,1)))

  class(pbc$bq)
  table(pbc$bq)
  coxph4 <- coxph(SurvObj ~ bq, data =  pbc)
  summary(coxph4)
    
    # km curves by quartile
  kmbq<-survfit(SurvObj ~ bq, data = pbc)
  ggsurvplot(kmbq,conf.int = F, risk.table = FALSE, censor=F)

# Infant mortality and gestational ages
  
  names(infantm)
  # boxplot of gest ages

  ggplot(infantm,aes(y=gestage))+
    geom_boxplot()
  
  coxph5 <- coxph(SurvObj ~ gestage, data =  infantm)
  summary(coxph5)
  
  # Cox regression, gestational age catoegories

  # gacat is another function in the "gtools" library that makes it relatively
  # easy to take a continuous predictor and catrogrize it
  # Instead of ceating quartiles (o cuts based on other percentiles), this funtion allows
  # the user to designate the intervals 
  
  infantm<-infantm%>%mutate(gacat=cut(infantm$gestage, breaks=c(min(infantm$gestage, na.rm=T),36,38,39,41,max(infantm$gestage, na.rm=T)),right=T))
  class(infantm$gacat)
  table(infantm$gacat)
  coxph6 <- coxph(SurvObj ~ gacat, data =  infantm)
  summary(coxph6)
  
  # km curves by quartile
  infantkm<-survfit(SurvObj ~ gacat, data = infantm,conf.type="none")
  
