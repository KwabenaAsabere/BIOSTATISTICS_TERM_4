# R code for results presented in lecture set 7

library(tidyverse)

# NHANES: obesity risk and sex
setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2019/lecture slides/lecture set 7/datasets/Excel")
nhaneso<-read.csv("NHANES2k14_obesity.csv", header=T)
names(nhaneso)
table(nhaneso$riagendr)

# gender is coded as 2 for females and 1 for males (source: https://wwwn.cdc.gov/nchs/nhanes/2013-2014/demo_h.htm#RIAGENDR)
# to make analyses with gender easier, will a factore variabl for sex which includes the labels "Male" and "Female"

nhaneso<-nhaneso%>%mutate(female=factor(riagendr, labels=c("Male", "Female")))
table(nhaneso$female)

# since obesity is the outcome of interest, I'll work with a subset of the data that only includes those observations with non-missing bmi
# and persons >= 18 years old

# total in original sample
length(nhaneso$riagendr)

# subset of those with BMI who are 18+ years old 

nhanesos<-nhaneso%>%filter(!is.na(bmxbmi) & ridageyr>=18)

length(nhanesos$riagendr)

# cutoff of for being classified as obese: BMI >= 30
# create indicator
nhanesos<-nhanesos%>%mutate(obese=ifelse(bmxbmi>=30, 1, 0))

# obesity by sex 

nhanesos%>%count(obese, female) %>%
  group_by(female) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  spread(female, prop)


# logistic regression

slr1<-glm(obese~female,family=binomial(link="logit"),data=nhanesos )
summary(slr1)
names(slr1)
exp(coef(slr1))
exp(confint(slr1))


# obesity and HDL
# 274 missing hdl cholesterol,so I  subset out the NAs
nhanesos<-nhanesos%>%filter(!is.na(lbdhdd))

# logistic regression
slr2<-glm(obese~lbdhdd,family=binomial(link="logit"),data=nhanesos )
summary(slr2)
exp(coef(slr2))
exp(confint(slr2))

# boxplot HDL bys sex, and slimple linear egression

ggplot(nhanesos, aes(y=lbdhdd, col=female))+
  geom_boxplot()

slr3<-lm(lbdhdd~female,data=nhanesos )
summary(slr3)

# Multiple logisitc with sex and HDL

mlr1<-glm(obese~female+lbdhdd,family=binomial(link="logit"),data=nhanesos )
summary(mlr1)
exp(coef(mlr1))
exp(confint(mlr1))

# age, categorized
library("gtools")

nhanesos<-nhanesos%>%mutate(ageq =quantcut(ridageyr, q=c(0,0.25,.5,.75,1)))
slr4<-glm(obese~ageq,family=binomial(link="logit"),data=nhanesos )
summary(slr4)

# mlr 2 with hdl, age, sex

mlr2<-glm(obese~female+lbdhdd+ageq,family=binomial(link="logit"),data=nhanesos )
summary(mlr2)
exp(coef(mlr2))
exp(confint(mlr2))

# mlr 2 with hdl categorizd, sex and age categorized
# age, categorized

nhanesos<-nhanesos%>%mutate(hdlq=quantcut(nhanesos$lbdhdd, q=c(0,0.25,.5,.75,1)))

mlr3<-glm(obese~female+hdlq+ageq,family=binomial(link="logit"),data=nhanesos )
summary(mlr3)
exp(coef(mlr3))
exp(confint(mlr3))
