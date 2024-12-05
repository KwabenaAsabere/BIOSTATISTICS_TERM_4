# R Lecture 6
# Multiple Linear Regression with R

library(tidyverse)
setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2019/lecture slides/lecture set 1/datasets/Excel")
nanthro<-read.csv("nepal children lt 12months.csv", header=T)
names(nanthro)

# simple regressions
# sex
slr1<-lm(nanthro$armcirc~nanthro$female)
summary(slr1)

# height
slr2<-lm(nanthro$armcirc~nanthro$height)
summary(slr2)

# weight
slr3<-lm(nanthro$armcirc~nanthro$weight)
summary(slr3)

# age, categorized
library("gtools")
nanthro$ageq <-quantcut(nanthro$age, q=c(0,0.25,.5,.75,1))
slr4<-lm(nanthro$armcirc~nanthro$ageq)
summary(slr4)
anova(slr4)


# MLR 1, height and weight
mlr1<-lm(nanthro$armcirc~nanthro$height+nanthro$weight)
summary(mlr1)
coef(mlr1)
confint(mlr1)

# MLR 2, height and weight, age q
mlr2<-lm(nanthro$armcirc~nanthro$height+nanthro$weight+nanthro$ageq)

summary(mlr2)
anova(mlr1, mlr2)

# adjusted variable plot for height
library("car")

avPlot(lm(nanthro$armcirc~nanthro$height+nanthro$weight+nanthro$ageq)
       ,nanthro$height, main ="Relationship Between AC (cm) and Height (cm) Adjusted for Weight(kg) \nand Age: Nepalese Children < 12 Months Old"
       , cex.main=1,ylab="Arm Circumference (cm)", xlab="Height (cm)")
text(4,1.5,expression(hat(beta)[height]~"=-0.09"))




