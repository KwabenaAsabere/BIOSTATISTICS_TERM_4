library(tidyverse)
library(survival)
library(ggsurvfit)
library(survminer)
library(broom)

aml <- aml

km <- survfit(Surv(time,status) ~ 1,data=aml)
km

aml |> 
  survfit(Surv(time,status)~1,data=_)

## using tidy() from the broom package to create a tibble
tidy(km)

km_table <- tidy(km)

plot(km,xlab="Months",ylab="Survival probability")

km2 <- aml |> 
  survfit(Surv(time,status)~x,data=_)

km_table2 <- tidy(km2)
km_table2
plot(km2)


## using ggsurvplot from survminer
ggsurvplot(km2,conf.int = TRUE,
           risk.table = TRUE)

ggsurvplot(km2)

## to calculate the Logrank statistic to compare the two curves
## Ho : survival across the two groups are equivalent

survdiff(Surv(time,status)~x,data=aml)
##







##using ggsurvfit
km |> 
  ggsurvfit(color='steelblue',linewidth=1)+
  add_risktable()+
  add_confidence_interval(fill="steelblue")+
  labs(x="Months")

aml |> 
survfit2(Surv(time,status)~x,data=_) |> 
  ggsurvfit(linewidth=1)+
  add_risktable()+
  scale_color_brewer(palette="Set1")+
  add_confidence_interval()+
  scale_fill_brewer(palette = "Set1")


## if you want to fit KM for the whole dataset you use ~1 
## if you want to check survival by treatment groups,use ~treatment




# COX PROPORTIONAL HAZARDS MODEL ------------------------------------------

lung <- lung
## t =survival time in days
## status 1= censored, 2 = dead
## sex 1=male, 2 = female
## wt.loss = weight loss in pounds in last 6 months
#3 age = age in years ( assessed at beginning)

## Cox model = coxph(formula,data= )
## formula = Surv(time, event) ~x1 + x2 + ..xp

lung_cox <- coxph(Surv(time,status) ~age + sex + wt.loss,data = lung)
summary(lung_cox)

## tidying up cox regression output

lungcoxtab <- tidy(lung_cox,exponentiate=TRUE,conf.int = TRUE)
lungcoxtab 

## Plotting the Hazard ratios
lungcoxtab |> 
  ggplot(aes(y=term,x=estimate,xmin=conf.low,xmax=conf.high))+
  geom_pointrange()+
  geom_vline(xintercept=1,color="red")+
  labs(x="Hazard Ratios",title="Hazard Ratios & 95% CIs",y= "Predictor Variable")+
  theme_bw()


## Predicting survival after coxph() with survfit()

## predict survival fxn for subject with mean values on all covariates
## However predicting survival at a mean o all covariates may not make sense
## especialy if one or more of the covariates are factors(categorical)
## instead it is recommended to supply to always supply a data frame of covariate values 
## at which to predict the survival function to the newdata =() option of survfit()

## making a new data frame
plotdata <- data.frame(age=mean(lung$age,na.rm=TRUE),
                       sex=1:2,
                       wt.loss=mean(lung$wt.loss,na.rm=TRUE))

plotdata
## supplying newdata frame to survfit
surv_by_sex <- survfit(lung_cox,newdata = plotdata)

survbysex_tab <-  tidy(surv_by_sex)

plot(surv_by_sex)

ggsurvplot(surv_by_sex,data=plotdata,censor=FALSE,legend.labs=c("Male","Female"))

## Assesing the proportional hazards assumptions

## 1) Chi-squared test based on Scheonfeld residuals is available with cox.zph()
##to test the hypothesis  Ho:covariate efect is constant(proportional) over time
## Ha: covariate effect changes over time

cox.zph(lung_cox)

plot(cox.zph(lung_cox),col="steelblue")





