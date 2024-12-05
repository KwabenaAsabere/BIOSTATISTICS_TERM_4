library(tidyverse)
library(survival)
library(ggsurvfit)
library(finalfit)
library(broom)
library(gtsummary)
library(ggeasy)



colon_ca <- df_colon |> 
  select(time,status,surg)

colon_ca |> 
  tbl_summary(statistic = status ~ " {n}/{N}") |> 
  add_stat_label() |> 
  bold_labels()

sf1 <- survival::survfit(Surv(time,status)~surg,data=colon_ca)
sf1

sf2 <- ggsurvfit::survfit2(Surv(time,status) ~ surg,data = colon_ca)
sf2

survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_risktable()

##STANDARD PLOT
survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit()+
  scale_ggsurvfit()+
  add_risktable()+
  add_confidence_interval()


survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_risktable()+
  scale_ggsurvfit()+
  labs(y="Recurrence-free Progression")+
  ggeasy::easy_move_legend("top")

### Transformation
survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(type="risk",linewidth=1)+
  add_risktable()+
  scale_ggsurvfit()+
  labs(y="Recurrence-free Progression")

## Scale_ggsurvfit
##The y axis scale is always from 0 to 1
##You may however want to specify the x_axis scales

survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_risktable()+
  scale_ggsurvfit(x_scales=list(breaks=0:9))+
  labs(y="Recurrence-free Progression")


## ADD CONFIDENCE INTERVALS


survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_confidence_interval()


survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_confidence_interval()+
  labs(title="Default")


survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_confidence_interval()

##Group by statistic or strata

survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
   scale_ggsurvfit()+
  add_risktable(risktable_group = "risktable_stats")


##customizing the risk table statistics
survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_risktable(risktable_stats = "{n.risk}({cum.event})")

## Median summary
survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_censor_mark()+
  add_quantile(y_value = 0.5)+
  scale_ggsurvfit()



survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_quantile(y_value = 0.5)+
  scale_ggsurvfit()

## At a given time point
survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_censor_mark()+
  add_quantile(x_value = 5,linetype = "solid",linewidth = 1,alpha=0.3)+
  scale_ggsurvfit()

survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_censor_mark()+
  add_quantile(y_value = 0.5)+
  scale_ggsurvfit()+
  add_pvalue(caption="Log-rank{p.value}")+
  add_risktable()




























