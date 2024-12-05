pbcData$SurvObj <- with(pbcData, Surv(survyr, death == 1))


pbcData <- pbcData |> 
  mutate(survobject=Surv(survyr,death==1))

 pbcData |> 
  mutate(survobject=Surv(survyr,death))

 km_model <- pbcData |> 
   survfit(survobject~ 1,data=_,conf.type="log-log")
 km_model |> 
   ggsurvfit(color="steelblue",linewidth=1)+
   add_risktable()+
   add_confidence_interval(fill="steelblue")
 
   
 km.drug |> 
   ggsurvfit(linewidth=1)+
   add_risktable()+
  
   labs(x="Months")
   
   aml |> 
   survfit(Surv(time,status)~1,data=_)
   
  head(summary(km.overall))

  summary(km_model)   
log(-log(0.991) ) 
ggsurvplot(km_model)



plot(km.drug, fun="cloglog", ylab="log(-log(Survival Probability)",
     xlab="Analysis time (shown on log scale)")


fit <- survreg(survobject~drug,data=pbcData,dist="weibull" ,link="cloglog") 
fiit <- survreg(Surv(survyr,death)~ drug,data=pbcData,dist = "weibull",link="cloglog")

  pbcData |> 
  survreg(survobject~drug,data=_) |> 
    ggsurvfit()



augment(fit)

ggsurvplot(km.drug,data=pbcData)

kmdata |> 
  ggplot(aes(time,))


kmaugmented <- augment(km.overall)
tidy(km.overall)
survreg(km.overall)
km.overall

km.overall <- km.overall |> 
  mutate(surv_probs=-log(-log(surv)))

kmdata <- data.frame(time=km.drug$time,cloglog=-log(-log(km.drug$surv)))
kmdata |> 
  ggplot(aes(y=cloglog,x=log(time),color=drug))+
  geom_line()
library(gtsummary)
tbl_summary(summary(km.overall))

