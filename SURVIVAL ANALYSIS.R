library(tidyverse)

library(survival)
library(finalfit)
library(broom)
leaukaemia <- read_csv("trt.csv")

leaukaemia <- leaukaemia %>% 
  mutate(survobject = Surv(weeks,failure))

leuk_surv <- survfit(survobject~ 1,data = leaukaemia)
leuk_surv
summary(leuk_surv, times = c(0,5,10,15,20)) 
ggsurvplot(leuk_surv, conf.int = FALSE, censor = FALSE,color = "blue",risk.table = TRUE)





surv_leukaemia <- leaukaemia %$%
  Surv(weeks,failure)


surv_leukaemia <- leaukaemia %$% Surv(weeks, failure)


survfit_leukaemia <-survfit(surv_leukaemia~1,data=leaukaemia)

summary(survfit_leukaemia)

summary_leukaemia <- summary(survfit_leukaemia)



explanatory <- c("trt")

leaukaemia |> 
  surv_plot(dependent,explanatory,pval=TRUE)

library(survminer)
library(ggsurvfit)

leukaemia <- read_csv("trt.csv")


survfit2(Surv(weeks,failure)~trt,data=leaukaemia) |> 
  ggsurvfit()+
  add_risktable()+
  labs(y="Relapse")+
  add_confidence_interval()
 
df_colon 

leukaemia
 survfit2(Surv(weeks,failure)~trt,data=leaukaemia) |> 
  ggsurvfit()+
  add_risktable()+
  labs(y="Relapse")





