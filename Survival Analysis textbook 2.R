library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)

SurvObj <- colon %$% Surv(time,status)


 colon.OS %>%  finalfit(coxph(SurvObj~ rx))

 dependent <- "SurvObj"  
explnatory <- "rx" 
colon.OS %>% finalfit(dependent,explnatory)


?finalfittrt <- read_csv("trt.csv")

lymphoma <- read_csv("lymphoma.csv")

lymphoma <- lymphoma |> 
  mutate(survobj=Surv(days,died))

dependent <- "Surv(days,died)"
explanatory <- "stage"




 lymphoma |> 
  finalfit(dependent,explanatory,add_dependent_label = FALSE) |> 
  rename("Overall Survival"=label) |> 
  rename(" "=levels) |> 
   rename (" "= all)
tidy(t)  
  


