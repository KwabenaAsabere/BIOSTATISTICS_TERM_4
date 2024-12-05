library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)

SurvObj <- colon %$% Surv(time,status)





lymphoma <- read_csv("lymphoma.csv")


dependent <- "Surv(days,died)"
explanatory <- "stage"




 lymphoma |> 
  finalfit(dependent,explanatory,add_dependent_label = FALSE) |> 
  rename("Overall Survival"=label) |> 
  rename(" "=levels) 
 lymphoma |> 
   ff_plot(dependent,explanatory)
   
 

 
 colon.OS <- colon |> 
   filter(etype==1)
 
 
 dependent= "Surv(time,status)"
explanatory=c("rx","obstruct","differ","extent","surg","node4")  
colon.OS |> 
  finalfit(dependent,explanatory,add_dependent_label = FALSE) |> 
  rename("Overall Survival"= label) |> 
  rename(" "=levels)  
  

table <- colon.OS |> 
  finalfit(dependent,explanatory,add_dependent_label = FALSE) |> 
  rename("Overall Survival"= label) |> 
  rename(" "=levels) 

library(knitr)
t <- kable(table,align=c("c","c","c","c"),"simple")


colon.OS|> ff_plot(dependent,explanatory)
