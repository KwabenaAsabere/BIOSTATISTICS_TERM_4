

# load libraries needed for analyses
  library(tidyverse)
  library(survival)
  library(survminer)
  library(jtools)
  
                   

# load data from working directory, and look at an overview of the dataset
  
    
  load("umaru.Rdata")
    
    str(umaru)
    # treatment group counts
    table(umaru$treat)

# first, let's focus on relapses in first year after rehab
# will need to truncate the time at 365, and censor anyone with 
# a time (either relapse or censoring) > 365 at 365
  
    umaru<-umaru%>%
      mutate(relapse365= ifelse(is.na(relapse),NA, ifelse(time>365,0, relapse)))%>%
      mutate(time365= ifelse(is.na(time),NA, ifelse(time>365,365, time)))

    
    # create survival object with Surv
    umaru<-umaru%>%mutate(survobject=Surv(time365, relapse365 == 1)) 


# KM curves and simple Cox regressions

    # treatment type (long-term versus short-term)
    kmtrt<-survfit(survobject ~treat, data = umaru)
    ggsurvplot(kmtrt,conf.int = F, risk.table = FALSE, censor=F,
               
               ylim=c(0,1),
               break.y.by = 0.05)
    
  
    coxtreat<- coxph(survobject ~ treat, data = umaru)
    tidy(coxtreat, conf.int=TRUE, exponentiate =TRUE)


#### binning and poisson regression

 ### Create the binning variable in (roughly) 1 month increments
    umaru<-umaru%>% 
    mutate(tbin = tcut(rep(0, nrow(umaru)), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,365)))
  
    ### Create the rates table 
    pyearsObj = pyears(survobject ~ tbin+treat , data.frame=TRUE, scale=1,data=umaru)
    
    ratesData = pyearsObj$data 
    ratesData<-ratesData%>%mutate(offset=log(pyears))

# poisson model 1:  assuming constant rate over follow-up
    pois1 <- glm(event ~treat + offset(log(pyears)), poisson, data=ratesData)
    summ( pois1, exp=TRUE,  confint = getOption("summ-confint", TRUE),digits = getOption("jtools-digits", 4))

# poisson assuming ln(risk) linear with time
pois2 <- glm(event ~treat +as.numeric(tbin)+ offset(log(pyears)), poisson, data=ratesData)
summ( pois2, exp=TRUE,  confint = getOption("summ-confint", TRUE),digits = getOption("jtools-digits", 4))

# poisson assuming consistent rate within each month of follow-up
pois3 <- glm(event ~treat +tbin+ offset(log(pyears)), poisson, data=ratesData)
summ( pois3, exp=TRUE,  confint = getOption("summ-confint", TRUE),digits = getOption("jtools-digits", 4))

