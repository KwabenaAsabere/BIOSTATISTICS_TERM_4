library(tidyverse)
library(haven)
library(survey)

nhanes_2017_20u <- read_csv('NHANES data 2017_20.csv')
lipids_data <- read_xpt("TRIGLY_J.XPT")


nhanes_2017_20_new <- read_csv('NHANES data 2017_20.csv') %>% 
  left_join(lipids_data, by = "SEQN") 


nhanes_2017_20u <- nhanes_2017_20 %>% 
  left_join(lipids_data, by = "SEQN")

nhanes_2017_20u <- nhanes_2017_20u %>% 
  mutate(
    current_smoke = case_when(
      SMQ040 ==1|SMQ040 == 2 ~ "yes",
      SMQ040 ==3 ~"no",
      SMQ020 ==2 ~"no"
    ))
    
                         
table(nhanes_2017_20u$current_smoke)


nhanes_2017_20u <-  nhanes_2017_20u %>% 
  select(SEQN,RIDAGEYR,RIAGENDR,RIDRETH3,WTINT2YR,current_smoke,SDMVPSU,SDMVSTRA)

nhanes_2017_20u <- nhanes_2017_20u %>% 
  mutate(sex = if_else(RIAGENDR ==2,1,0),
        raceth = factor(RIDRETH3) ) 


##percentage of smokers in nahes_2017_20u
nhanes_2017_20u %>% 
  filter(!is.na(current_smoke)) %>% 
  count(current_smoke) %>% 
  summarise(proportion = (n/sum(n))*100) 

## incorporating survey design

nhanes_2017_20uw<-svydesign(id=~SDMVPSU, weights=~WTINT2YR,strata=~SDMVSTRA,
                            nest=TRUE, data= nhanes_2017_20u)


## percentage of smokers after incorporating survey design
prop.table(svytable(~current_smoke, design = nhanes_2017_20uw))


##What is the unweighted mean age for smokers in the nhanes_2017_20u dataset?

nhanes_2017_20u %>% group_by(current_smoke) %>% 
  summarise(mean_age_smk = mean(RIDAGEYR,na.rm = TRUE))

##unweighted mean difference in age for smokers to non-smokers
nhanes_2017_20u %>% group_by(current_smoke) %>% 
  reframe(mean_age_smk = mean(RIDAGEYR,na.rm = TRUE)) %>% 
  reframe(mean_age_diff = diff(mean_age_smk))

##weighted mean age for smokers in the nhanes_2017_20uw dataset
svyby(formula = ~ RIDAGEYR, by = ~ current_smoke, design =
        nhanes_2017_20uw, FUN = svymean, na.rm=TRUE)

## the weighted mean difference in age for smokers to non-smokers in
## nhanes_2017_20uw dataset

svyttest(RIDAGEYR~current_smoke, nhanes_2017_20uw)

##unweighted proportion of male smokers in the nhanes_2017_20u dataset

nhanes_2017_20u %>% 
  filter(sex == 0) %>% 
  filter(!is.na(current_smoke)) %>% 
   count(current_smoke) %>% 
  reframe(smk_prop = n/sum(n))




prop.table(table(nhanes_2017_20u$sex,nhanes_2017_20u$current_smoke))



##p-value from the unweighted chi-sq test comparing the proportions male smokers and
## female smokers in the nhanes_2017_20u dataset

nhanes_2017_20u %$%
  table(sex,current_smoke) %>% 
  chisq.test()


##weighted proportion of male smokers in the nhanes_2017_20u dataset
prop.table(svytable(~current_smoke+sex, design = nhanes_2017_20uw),
           margin=2)

##p-value from the weighted chi-sq test comparing the proportions male smokers and
##female smokers in the nhanes_2017_20u dataset

svychisq(~current_smoke+sex, design = nhanes_2017_20uw,
         statistic="Chisq")

##weighted proportion of smokers who identify as Mexican American

prop.table(svytable(~current_smoke+raceth, design = nhanes_2017_20uw),
           margin=2)

##the weighted proportion of smokers who identify as Non-Hispanic White

prop.table(svytable(~current_smoke+raceth, design = nhanes_2017_20uw),
           margin=2)
##p-value from the weighted chi-sq test comparing the proportions of smokers across
##ethnicity in the
svychisq(~current_smoke+raceth, design = nhanes_2017_20uw,
         statistic="Chisq")

svychisq(~current_smoke+raceth, design = nhanes_2017_20uw,
         statistic="Chisq")


##the p-value from the weighted chi-sq test comparing the proportions of smokers across
## ethnicity in the US population in 2017-20
svychisq(~current_smoke+sex, design = nhanes_2017_20uw,
         statistic="Chisq")


##the weighted mean LDL (LBDLDL) cholesterol for smokers in the nhanes_2017_20u
##dataset
nhanes_2017_20uw<-svydesign(id=~SDMVPSU, weights=~WTINT2YR,strata=~SDMVSTRA,
                            nest=TRUE, data= nhanes_2017_20u)


svyby(formula = ~LBDLDL, by = ~ current_smoke,design = nhanes_2017_20uw,
      FUN = svymean, na.rm = TRUE)

##the weighted mean difference in LDL cholesterol for smokers to non-smokers in
##nhanes_2017_20u dataset
svyttest(LBDLDL~current_smoke, nhanes_2017_20uw)

### PROPORTION OF RACES

prop.table(svytable(~current_smoke+raceth, design = nhanes_2017_20uw),
           margin=2)

##the p-value from the weighted t-test comparing the mean LDL between smokers and
##non-smokers in the US population in 2017-20

nhanes_2017_20u <- nhanes_2017_20u %>% 
  mutate(current_smoken = if_else(current_smoke== "yes",1,0)) %>% 
  mutate(current_smoken = as.numeric(current_smoken))


lfit<-loess( nhanes_2017_20u$current_smoken~ nhanes_2017_20u$RIDAGEYR)


dfphat<-tibble(age=lfit$x, phat=lfit$fitted,lnodds=log(phat/(1-phat)))

ggplot( dfphat, aes(x = age, y= lnodds )) + geom_point()+geom_line()+
  labs(title = "Estimated Ln Odds of Smoking by Age, Smoothed",
       y = "Ln Odds of Smoking",
       x="Age Years")+
  scale_x_continuous(breaks=seq(20,80,by=5))




### adding spline terms

nhanes_2017_20u <-  nhanes_2017_20u %>% 
  mutate(
    agespl35 = if_else(RIDAGEYR> 35,RIDAGEYR-35,0),
    agespl65 = if_else(RIDAGEYR > 65,RIDAGEYR - 65,0),
    ldlgt135 = if_else(LBDLDL>135,1,0)
  )
  

uwlr<-
  glm(current_smoken~sex+raceth+RIDAGEYR+agespl35+agespl65+ldlgt135,data =
        nhanes_2017_20u, family = binomial(link="logit"))

summary(uwlr,exp=TRUE)
summary(uwlr)

exp(coef(uwlr))  
confint(uwlr)

##code for fitting the weighted logistic regression and extracting its key results on the odds
##and odds ratio scale


s1<-svydesign(id=~SDMVPSU, weights=~WTINT2YR,strata=~SDMVSTRA,
              nest=TRUE,
              data=nhanes_2017_20u%>%filter(!is.na(current_smoke)&RIDAGEYR>=18))

wlr<-
  svyglm(current_smoken~sex+raceth+RIDAGEYR+agespl35+agespl65+ldlgt135,design
         = s1, family=quasibinomial,na.action = na.omit)

summary(wlr)
exp(coef(wlr))
exp(confint(wlr))


















  
