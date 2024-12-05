
library(tidyverse)
packages <- c("expss","table1","car","jtools","tidyverse","multcomp","gtsummary")
map(packages,library,character.only = TRUE)

nhanes <- read_csv("NHANES data 2017_20.csv")
nhanes %>% 
  select(where(is.numeric)) %>% 
  map_df(summary)


nhanes_summary <-
  nhanes %>% 
  select(where(is.numeric)) %>% 
  map(summary) 

print(nhanes_summary)

dim(nhanes)

nhanes_summary$BMXBMI

nhanes %>% 
  select(where(is.character)) %>% 
  map(table)

nhanes %>% summarise(sum(is.na(LBDLDL)))

         nhanes %>% 
            filter( RIDAGEYR>= 18) %>% 
            summarise(sum(is.na(RIDAGEYR)))

         nhanes_subset <- nhanes %>% 
           filter( RIDAGEYR>= 18) %>% 
           select(SEQN,RIAGENDR,RIDAGEYR,DMDHHSIZ,BMXBMI,BPXSY1,BPXSY2,BPXSY3,LBDLDL,
                  INDFMPIR,INDHHIN2,RIDRETH3)
         
nhanes_subset <-nhanes_subset %>% 
  rowwise() %>% 
  mutate( SBPc= mean(c(BPXSY1,BPXSY2,BPXSY3),na.rm = TRUE)) %>% 
  relocate(SBPc, .after = BPXSY3) %>% 
  mutate(sex = factor(RIAGENDR,levels = c(1,2),labels=c("Male","Female")),
                     raceeth = factor(RIDRETH3,
                                      levels=c(1,2,3,4,6,7),
                                      labels = c("Mexican American","Other hispanic","Non-hispanic White",
                                                 "Non-Hispanic Black","Non-Hispanic Asian","Other Race-Including Multi-Racial")) )%>% 
  
  mutate( hhin=factor(INDHHIN2, levels=c(1:10,12:15,77,99),labels= c("$ 0 to $ 4,999","$ 5,000 to $ 9,999","$10,000 to $14,999","15,000 to $19,999",
                                                                     "$20,000 to $24,999","25,000 to $34,999","$35,000 to $44,999","$45,000 to $54,999",
                                                                     "$55,000 to $64,999","$65,000 to $74,999","$20,000 and Over","Under $20,000	",
                                                                     "$75,000 to $99,999","$100,000 and Over	","Refuse","Dont Know"))) %>% 
  mutate(flag = factor(as.numeric(!is.na(LBDLDL)& !is.na(INDHHIN2)& !is.na(BMXBMI)
                                  & !is.na(RIDAGEYR)),levels = c(1,0),
                                  labels = c("Not in Regression Sample",
                                             "In Regression Sample")))

nhanes_subset <- 
  nhanes_subset %>% 
  apply_labels(SEQN = 'respondent id',
               RIDAGEYR= "Age(years)",
               BMXBMI= "Body Mass Index",
               SBPc = "Systolic Blood Pressure(mmHg)",
               LBDLDL= "LDL Cholesterol (mg/dl)",
               sex = "Sex",
               raceeth= 'Race/Ethnicity'
               )


  
  
         
         
         
         
         








