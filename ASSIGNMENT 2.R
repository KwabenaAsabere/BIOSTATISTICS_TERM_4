library(tidyverse)
library(haven)
options(digits = 7)

demo_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT")
nrow(demo_data)   ## number of observations(row) in dataset
ncol(demo_data)  ## number of columns in dataset

demo_data %>% 
  distinct(SEQN) %>% 
  count() ## number of unique persons in the dataset

length(unique(demo_data$SEQN))

## mean age in years for this dataset
demo_data %>% 
  summarise(
    mean_ageyr = mean(RIDAGEYR)
  )


##number of people missing a value for age

demo_data %>% 
  summarise(age_missing = sum(is.na(RIDAGEYR)))

## number of persons with age less than or equal to 2 years

demo_data %>% 
  filter(RIDAGEYR <= 2) %>% 
  count()


## mean age in months for children less than or equal to 2 years

demo_data %>% 
  summarise(
    children_mean_age = mean(RIDAGEMN,na.rm = TRUE)
  )


body_data <- read_xpt("BMX_J.xpt")

## number of persons in body_data dataset

body_data %>% 
  distinct(SEQN) %>% 
  count()

## number of non-missing values of weight 

body_data %>% 
  summarise(
    wt_nonmissing = sum(!is.na(BMXWT))
  )

## number of missing values of weight

body_data %>% 
  summarise(
    wt_Missing = sum(is.na(BMXWT))
  )


##left join of demo_data and body_data to create nhanes_2017_20 dataset

nhanes_2017_20 <- demo_data %>% 
  left_join(body_data, by = "SEQN")

## number of observations in merged dataset
nrow(nhanes_2017_20)

## number of columns in merged dataset
ncol(nhanes_2017_20)


## number of observations with missing value of weight in merged dataset

nhanes_2017_20 %>% 
  summarise( 
    wt_nhanes_missing = sum(is.na(BMXWT)))


## individual with SEQN = 93786

nhanes_2017_20 %>% 
  filter( SEQN == "93786") %>% 
  print( width = Inf)

temp_nhanes <- nhanes_2017_20


## import smoking data and name it smoke_data
smoke_data <- read_xpt("SMQ_J.xpt")

## merge nhanes_2017_2020 with smoke_data
 
nhanes_2017_20 <-  nhanes_2017_20 %>% 
  left_join(smoke_data, by = "SEQN") ## 36 new variables added


nhanes_rightjoin <- nhanes_2017_20 %>% 
  right_join(smoke_data, by = "SEQN")



# SECTION 2 ---------------------------------------------------------------

demo_2015_2016 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT")
body_2015_2016 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT")
smoke_2015_2016 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT")

nhanes_2015_2016 <- demo_2015_2016 %>% 
  left_join(body_2015_2016, by = "SEQN")

nhanes_2015_2016 <- nhanes_2015_2016 %>% 
  left_join(smoke_2015_2016, by = "SEQN")

## number of observations in nhanes_2015_2016
nrow(nhanes_2015_2016)

## number of variables(columns in nhanes_2015_2016)
ncol(nhanes_2015_2016)

## number of observations with missing value of weight in nhanes_2015_2016

nhanes_2015_2016 %>% 
  summarise(wt_missing = sum(is.na(BMXWT)))

## sorting of variables names alphabetically

sort(names(nhanes_2015_2016))
sort(names(nhanes_2017_20))


## appending  the two datasets 

nhanes_two_waves <- bind_rows(nhanes_2017_20, nhanes_2015_2016)

## number of observations in nhanes_two_waves with missing value for SMQ935
nhanes_two_waves %>% 
  summarise(
    SMQ935_missing = sum(is.na(SMQ935))
  )

## number of missing values of SMQ935 in nhanes_two_waves are from nhanes_2015_2016
##  (SDDSRVYR = 9)

nhanes_two_waves %>% 
  summarise(
    SDDSRVYR_missing = sum(is.na(SDDSRVYR))
  )


## number of missing values of SMQ935 that are from nhanes_2015_2016

nhanes_2015_2016 %>% 
  summarise(
    SMQ935_misiing = sum(is.na(SMQ935))
  )































