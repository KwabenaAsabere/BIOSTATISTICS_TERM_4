# Lecture 8, Multiple Poisson Regression


### Pennsylvania LC Cases

# need to load the library "SpatialEpi"
library(SpatialEpi)

# load data
    data(pennLC)
    names(pennLC)
# created data frame of pennLC lung cancer counts data
    plc<-pennLC$data
    names(plc)

# poisson regression, sex
    class(plc$gender)
    table(plc$gender)


    # before doing Poisson regression, if if population is 0, need to "tweak" it to be 1
    summary(plc$population)
    plc<-plc%>%mutate(population=ifelse(population==0,1,population))

    # poisson regression with gender as predictor (compares males to females because of ordering in gender factor variable)
    spr1 <- glm(cases ~ gender+offset(log(population)), family="poisson"(link="log"), data=plc)
    summary(spr1)

# Poisson regression with age as a predictor

    # poisson regression, age which is categorical in these data, and stored as a factor 
    class(plc$age)
    table(plc$age)
    spr2 <- glm(cases ~ age+offset(log(population)), family="poisson"(link="log"), data=plc)
   
     # if you have not installed lmtest from the CRAN site you will first need to do this: can use "Tools" menu or "install.packages" command

    library("lmtest")
    # compare model with age categories to null model (model with just an intercept and no xs)
    #  first get intercpt model
    intonly<- glm(cases ~ offset(log(population)), family="poisson"(link="log"), data=plc)
    lrtest(intonly,spr2)

      # want to recreate age so that < 40 is the reference
      plc<-plc%>%mutate(agecat=ifelse(as.numeric(age)==4,1,as.numeric(plc$age)+1))
      spr2 <- glm(cases ~ as.factor(agecat)+offset(log(population)), family="poisson"(link="log"), data=plc)
      summary(spr2)
      exp(coef(spr2))
      exp(confint(spr2))
      lrtest(spr2)

# poisson regression with race as predictor
      spr3 <- glm(cases ~ race+offset(log(population)), family="poisson"(link="log"), data=plc)
      summary(spr3)
      lrtest(intonly,spr3)
# sex and age incident rate components
    
    plc %>%
      group_by(gender,agecat) %>% 
      summarise(atrisks=sum(population))
    plc %>%
      group_by(gender) %>% 
      summarise(denom=sum(population))

# regression of lc on sex and age

    mpr2 <- glm(cases ~ agecat+gender+offset(log(population)), family="poisson"(link="log"), data=plc)
    summary(mpr2)
    # test model with age and sex versus model with just age
    lrtest(spr2, mpr2)

# regression of lc on sex and age, and race
    mpr3 <- glm(cases ~ agecat+gender+race+offset(log(population)), family="poisson"(link="log"), data=plc)
    summary(mpr3)
    # test model with race, age and sex versus model with age and sex
    lrtest(mpr2, mpr3)

# just sex and race
    mpr4 <- glm(cases ~ gender+race+offset(log(population)), family="poisson"(link="log"), data=plc)
    summary(mpr4)

# just age and race
    mpr5 <- glm(cases ~ agecat+race+offset(log(population)), family="poisson"(link="log"), data=plc)
    summary(mpr5)

########################## UMARU relapse date

# set working directory to where data are stored

  umaru<-read.csv("umaru time to relapse.csv", header=T)

# what columns are in these data?  How many rows and columns are in the dataframe?
    names(umaru)
    class(umaru$treat)
    dim(umaru)

# How many persons were randomized to each treatment grouo?
    table(umaru$treat)

# types of data for the time and relapse values
    class(umaru$time)
    class(umaru$relapse)

# How mant relapses in each treatment group

# to get incidence rates of relapse by treatment group (long-term or short-term), and the IRR
# total number of relapses by treatment group

  umaru%>%group_by(treat) %>% 
    summarise(irday=sum(relapse)/sum(time))
  
  irs<-umaru%>%group_by(treat) %>% 
    summarise(irday=sum(relapse)/sum(time))
  irr<-irs[1,2]/irs[2,2]

# Poisson 1, treatment
    spru1 <- glm(relapse ~ treat+offset(log(time)), family="poisson"(link="log"), data=umaru)
    summary(spru1) 
    class(umaru$age)

# categorize age into quartiles

library("gtools")
umaru<-umaru%>%mutate(ageq=quantcut(age, q=c(0,0.25,.5,.75,1)))
# Poisson 2, age quart
# intercept only for lrt 
    # some people are missing age, no other variables of interest have missing values
    # for moving forward, let's work off a dataset that only include observations with non-missing age
    
    umaru1<-umaru%>%filter(!is.na(age))
    
    #need to create intercept only model for testing age categories model
    
    intonly<- glm(relapse ~ offset(log(time)), family="poisson"(link="log"), data=umaru1)
    spru2<- glm(relapse ~ ageq+offset(log(time)), family="poisson"(link="log"), data=umaru1)
    
    summary(spru2)

    names(spru2)
    lrtest(intonly,spru2)

# poisson 3, race

# rcreate new indicator of white
      umaru<-umaru%>%mutate(white=ifelse(as.numeric(race)==2, 0, 1))
      spru3 <- glm(relapse ~ white+offset(log(time)), family="poisson"(link="log"), data=umaru1)
      summary(spru3)


      # type of drug use
      table(umaru1$hercoc)
      table(as.numeric(umaru1$hercoc))
      # 2 = cocaine, 3 = heroin, 4 = h and c, 5 = neither, and some are missing
      umaru1<-umaru1%>%mutate(hercoc2=ifelse(as.numeric(hercoc)==1, NA, hercoc))
      # recode neither as the reference
      umaru1<-umaru1%>%mutate(hercoc2=ifelse(as.numeric(hercoc)==5, 1, hercoc))

      spru4<-glm(relapse ~ as.factor(hercoc2)+offset(log(time)), family="poisson"(link="log"), data=umaru1)
      summary(spru4)
      lrtest(intonly,spru4)

### multiple poisson

    mpru1 <- glm(relapse ~ treat+ageq+white+as.factor(hercoc2)+offset(log(time)), family="poisson"(link="log"), data=umaru1)
    summary(mpru1)
    exp(coef(mpru1))
    exp(confint(mpru1))

# test age in multiple poisson model from above
    null1 <- glm(relapse ~ treat+white+as.factor(hercoc2)+offset(log(time)), family="poisson"(link="log"), data=subset(umaru,!is.na(age)))
    lrtest(null1,mpru1)

# test type of drug
    null2 <- glm(relapse ~ treat+white+ageq+offset(log(time)), family="poisson"(link="log"), data=subset(umaru,!is.na(age)))
    lrtest(null2,mpru1)
