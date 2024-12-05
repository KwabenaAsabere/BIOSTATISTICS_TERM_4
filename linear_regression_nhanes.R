# Load necessary packages
# You may need to install some of the following packages before loading them into the session library, if you do not
# already have them installed. If you do not have a package installed, you will get an error when using the library() 
# command that will look like "Error in library(): there is no package called ..."

# Example code for installing packages: install.packages("multcomp"), install.packages(c("multcomp", "gtsummary"))
# Alternative example code that checks for (and installs if necessary) packages and loads them into the session library:
# pckgs <- c("tidyverse","lubridate","haven","jtools","multcomp",
#           "expss","table1","car","gtsummary","simpleboot","olsrr") #create vector of package names
#sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) { 
#  install.packages(x)
#  require(x, character.only=TRUE) 
#}) #checks each element in pckgs to see if installed or not and loads all packages 


library(tidyverse) # general functions for working with data
library(lubridate) # package for working with dates
library(jtools)  # includes summ() functions for better aesthetic summaries from regression models
library(multcomp) # includes glht command for computing linear combinations (and CIs) from regression
library(expss) # includes tool for labeling variable names with descriptive text
library(table1) # includes tools for easily making table of summary statistics for specific subgroup comparisons
library(car) # for avPlots() function which constructs added-variables plots for linear models 
library(gtsummary) # great tools for table making, including regression results tables

###################################################
# Video 1
# Getting Data and doing some inital cleaning and organization
     
    # Get datafiles from the NHANES site and assign to dataframe

        # set working directory
      setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio 624 2024/Videos/R/Linear Regression NHANES/data")
  
       # read in NHANES data from csv file, store in data frame object
        my_data<-read_csv("NHANES data 2017_20.csv")
        
        str(my_data)
        
    # Some basic summaries to better understand key variable and their distributions: base R approach with summary command
      
        
        # BMI
        summary(my_data$BMXBMI) # gives minimum, first quartile, median, mean, third quartile, maximum, and number of NAs
 
        # HH income to poverty ratio
        summary(my_data$INDFMPIR)
        
        # SBP
        summary(my_data$BPXSY1) 
        summary(my_data$BPXSY2)
        summary(my_data$BPXSY3)
        # LDL
        summary(my_data$LBDLDL)
        
    # could also use a tidyverse approach with summarise
        my_data%>%summarise(across(c("BMXBMI","INDFMPIR","LBDLDL"), ~ mean(.x, na.rm = TRUE) ))
        my_data%>%summarise(across(c("BMXBMI","INDFMPIR","LBDLDL"), ~ sd(.x, na.rm = TRUE) ))
        
    #. let's look at missing values for outcome of interest, LBDLDL
        
        table(is.na(my_data$LBDLDL)) # number of missing values where TRUE is missing and FALSE is not missing
        table(is.na(my_data$LBDLDL[my_data$RIDAGEYR>=18])) # number of missing values for those >=18 years old
    
    
    # Subset those who are greater than 18 years : flag those who have ldl, age and household income for subsetting later
    # Create factor versions of sex, race/ethnicity, household incme and add labels
      
      rdf<-my_data%>%filter(RIDAGEYR>=18)%>% # subset those who are greater than 18 years
        
                     dplyr::select(SEQN,RIAGENDR,RIDAGEYR,DMDHHSIZ,BMXBMI,BPXSY1,BPXSY2,BPXSY3,LBDLDL,INDFMPIR,INDHHIN2,RIDRETH3)%>% #select certain variables
        
                    # create composite mean of all measured SBP values and name this composite mean variable SBPc
                   rowwise%>%mutate(SBPc = mean(c(BPXSY1, BPXSY2, BPXSY3),na.rm = T))%>%
                   # create a flag for inclusion in regression dataset
                  # for the regression analyses we will only include "complete case" records in terms of the ldl, SBP, hhin and bmi variables - no missing values in any variable
                    
                 mutate(flag =factor(as.numeric(!is.na(LBDLDL) & !is.na(SBPc) & !is.na(INDHHIN2) &!is.na(BMXBMI) & 
                                                      !is.na(RIDAGEYR) ), levels = c(0,1),labels=c("Not in Regression Sample", "In Regression Sample")))   %>%
                   # create factor versions of sex, household income, and race ethncity
                    
                    mutate(sex=factor(RIAGENDR, levels=c(1,2),labels = c("Male","Female")),
                      raceeth= factor(RIDRETH3, levels=c(1,2,3,4,6,7), labels=c("Mexican American","Other Hispanic", "Non-Hispanic White","Non-Hispanic Black",
                                                                                  "Non-Hispanic Asian", "Other Race - Including Multi-Racial")),
                      hhin=factor(INDHHIN2, levels=c(1:10,12:15,77,99),labels= c("$ 0 to $ 4,999","$ 5,000 to $ 9,999","$10,000 to $14,999","15,000 to $19,999",
                                                                            "$20,000 to $24,999","25,000 to $34,999","$35,000 to $44,999","$45,000 to $54,999",
                                                                            "$55,000 to $64,999","$65,000 to $74,999","$20,000 and Over","Under $20,000	",
                                                                        "$75,000 to $99,999","$100,000 and Over	","Refuse","Dont Know")))
    
    
     # Give descriptive text to accompany variable names 	
    rdf <- apply_labels(rdf,
                       SEQN = "respondent id",
                       RIDAGEYR = "Age (years)",
                       BMXBMI = "Body Mass Index",
                       SBPc = "Systolic Blood Pressure (mmHg)",
                       LBDLDL = "LDL Cholesterol (mg/dL)",
                       sex = "Sex",
                       raceeth="Race/Ethnicity",
                       hhin="Household Income")
    
      dim(rdf)
      str(rdf)
      
      
####################################################################################
# Video 2 - making comparison tables with the table1 command
  
  # Table 0: compare characteristics by inclusion in regression dataset status (flag)
  
    
    # Function that computes p-values corresponding to comparisons being made in table
    # This was taken directly from https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
    # See if you can figure out what this function does, exactly: for those interested in parsing the components, we
    # can discuss after any  class session
    
    pvalue <- function(x, ...) {
      # Construct vectors of data y, and groups (strata) g
      y <- unlist(x)
      g <- factor(rep(1:length(x), times=sapply(x, length)))
      if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test and extract p-value
        p <- t.test(y ~ g)$p.value
      } else {
        # For categorical variables, perform a chi-squared test of independence and extract p-value
        p <- chisq.test(table(y, g))$p.value
      }
      # Format the p-value, using an HTML entity for the less-than sign.
      # The initial empty string places the output on the line below the variable label.
      c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))) # rounds pvalues 
      
    }
  
  # Table 0: comparing characteristics of those in regression sample versus those not in sample. ("Table 0")
      # Recall that the ids in the regression samlpe were those that had complete cases of the variables so no missing values
      # The following computes summary statistics of the for each group and computes a pvalue using the function we made to 
      # test if there is a difference among groups
      
    table1(~ LBDLDL+RIDAGEYR+ SBPc+BMXBMI+sex+ raceeth| flag, data=rdf, overall=F,extra.col=list(`P-value`=pvalue)) 
    table1(~ hhin| flag, data=rdf, overall=F,extra.col=list(`P-value`=pvalue))

    ##### Subsetting the data to get the analytic sample, and creating a "Table 1" comparing characteric distributions between those
    ### with lower and higher BMIs
    
    # the flag variable needed to be a factor variable to be able to use to stratify the above tables into the two groups 
    # however, now can create dataframe with just those observations that will be included in the regression sample
    
      table(rdf$flag) # number of participants in each group
      table(as.numeric(rdf$flag))
      rdf2<-rdf%>%filter(as.numeric(flag)==2) # subset the dataframe to just those in the regression sample
    
    # Table: comparing characteristics of those with lower (<27) and higher (>=27) BMI
      #creates factor variable bmicat with the two levels of lower (<27) and higher (>=27) BMI
      
      rdf2<-rdf2%>%mutate(bmicat=factor(as.numeric(BMXBMI>27), levels=c(0,1),labels = c("BMI <= 27","BMI > 27"))) 
      table1(~ LBDLDL+RIDAGEYR+ SBPc+sex+ raceeth| bmicat, data=rdf2, overall=F) #summary statistics for variables by BMI category
      table1(~ hhin| bmicat, data=rdf2, overall=F)
      
######################################################
# Video 3: Simple Linear Regression with a continuous predictor
  # Simple Linear Regressions
  
    #LDL and BMI
  
    # Loess smoothing plot
        ggplot(rdf2, aes( BMXBMI,LBDLDL)) +
          geom_point() +
          geom_smooth()+
          theme(text=element_text(size=25))+
          labs(title = "LDL Cholesterol Levels (mg/dL) by BMI ", 
               y = "LDL Cholesterol Levels (mg/dL)", 
               caption = "NHANES 2017-2018 Subsample (n=2,213)", x="BMI (kg/m2)")
        
        # Simple regression- BMI as predictor and LDL levels as the outcome
        slrbmi1<-lm(data=rdf2,LBDLDL~BMXBMI)
        summ(slrbmi1,confint=getOption("summ-confint",TRUE),digits=4)
        
        # Add spline at BMI=30 
        rdf2<-rdf2%>%mutate(bmispl30=(BMXBMI-30)*as.numeric(BMXBMI>=30))
        slrbmi2<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30)
        summ(slrbmi2,confint=getOption("summ-confint",TRUE), digits=4)
      
    #LDL and Age
        
      # Loess smoothing plot
      ggplot(rdf2, aes(RIDAGEYR, LBDLDL)) +
        geom_point() +
        geom_smooth()+
        theme(text=element_text(size=25))+
        labs(title = "LDL Cholesterol Levels (mg/dL) by Age ", 
             y = "LDL Cholesterol Levels (mg/dL)", 
             caption = "NHANES 2017-2018 Subsample (n=2,213)", x="Age Years")
      
      # Simple regression- Age as predictor and LDL levels as the outcome
        slrage1<-lm(data=rdf2,LBDLDL~RIDAGEYR)
        summ(slrage1,confint=getOption("summ-confint",TRUE),digits = 4)
        
        # Add spline at Age=50
        rdf2<-rdf2%>%mutate(agespl50=(RIDAGEYR-50)*as.numeric(RIDAGEYR>=50))
        slrage2<-lm(data=rdf2,LBDLDL~RIDAGEYR+agespl50)
        summ(slrage2,confint=getOption("summ-confint",TRUE),digits = 4)
        names(coef(slrage2)) # names of coefficients in the model 
        summary(glht(slrage2, linfct = c("RIDAGEYR +agespl50 = 0")))
        
        # To get slope after 50 years, and the corresponding 95% CI:
        names(coef(slrage2))
        summary(glht(slrage2, linfct = c("RIDAGEYR +agespl50 = 0"))) # adds the coefficient for Age and spline term to get slope for those >50
        confint((glht(slrage2, linfct = c("RIDAGEYR +agespl50 = 0")))) # computes corresponding CI
        
        # Mean LDL for a 40 year-old 
        summary(glht(slrage2, linfct = c("(Intercept)+40*RIDAGEYR  = 0")))
        confint(glht(slrage2, linfct = c("(Intercept)+40*RIDAGEYR  = 0")))
        
        # Mean LDL for a 60 year-old
        summary(glht(slrage2, linfct = c("(Intercept)+60*RIDAGEYR+10*agespl50   = 0")))
        confint(glht(slrage2, linfct = c("(Intercept)+60*RIDAGEYR+10*agespl50  = 0")))
        
        # Mean difference in LDL for 40 to 60 years of age
        summary(glht(slrage2, linfct = c("20*RIDAGEYR+10*agespl50  = 0")))
        confint(glht(slrage2, linfct = c("20*RIDAGEYR+10*agespl50 = 0")))
      
  #LDL and SBP
    # LOESS smoothing plot
    ggplot(rdf2, aes(SBPc,LBDLDL)) +
      geom_point() +
      geom_smooth()+
      theme(text=element_text(size=25))+
      labs(title = "LDL Cholesterol Levels (mg/dL) by Systolic Blood Pressure (SBP) ", 
           y = "LDL Cholesterol Levels (mg/dL)", 
           caption = "NHANES 2017-2018 Subsample (n=2,213)", x="Systolic Blod Pressue (mm/Hg)")
    
    # Simple regression 
    slrsbp<-lm(data=rdf2,LBDLDL~SBPc)
    summ(slrsbp,confint=getOption("summ-confint",TRUE), digits=4)

##########################################################################
# Video 4: Simple Linear Regression with binary/categorical predictor
    
    # Sex
    table(rdf2$RIAGENDR) # Recall level 2 is Female
    class(rdf2$RIAGENDR)
    slrsex<-lm(data=rdf2,LBDLDL~as.factor(RIAGENDR))
    summ(slrsex,confint=getOption("summ-confint",TRUE), digits=4)
    slrsex<-lm(data=rdf2,LBDLDL~sex)
    summ(slrsex,confint=getOption("summ-confint",TRUE), digits=4)          
      
      # Side by side boxplots of LDL by sex
        ggplot(rdf2, aes(y=LBDLDL, x=sex,)) + 
          geom_boxplot() +
          labs(title = "LDL Cholesterol Levels (mg/dL) by Sex ", 
               y = "LDL Cholesterol Levels (mg/dL)", 
               caption = "NHANES 2017-2018 Subsample (n=2,213)", x="Sex") +
               ylim(0,400)+
          theme_bw()+
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                plot.title = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5),
                axis.ticks.x = element_blank())
    
    # Race/Ethnicity
      table(rdf2$RIDRETH3)
      class(rdf2$raceeth)
      table(rdf2$raceeth)
      slreth<-lm(data=rdf2,LBDLDL~raceeth)
      summ(slreth,confint=getOption("summ-confint",TRUE), digits=4)
      
     # Side by side boxplots of LDL by race/ethnicity
      ggplot(rdf2, aes(y=LBDLDL,  x= raceeth,)) + 
        geom_boxplot() +
        labs(title = "LDL Cholesterol Levels (mg/dL) by Race/Ethncity ", 
             y = "LDL Cholesterol Levels (mg/dL)",x="", 
             caption = "NHANES 2017-2018 Subsample (n=2,213)") +
        ylim(0,400)+
      theme_bw()+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              plot.title = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5),
              axis.ticks.x = element_blank())
      
    # Annual Household Income
    table(rdf2$INDHHIN2)
    table(is.na(rdf2$INDHHIN2)) # no missing values 
    class(rdf2$INDHHIN2)
    table(rdf2$hhin)
    
        # Side by side boxplots of LDL by household income
      ggplot(rdf2, aes(y=LBDLDL,  x= hhin,)) + 
        geom_boxplot() +
        labs(title = "LDL Cholesterol Levels (mg/dL) by Household Income Catoegory ", 
             y = "LDL Cholesterol Levels (mg/dL)", 
             caption = "NHANES 2017-2018 Subsample (n=2,213)", x="HH Income") +
        ylim(0,400)+ theme_bw()+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              plot.title = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5),
              axis.ticks.x = element_blank())+ theme(axis.text.x = element_text(angle = 90))
      
    slrinc<-lm(data=rdf2,LBDLDL~hhin)
    summ(slrinc,confint=getOption("summ-confint",TRUE), digits=4)

    
    # Household Size - total number of people in the household
    table(rdf$DMDHHSIZ)
    class(rdf$DMDHHSIZ)
    slrhh1<-lm(data=rdf2,LBDLDL~as.factor(DMDHHSIZ))
    summ(slrhh1,confint=getOption("summ-confint",TRUE), digits=4)
    
###############################################################################
# Video 5. Using tbl_regression command to make nice regression results table
    # Summary table of unadjusted linear regressions
    # Need to cobble this together

    # Great resource for styling in gtsummary
    # http://www.danieldsjoberg.com/gtsummary-weill-cornell-presentation/#32
    
    # Variables with splines
    
    tBMI<-tbl_regression(slrbmi2,pvalue_fun = ~ style_pvalue(.x, digits = 2),list(BMXBMI ~ "BMI", bmispl30 ~ "BMI spline at 30"))%>%add_global_p()
    tage<-tbl_regression(slrage2,pvalue_fun = ~ style_pvalue(.x, digits = 2),list(RIDAGEYR ~ "Age (years)", agespl50 ~ "Age spline at 50"))%>%add_global_p()
    tBMI
    tage
    
    # Variables without splines
    tothers<-rdf2 %>%
      dplyr::select(SBPc, LBDLDL, sex,raceeth) %>%
      tbl_uvregression(
        method = glm,
        y = LBDLDL,
        #method.args = list(family = normal),
        exponentiate = FALSE,
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        hide_n = TRUE
      ) %>%
      add_global_p() # add global p-value
    tothers
    
    # Aggregating 3 tables - tBMI, tage, and tothers into one cohesive table
    tbl_unadjusted <- 
      tbl_stack(
        list(tBMI, tage, tothers),
      )
    tbl_unadjusted
    
    
#### Let's save what we have thus far as an R data file

    saveRDS(rdf2,"NHANESregression_sample.RData")
    write_csv(rdf2,"NHANESregression_sample.csv")
####################################################################
###. Video 6:  Multiple Linear Regression with R
    
    # Multiple linear regression, no splines
    mlrfull<-lm(data=rdf2,LBDLDL~BMXBMI+RIDAGEYR+SBPc+sex+raceeth+hhin)
    summ(mlrfull,confint=getOption("summ-confint",TRUE), digits=4)
    
      # Residuals vs fitted values plot
      ggplot(mlrfull, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()
      
      # Added-variable plots: avplots
      avPlots(mlrfull)
    
    
    # Multiple linear regression with splines, test categories
   mlrfull<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+RIDAGEYR+agespl50+SBPc+sex+raceeth+hhin)
    summ(mlrfull,confint=getOption("summ-confint",TRUE), digits=4)
    
    # Residuals vs. fitted values plot
    ggplot(mlrfull, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()
    
    # AIC
    extractAIC(mlrfull)

    
    # Test possible multiple linear regressions
    # Test BMI with spline
    mlrnobmi<-lm(data=rdf2,LBDLDL~RIDAGEYR+agespl50+SBPc+sex+raceeth+hhin)
    anova(mlrnobmi,mlrfull)
    # Test age with spline
    mlrnoage<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+SBPc+sex+raceeth+hhin)
    anova(mlrnoage,mlrfull)
    # Remove race/ethnicity
    mlrnoeth<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+RIDAGEYR+agespl50+SBPc+sex+hhin)
    anova(mlrnoeth,mlrfull)
    # Remove hhin
    mlrnoinc<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+RIDAGEYR+agespl50+SBPc+sex+raceeth)
    anova(mlrnoinc,mlrfull)
  
    
    # For association purposes, model has BMI, age, sbpc, sex
    fm1<-lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+RIDAGEYR+agespl50+SBPc+sex)
    summ(fm1)
    
    # Compute slopes involving spline terms after cut using the summary of main and spline terms 
    summary(glht(fm1, linfct = c("BMXBMI +bmispl30 = 0"))) # slope of BMI for those >30
    confint(glht(fm1, linfct = c("BMXBMI +bmispl30 = 0"))) # slope of BMI for those >30
    
    summary(glht(fm1, linfct = c("RIDAGEYR +agespl50 = 0"))) # slope of Age for those >50
    
    # Same model, but reparameterize spline xs so that coefficient for main BMI term is still slope before 30, but coefficient for
    # spline is slope after 30 (not change in slope from before 30 to after 30), and similarly, coefficient for main age term is still slope prior to 50, but coefficient for spline is
    # slope after 50 (not change in slope from before 50 years of age to after 50 years of age)
    
    rdf2<-rdf2%>%mutate(bmi2= BMXBMI*as.numeric(BMXBMI<=30)+30*as.numeric(BMXBMI>30), bmispl30nm =(BMXBMI-30)*as.numeric(BMXBMI>30), 
              age2=RIDAGEYR*as.numeric(RIDAGEYR<=50)+50*as.numeric(RIDAGEYR>50), agespl50nm=(RIDAGEYR-50)*as.numeric(RIDAGEYR>50))
    fm2<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm+age2+agespl50nm+SBPc+ sex)
    summ(fm2)
    confint(fm2)
    
    # Residuals vs fitted values plot
  
    ggplot(fm1, aes(x = .fitted, y= .resid )) + geom_point()+geom_smooth()+
    labs(title = "Residuals Vs. Fitted Values ", 
         y = "Residuals", , x="Fitted Values",
         caption = "LDL regressed on BMI (spline at 30), Age (spline at 50), SBP, and Sex")+
      geom_hline(yintercept=00, linetype="dashed", color = "red")
    
    # Added variable plot SBP
    
      # Compute residuals from LDL on all predictors in fm1 except SBP
      avpldlr=residuals(lm(data=rdf2,LBDLDL~BMXBMI+bmispl30+RIDAGEYR+agespl50+ sex))
      # Compute residuals from SBP on all other predictors in fm1 except SBP
      avpsbpr=residuals(lm(data=rdf2,SBPc~BMXBMI+bmispl30+RIDAGEYR+agespl50+ sex))
      
      # Put these residuals in a data frame
      avpsbp<-tibble(avpldlr,avpsbpr)
      
      # Plot the two residual vectors, and add loess
      ggplot( avpsbp, aes(x = avpsbpr, y= avpldlr )) + geom_point()+geom_smooth()+
        labs(title = "Added Variable Plot: SBP (Systolic Blood Pressure)", 
             y = "Residuals: LDL on BMI (with spline), Age (with Spline), Sex",  
             x="Residuals: SBP on BMI (with spline), Age (with Spline), Sex",
             caption = "LDL regressed on BMI (spline at 30), Age (spline at 50), SBP, and Sex")

      # Qnorm plot
      rfm1<-tibble(fm1$residuals)
      ggplot(data = rfm1, aes(sample=fm1$residuals)) + 
        geom_qq( ) +
        geom_qq_line( ) +
        labs(y = "Model Residuals", x="") 
      
      # First pass at bootstrapping regression coefficients
      library(simpleboot)
      lboot <- lm.boot(fm2, R = 1000)
      summary(lboot)

      #### let's again save what we have thus far as an R data file
      saveRDS(rdf2,"NHANESregression_sample.RData")
      
#############################################################################################
## Video 7:  Creating Tables with Unadjusted and Adjusted Associations from Linear Regression
      
      # Table of unadjusted results, with updated approach to splines 
      # Summary table of unadjusted linear regressions
      # Need to cobble this together
      
      # Great resource for styling in gtsummary
      # http://www.danieldsjoberg.com/gtsummary-weill-cornell-presentation/#32
      
      # Variables with splines, with 2nd approach to splines
      
      slrbmi3<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm)
      slrage3<-lm(data=rdf2,LBDLDL~age2+agespl50nm)
      tBMI2<-tbl_regression(slrbmi3,pvalue_fun = ~ style_pvalue(.x, digits = 2),list(bmi2~ "BMI slope, BMI <=30 ", bmispl30nm ~ "BMI slope, BMI >30"))%>%add_global_p()
      tage2<-tbl_regression(slrage3,pvalue_fun = ~ style_pvalue(.x, digits = 2),list(age2~"Age slope, age <= 50 yrs",agespl50nm~"Age slope, age > 50 yrs"))%>%add_global_p()
      tBMI2
      tage2
      
      
      # Variables without splines
      tothers<-rdf2 %>%
        dplyr::select(SBPc, LBDLDL, sex,raceeth) %>%
        tbl_uvregression(
          method = glm,
          y = LBDLDL,
          #method.args = list(family = normal),
          exponentiate = FALSE,
          pvalue_fun = ~ style_pvalue(.x, digits = 2),
          hide_n = TRUE
        ) %>%
        add_global_p() # add global p-value
      tothers
      
        # Aggregate unadjusted results into one table
        tbl_unadjusted2 <- 
          tbl_stack(
            list(tBMI2, tage2, tothers),
          )
        tbl_unadjusted2


      
    # Adjusted with splines such that the apline coefficients don't need to be added to get slope after cutpoint
       t_adjusted2<-  tbl_regression(fm2,list(bmi2~ "BMI slope, BMI <=30 ", bmispl30nm ~ "BMI slope, BMI >30",age2~"Age slope, age <= 50 yrs",agespl50nm~"Age slope, age > 50 yrs",
                                            SBPc="Systolic Blood Pressure (mmHg)", sex ="Sex"))%>%add_global_p()
      
      # Merge the unadjusted and adjusted tables together
      
      tbl_un_and_adj <- 
        tbl_merge(
          list(tbl_unadjusted2,  t_adjusted2),
          tab_spanner = c("**Unadjusted**", "**Adjusted**")
        )
      tbl_un_and_adj
      
    ## For looking at "story" with BMI upon adjustment
    
    model1<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm)
    model2<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm+age2+agespl50nm)
    model3<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm+age2+agespl50nm+SBPc)
    model4<-lm(data=rdf2,LBDLDL~bmi2+bmispl30nm+age2+agespl50nm+SBPc+ sex)
    
    t1<-  tbl_regression(model1,list(bmi2 ~ "BMI", bmispl30nm ~ "BMI spline at 30"))%>%
      add_global_p() 
    
    t2<-  tbl_regression(model2,list(bmi2 ~ "BMI", bmispl30nm ~ "BMI spline at 30",age2~"Age (years)",agespl50nm~"Age Spline at 50 years"))%>%
      add_global_p() 
    
    t3<-  tbl_regression(model3,list(bmi2 ~ "BMI", bmispl30nm ~ "BMI spline at 30",age2~"Age (years)",agespl50nm~"Age Spline at 50 years",SBPc="Systolic Blood Pressure (mmHg)"))%>%
      add_global_p() 
    
    t4<-  tbl_regression(model4,list(bmi2 ~ "BMI", bmispl30nm ~ "BMI spline at 30",age2~"Age (years)",agespl50nm~"Age Spline at 50 years",SBPc="Systolic Blood Pressure (mmHg)", sex="Female"))%>%
      add_global_p() 
    
          # Merge the four tables together
          tbl_merge_ex1 <-
            tbl_merge(
              tbls = list(t1, t2,t3,t4),
              tab_spanner = c("**Model 1**", "**Model 2**","**Model 3**", "**Model 4**"))%>%
            modify_column_hide(columns = c(p.value_1, p.value_2,p.value_3,p.value_4))
          
          # View table
          tbl_merge_ex1

    # View table
    tbl_merge_ex1

#####################################################################################
# Video 8: Exploring interaction with linear regression
### Interaction between BMI and sex?
    
    model_int<-lm(data=rdf2,LBDLDL~BMXBMI*sex+bmispl30*sex+RIDAGEYR+agespl50+SBPc)
    summ(model_int,confint=getOption("summ-confint",TRUE), digits=4)
    confint(glht(model_int, linfct = c("BMXBMI+BMXBMI:sexFemale = 0")))
    confint(glht(model_int, linfct = c("BMXBMI+BMXBMI:sexFemale+bmispl30+sexFemale:bmispl30 = 0")))
    tint<-  tbl_regression(model_int,list(BMXBMI ~ "BMI", bmispl30 ~ "BMI spline at 30", RIDAGEYR~"Age (years)",agespl50~"Age Spline at 50 years",
                                     SBPc~"Systolic Blood Pressure (mmHg)",sex~"Female",BMXBMI:sex~"BMI * Female", sex:bmispl30~"(BMI-30)+ * Female"))
    tint
    
    # Test the interaction
    anova(model_int,fm1)
    
    # show interaction result with 2nd spline approach
    model_int2<-lm(data=rdf2,LBDLDL~bmi2*sex+bmispl30nm*sex+age2+agespl50nm+SBPc)
    summ(model_int2,confint=getOption("summ-confint",TRUE), digits=4)
      # slope of bmi females, bmi < 30
      confint(glht(model_int2, linfct = c("bmi2+bmi2:sexFemale = 0")))
      # slope of BMI female, BMI >= 30
      confint(glht(model_int2, linfct = c("bmispl30nm+sexFemale:bmispl30nm = 0")))

    # Test the interaction
    anova(model_int2,fm2)
    
    ### Interaction between BMI and ethnicity
    model_int_eth<-lm(data=rdf2,LBDLDL~bmi2*raceeth+bmispl30nm*raceeth+age2+agespl50nm+SBPc+sex)
    summ(model_int_eth,confint=getOption("summ-confint",TRUE), digits=4)
    anova(fm2,model_int_eth)   
      
      # slope of BMI Non-Hispanic White, BMI < 30
      confint(glht(model_int_eth, linfct = c("bmi2+ `bmi2:raceethNon-Hispanic White` = 0")))
      # mean of BMI Non-Hispanic White, BMI < 30
      confint(glht(model_int_eth, linfct = c("(Intercept)+ `bmi2:raceethNon-Hispanic White` = 0")))
      
      
      
      