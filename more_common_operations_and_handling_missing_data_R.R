# load tidyverse
library(tidyverse)

# set working directory and import the .csv data into R and store as a data frame object
# example, setting my working directory: setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio 624 2023/Videos/R/Common Operations in R/data")
 
  # what files are the working directory?
   dir()

# read in data to dataframe (I name the dataframe "df" but you may name it whatever you wish)
   
   df<-read_csv("missing_data_example.csv")

   # basic overview of df and its contents
   str(df)
   head(df)
   dim(df)

# more detailed look at each column 
  
##### facility id
    
      # number of observations missing facility id
   
          # base R pulling columns with $ operator
          table(is.na(df$permanent_facility_id))
          # or #
          table(!is.na(df$permanent_facility_id))
     
          # tidyverse
         
          temp<-df%>%filter(is.na(permanent_facility_id))
          dim(temp)
          rm(temp)
          
          dim(df%>%filter(is.na(permanent_facility_id)))
        
       # number of unique facilities
      
           # base R approach
            length(unique(df$permanent_facility_id))
            
           # tidyverse
           temp<-df%>%group_by(permanent_facility_id) %>% filter(row_number(permanent_facility_id) == 1)
           dim(temp)
           
####### facility name
           
      table(is.na(df$facility_name))
      
      length(unique(df$facility_name))
      

##### sex
      
      table(is.na(df$sex))
      table(df$sex)
      class(df$sex)
      
      df <-df %>%
        mutate(sex = as.factor(sex)) 
      table(df$sex)
      class(df$sex)
      table(as.numeric(df$sex))
  
##### length of stay
      
      table(is.na(df$length_of_stay))
      class(df$length_of_stay)
      summary(df$length_of_stay)
      
        # what about the -99s?
           
             # base R approach
              summary(df$length_of_stay[df$length_of_stay <0])
              length(df$length_of_stay[df$length_of_stay <0 & !is.na(df$length_of_stay)])
              
              # tidyverse
                 temp1<-df%>%filter(length_of_stay <0)%>%select(length_of_stay)
                 summary(temp1)
                 dim(temp1)
                 rm(temp1)
                 
                   #or #
                 df%>%filter(length_of_stay <0)
                
         # replace -99 with missing (NA in R)
                
                # base R
                  df$length_of_stay<-if_else(df$length_of_stay <0, NA, df$length_of_stay)
                  
                # tidyverse
                 df<-df%>%mutate(length_of_stay=if_else(length_of_stay <0, NA, length_of_stay))
                                    
######## total costs
            table(is.na(df$total_costs))
            class(df$total_costs)
            summary(df$total_costs)
            
            # missing value issues in certain functions
            mean(df$total_costs)
            mean(df$total_costs,na.rm=T)
            sd(df$total_costs)
            sd(df$total_costs,na.rm=T)
            
            # this persists when these functions are used in tideverse summarise coond
            df%>%summarise(meantc = mean(total_costs), sdtc= sd(total_costs))
            df%>%summarise(meantc = mean(total_costs, na.rm = T), sdtc= sd(total_costs, na.rm=T))
            
# collapsing discrete continuous variable into smalller number of categories
  
    #suppose we want to  dichotomize length of stay into short (1-2 days) and long (> 2 days)
            
            df<-df%>%mutate(loslong2=factor(if_else(length_of_stay>2,1,0),levels=c(0,1), labels =c("1 -2 days",">2 days")))
            
  #suppose we want to categorize length of stay
  
    # let's for 4 groups : 1 day, 2 days, 3-4 days, > 4 days
    		 
          df<-df%>%mutate(loscat=case_when(
            length_of_stay==1 ~ 1,
            length_of_stay==2  ~2,
            length_of_stay %in% 3:4~ 3,
            length_of_stay >4 ~4))
          
          table(df$loscat)
          class(df$loscat)
          df <-df %>%
            mutate(loscat = factor(loscat,levels=c(1,2,3,4), labels =c("1 day","2 days", "3 - 4 days", "More than 4 days")))
             
          table(df$loscat)
          class(df$loscat)
     
			 
