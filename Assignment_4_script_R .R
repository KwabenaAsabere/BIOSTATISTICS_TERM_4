# Load necessary packages
library(tidyverse)

# change working directory
  setwd("/Users/johnmcgready/Dropbox/JMCGREADY/bio 624 2024/Assignments/Assignment 4/data")
# Read in the data set, which is repeated measures data in "wide formaT"
  opth_data <- read_csv("opth_data_cplus.csv")

# Look at the structure of the data
# Notice everything is read in as a numeric variable (a double)
  str(opth_data)

# Recode the eye, group, sex, evrsmk, and hypstat variables as factor
# variables to reflect that they are categorical variables with numbers that
# correspond to categories.
  opth_data <- opth_data %>%
    mutate(
      eye = factor(eye, levels = c(1,2), labels = c("OD-right", "OS-left")),
      group = factor(group, levels = c(0,1), labels = c("no trt", "laser trt")),
      sex = factor(sex, levels = c(1,2), labels = c("female", "male")),
      evrsmk = factor(evrsmk, levels = c(0,1), labels = c("never smoked", "ever smoked")),
      hypstat  = factor(hypstat, levels = c(0,1), labels = c("normal", "hypertensive"))
    )

# Look at the structure after recoding
# Notice now these variables aare coded as factors 
# and you can see the different levels (categories) of the variables
  str(opth_data)

# Look at a basic summary of the dataset.
# Confirm that we have 156 right eye records and 156 left-eye records.
# Confirm that we have 156 eyes that received treatment and 156 eyes that were controls.
# Answer: How many males? How many ever smokers? How many with hypertension? Mean age?
  summary(opth_data)

# Let's check that we have 156 patients represented by counting the number of unique IDs
  length(unique(opth_data$id))
# Let's confirm that number of unique IDs per eye as well
  opth_data %>%
    group_by(eye) %>%
    summarize(num_patients = length(unique(id)))
  
  # Let's look at the data again
# Consider the first 10 rows of the data
# What does each column that starts with va (viusal acuity) represent? A va measurement at a different time point.
# Because we have repeated measurements on the same eyes over time, we call this "longitudinal data"
    head(opth_data, n = 10)

# Look at the participant with id = 1006 (the 5th row of data).
# This individual had a baseline VA measurement, but then the follow-up measurements are missing
# Look at the participant with id = 1008 (the 7th row of data).
# When was this participant lost to follow-up? Sometime between month 24 and month 30 after treatment.

# Suppose we wanted to compare VA at baseline between the treatment and control groups.
# We would need to consider the va0 variable to do so.
# Does it look like there is a difference in VA at baseline?
# Notice there is no missing data for the baseline measurement!
    opth_data %>%
      group_by(group) %>%
      summarize(n = sum(!is.na(va0)),
                mean_va_base = mean(va0), 
                med_va_base = median(va0))

# How would we formally test whether there is a different in VA at baseline?
# Are the treatment and control groups significantly different in VA at baseline?
  t.test(va0 ~ group, data = opth_data)

# Suppose we wanted to compare the change in VA from baseline to 3 months post treatment
# between the treatment and control groups?
# We would need to subtract the baseline value (va0) from the 3-month value (va3)
# We can create a new variable, va_change_0_3
    opth_data <- opth_data %>%
      mutate(va_change_0_3 = va3 - va0)

# Then we can compare treatment to control as before
  opth_data %>%
    group_by(group) %>%
    summarize(n = sum(!is.na(va_change_0_3)),
              mean_va_change = mean(va_change_0_3), 
              med_va_change = median(va_change_0_3))

# Why are we getting NA values here?
# You can see we only have 154 non-missing values, meaning two patients are missing 3-month VA
# How do we get means/medians with missing values?
    opth_data %>%
      group_by(group) %>%
      summarize(n = sum(!is.na(va_change_0_3)),
                mean_va_change = mean(va_change_0_3, na.rm = TRUE), 
                med_va_change = median(va_change_0_3, na.rm = TRUE))

# And again we could formally test for a difference
# Are the treatment and control groups significantly different in 
# change in VA from baseline to 3 months?
  t.test(va_change_0_3 ~ group, data = opth_data)

# Suppose we wanted to compare the average VA post-treatment between the treatment and control groups?
# We would need to combine the information in the va3 - va48 variables by taking a mean
# across those variable columns.
# We can calculate this using the rowMeans() command to create a new variable post_trt_mean_va
# Notice we can specify which columns to combine within the across() function
# Since va3 through va48 are all next to each other, we can use first_column:last_column notation 
# to specify the columns we want instead of listing them all out
  opth_data <- opth_data %>%
    mutate(post_trt_mean_va = rowMeans(across(va3:va48)))
  
  head(opth_data, n = 10) %>% print(width = Inf)

# If the columns we wanted weren't next to each other, we could use other methods
# Using the num_range() command we say we want all the columns that start with "va" and then have a number in the 1 to 48 range
# This assures we don't get the baseline va0 measurement
  opth_data <- opth_data %>%
    mutate(post_trt_mean_va = rowMeans(across(num_range(prefix = "va", range = 1:48))))
  
  head(opth_data, n = 10) %>% print(width = Inf)

# One thing you might notice is that anytime any one of the post-treatment VA measurements is missing
# we end up with a missing post_trt_mean_va value as well.
# Sometimes that is what we would want, but in this case we want an average of all the values that are there, 
# ignoring missing values.
# How do you think we might fix this?
  opth_data <- opth_data %>%
    rename(post_trt_mean_va_MISS = post_trt_mean_va) %>%
    mutate(post_trt_mean_va = rowMeans(across(num_range(prefix = "va", range = 1:48)), na.rm = TRUE))
  
  head(opth_data, n = 10) %>% print(width = Inf)

# You might notice id = 1006 (5th row of data) now has a NaN for their post_trt_mean_va variable.
# Why is this?
# What value do you think they should have? How could we fix this?
  opth_data <- opth_data %>%
    mutate(post_trt_mean_va = rowMeans(across(num_range(prefix = "va", range = 1:48)), na.rm = TRUE)) %>%
    mutate(post_trt_mean_va = if_else(is.finite(post_trt_mean_va), post_trt_mean_va, NA_real_))
  
  head(opth_data, n = 10) %>% print(width = Inf)

### Reshaping Data to Long Format
  
# So far we have been combining various columns of VA data to get the summary information we want
# But really each of the VA columns is the same variable -- visual acuity
# And a second variable, time, is missing from our dataset since it is contained in the VA column names
# but not as a variable of it's own
# We can fix this by transforming our "wide" dataset into "long" format
# We do this by reshaping the data to have a single visual acuity variable and a new time variable
# We can move from "wide" data to "long" data using the pivot_longer() function
# Note there's a pivot_wider() function that will go the other way, but it's more common to want to put
# your data into long format

# Here we will create a new dataset called opth_data_long that contains the long version of this data
# We have to give the columns (cols) that make up the variables we want to reshape; 
# notice we want to include the baseline 0 column this time.
# We also give the name of the new variable that will contain the time values that are pulled from the 
# va column names (names_to).
# Then we give the name of the new variable that will contain the visual acuity values 
# at the different time points (values_to)
    
    opth_data_long <- opth_data %>%
      pivot_longer(cols = num_range(prefix = "va", range = 0:48),
                   names_to = "time",
                   values_to = "va")

# Take  a look at our reshaped data!
# See that instead of all the va0, va3, etc columns now we have a single va column and a time column.
# Also notice the id column is now repeated multiple times!
# So the baseline VA for id 1002 is in row 1, the 3-month VA for id 1002 is in row 2, and so on.
# You should also notice that the characteristics that don't vary over time (group, sex, age, evrsmk, hypstat)
# have the same value for each row of id 1002's data.  The only variable that changes in the variable that changes
# over time, which is the VA variable
    head(opth_data_long, n = 10) %>% print(width = Inf)

# We can do a little better by making the time variable numeric instead of having the va prefix on each value of time
# We can do this within out pivot_longer() function by specifying a prefix in the original column names
# that should be stripped when creating the new variable out of the names
    opth_data_long <- opth_data %>%
      pivot_longer(cols = num_range(prefix = "va", range = 0:48),
                   names_to = "time",
                   names_prefix = "va",
                   values_to = "va")

# Now notice that time is a numeric variable that gives the number of months of the VA measurement
    head(opth_data_long, n = 10) %>% print(width = Inf)

# Remember id = 1006, who had missing data from 3 months on?
# How do their values look in this new long dataset?
      opth_data_long %>% filter(id == 1006)

# We see they have va values only at time 0 for both the right and left eye.
# Do we really need to keep the additional rows of data with the VA value is NA?
# We can automatically remove these rows of data when we create the long data set with the values_drop_na = TRUE option
    opth_data_long <- opth_data %>%
      pivot_longer(cols = num_range(prefix = "va", range = 0:48),
                   names_to = "time",
                   names_prefix = "va",
                   values_to = "va",
                   values_drop_na = TRUE)

# Now notice that individual with id = 1006 only has two rows of data
    opth_data_long %>% filter(id == 1006)

# We have successfully created a long version of this longitudinal dataset.
# Long data is the prefered way to work with longitudinal data because it gives each variable (time, va)
# it's own column instead of having time given in the names of the other variables
# Data in this form is called "tidy data"
# It means that we can create graphs and fit regression models that include time as a variable!

# Now that we have our data in long format, we may want to create an id for the specific eye 
# rather than just for the individual.
# Now that each eye has more than one row in the dataset, we may want to keep tract of which row of
# data corresponds to which participant and eye
# A meaningful id for eye would give us information both about the individual it's associate
# and about whether it's the left or right eye
# For example, the right eye for patient 1002 could be given the id 10021 and the left eye 
# for patient 1002 could be given the id 10022.
# We can create this new id variable as follows:
    opth_data_long <- opth_data_long %>%
      mutate(eye_id = if_else(eye == "OD-right", paste0(id, 1), paste0(id, 2)))

# Check the data to see that the eye IDs were created correctly
    head(opth_data_long, n = 20) %>% print(width = Inf)

# We can also move the eye IDs to the beginning of the dataset
  opth_data_long <- opth_data_long %>%
    select(id, eye_id, everything())
  # See what that did
  opth_data_long

# What kind of variable is this new eye_id?
# How could you change it from type "string" to type "numeric"?
    opth_data_long <- opth_data_long %>%
      mutate(eye_id = as.numeric(eye_id))


# Check that it worked
    opth_data_long
    
    save(opth_data_long, "opth_data_long_all_records.Rdata")

# Finally, we can do calculations now within groups defined by an id variable, 
# so either within participants (using id) or within eyes (using eye_id)
# We do this by first grouping by the id and then the newly calculated variable
# will be computed within that defined group.
# Don't forget to ungroup after the calculation!
# If we want to add a variable that gives the VA at baseline, we could do:
    opth_data_long <- opth_data_long %>%
      group_by(eye_id) %>%
      mutate(va_baseline = va[time == 0]) %>%
      ungroup()

# Why is this value unchanging in the first 10 rows of the dataset?
    head(opth_data_long, n = 10)  %>% print(width = Inf)

# We could also add a variable giving the change in VA from baseline to 3 months, 
# as we did before using the wide data
    opth_data_long <- opth_data_long %>%
      group_by(eye_id) %>%
      mutate(va_change_0_3_v2 = va[time == 3] - va[time == 0]) %>%
      ungroup()

# Did you get an error? 
# Was if for eye_id = 10061, corresponding to patient id = 1006?
# Do you remember why this happened before?  This individual doesn't have a 3-month measurement 
# so we can't do the subtraction!
# We could fix this instead by first checking that there is a time = 3 for each individual and if there
# isn't then this VA change gets a value of NA
    opth_data_long <- opth_data_long %>%
      group_by(eye_id) %>%
      mutate(va_change_0_3_v2 = ifelse(any(time == 3),
                                              va[time == 3] - va[time == 0],
                                              NA)) %>%
      ungroup()

# Check that this worked for patient id = 1006.  It did!
  opth_data_long %>% filter(id == 1006) %>% print(width = Inf)

# Check these match our VA change from working with the wide data
# There are no rows where the two calculations don't match!
    sum(opth_data_long$va_change_0_3 != opth_data_long$va_change_0_3_v2, na.rm = TRUE)

# Finally, we can calculate the mean VA post treatment as we did in our wide format
# But here combining information across various time points isn't combining across variables, 
# it's just combining information within a single variable
    opth_data_long <- opth_data_long %>%
      group_by(eye_id) %>%
      mutate(post_trt_mean_va_v2 = mean(va[time != 0])) %>%
      ungroup()

# Check that this worked for patient id = 1006.  We've got that NaN value again!
# We can fix that as before
    opth_data_long %>% filter(id == 1006) %>% print(width = Inf)
    
    opth_data_long <- opth_data_long %>%
      group_by(eye_id) %>%
      mutate(post_trt_mean_va_v2 = mean(va[time != 0])) %>%
      mutate(post_trt_mean_va_v2 = if_else(is.finite(post_trt_mean_va), post_trt_mean_va_v2, NA_real_))
      ungroup()
      
# And check it!
opth_data_long %>% filter(id == 1006) %>% print(width = Inf)

# And again we can check these match our calculation from working with the wide data
# There are no rows where the two calculations don't match!
  sum(opth_data_long$post_trt_mean_va != opth_data_long$post_trt_mean_va_v2, na.rm = TRUE)
  
 # We can save both our original and long version of this data
  save(opth_data, opth_data_long, file = "opth_exercise.Rdata")




