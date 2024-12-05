library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)

smoke_data <- read_csv("smoking.csv")


# check summaries of key variables in dataset -----------------------------

##age variable
smoke_data |> 
  count(age)

##smoke variable
smoke_data |> 
  count(smoke)

##pop variable
summary(smoke_data$pop)

##deaths variable
summary(smoke_data$deaths)

dependent <- "deaths"
explanatory <- c("age","smoke","pop")
smoke_data |> 
  summary_factorlist(dependent,explanatory,p=TRUE,add_dependent_label = TRUE)

ff_glimpse(smoke_data)



## whole data
smoke_data |> 
  print(n=Inf)



## convert the covariate smoke to a factor variable and make never the reference level
smoke_data <- smoke_data |> 
  mutate(smoke = as.factor(smoke)) |>   
  mutate(smoke = fct_relevel(smoke, "Never"))

## fitting poisson regression model
model_1 <- smoke_data |> 
  glm(deaths ~ age + smoke, 
      family = poisson(link = "log"),
      offset = log(pop),
      data = _)
summary(model_1)
confint.default(model_1)
exp(coef(model_1))



tidy(model_1)
tidy(confint.default(model_1))
tidy(exp(coef(model_1)))

##we can interpret the estimated coefficients for the indicator variables of
##smoke as the log incidence rate ratio of death in age group x 
## as compared to never smokers, adjusted for age.


##4. Use the glht() function to estimate adjusted incidence rate ratios
## for linear combinations of indicator variables

##The glht() function in the multcomp package can be used for comparing groups
##(other than the default comparison to the reference group (e.g. the lowest group
##If we were
##interested in comparing each age group x+1 to age group x 
##(adjusting for smoking), we could specify the following:

library(multcomp)

names(coef(model_1))
##pairwise comparisons

exp(confint(glht(model_1, "`age45-49` = 0"))$confint)

exp(confint(glht(model_1, "`age50-54` - `age45-49` = 0"))$confint)

##


dependent <- "deaths"
explanatory <- c("age","smoke","offset(log(pop))")

smoke_data |> 
finalfit(dependent,explanatory)



smoke_data |> 
  ff_plot(dependent,explanatory)

smoke_data |> 
  group_by(age) |> 
  summarise(Total_deaths = sum(deaths))


smoke_data |> 
  group_by(smoke) |> 
  summarise(Total_deaths = sum(deaths))

smoke_data |> 
  group_by(age) |> 
  summarise(Total_deaths = sum(deaths),
            death_rate = sum(deaths)/sum(pop))

smoke_data |> 
  group_by(smoke) |> 
  summarise(Total_deaths = sum(deaths),
            death_rate = sum(deaths)/sum(pop))

smoke_data |> 
  group_by(age,smoke) |> 
  summarise(Total_deaths = sum(deaths),
            death_rate = sum(deaths)/sum(pop)) 

table <- smoke_data |> 
  group_by(age,smoke) |> 
  summarise(Total_deaths = sum(deaths),
            death_rate = sum(deaths)/sum(pop))  

table |> 
  ggplot(aes(x=age,y=death_rate)) +
  geom_point()+
  geom_line(aes(x))

model_A <- smoke_data |> 
  glm(deaths~ NULL,
      family = poisson(link="log"),
      offset=log(pop),
      data=_)
summary(model_A)
summary(model_A)
exp(coef(model_A))
tidy(confint.default(model_A))



model_B <- smoke_data |> 
  glm(deaths~age ,
      family = poisson(link="log"),
      offset=log(pop),
      data=_)
summary(model_B)
exp(coef(model_B))
tidy(exp(coef(model_B)))
tidy(confint.default(model_B))

model_C <-  smoke_data |> 
  glm(deaths~smoke ,
      family = poisson(link="log"),
      offset=log(pop),
      data=_)
summary(model_C)
exp(coef(model_B))
tidy(exp(coef(model_C)))
tidy(confint.default(model_C))

exp(confint.default(model_C))

model_D <-  smoke_data |> 
  glm(deaths~ age +smoke ,
      family = poisson(link="log"),
      offset=log(pop),
      data=_)
summary(model_D)
exp(coef(model_D))
tidy(exp(coef(model_D)))
tidy(confint.default(model_D))

predict(model_D)
tidy(predict(model_D))

aug_model_D <- augment(model_D)
aug_model_D <- aug_model_D |> 
  
  
  
  
  x <- exp(coef(model_E))
  
 y <- smoke_data$age





model_E <-  smoke_data |> 
  glm(deaths~ age * smoke ,
      family = poisson(link="log"),
      offset=log(pop),
      data=_)
summary(model_E)
exp(coef(model_E))
tidy(exp(coef(model_E)))
tidy(confint.default(model_E))

anova(model_D,model_E,test = "Chisq")

anova(model_E,model_D,test = "Chisq")









 
 
 