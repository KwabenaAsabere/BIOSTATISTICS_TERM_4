library(gapminder)
library(patchwork)
library(tidyverse)

gap_data <- gapminder

gap_data |> 
  filter(continent == "Africa") |> 
  select(lifeExp) |> 
  t.test(mu = 50)

my_t_test <- gap_data |> 
  filter(continent == "Africa") |> 
  select(lifeExp) |> 
  t.test(mu = 50)


## confidence interval does not include 50 making the finding statistically significant

names(my_t_test)


## two-sided t-test for difference of means

gap_data |> 
  filter(continent %in% c("Africa", "Europe")) |> 
  t.test(lifeExp ~ continent, data= _ ,
         alternative = "two.sided")

## ONE SIDED TEST FOR DIFFERENCE OF MEANS

# NULL HYPOTHESIS THAT THE NULL HYPOTHESIS IN BOTH COUNTRIES ARE THE SAME

gap_data |> 
  filter(country %in% c( "Switzerland","Ireland")) |> 
  t.test( lifeExp ~ country,data = _ ,
          alternative = "less",
          conf.level = 0.95)


# PAIRED t-test -----------------------------------------------------------

gap_data |> 
  filter(year %in% c(1957,2007) & 
           continent == "Africa") |> 
  mutate(year = factor(year,levels = c(2007,1957))) |> 
  t.test( lifeExp ~ year, data = _ ,
          paired = TRUE)

## u bring 2007 first because u want to subtract 1957 from 2007

## 

# Assumptions of t-test ---------------------------------------------------

# 1) Large representative sample
# 2) Values are normally distributed
# 3) Your two samples have similar variances





















