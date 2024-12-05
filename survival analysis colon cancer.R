library(tidyverse)
library(survival)
library(finalfit)
library(ggsurvfit)
library(broom)

colon_ca <- colon
str(colon)
ff_glimpse(colon_ca)

colon |>
  mutate(sex=as.factor(sex)) |> 
  ggplot(aes(x=rx,y=age)) +
  geom_boxplot(aes(fill=sex))+
  theme_bw()

colon |>
  mutate(sex=as.factor(sex)) |> 
  ggplot(aes(x=rx,y=age)) +
  geom_boxplot(aes(fill=sex))+
  theme_bw()



