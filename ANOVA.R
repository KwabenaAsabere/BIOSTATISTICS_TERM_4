library(palmerpenguins)
penguins <- penguins
penguins |> 
  ggplot(aes(x=species,y=flipper_length_mm,fill=species))+
  geom_boxplot()+
  theme_bw()

##ASUMPTIONS OF anova
##1) the observations are independent of each other
##2) the observations in each group are normally distributed
#3) the variance within each group are equal

## the null hypothesis is that the average flipper length among the various species
# are not different

# TO CHECK IF OBS ARE NORMALLY DISTRIBUTED --------------------------------

penguins |> 
  ggplot(aes(x=flipper_length_mm))+
  geom_histogram(aes(fill=species))+
  facet_wrap(~species,ncol=1)+
  theme_bw()


penguins |> 
  ggplot(aes(x=flipper_length_mm))+
  geom_density(aes(fill=species))+
  facet_wrap(~species,ncol=1)+
  theme_bw()


# EQUALITY OF VARIANCE ----------------------------------------------------

penguins |> 
  group_by(species) |> 
  summarise(variance = var(flipper_length_mm,na.rm=TRUE))

model_aov <- penguins |> 
  aov(flipper_length_mm ~species,data=_)
summary(model_aov)



# TO SEE WHICH OF THE DIFFERENCES BN TWO GROUPS ARE STATISTICALLY  SIGNIFICANT

TukeyHSD(model_aov)

model_aov |> 
  TukeyHSD()

# R PROGRAMMING 101 ANOVA -------------------------------------------------

library(gapminder)
library(patchwork)
library(forcats)

gapminder_data <- gapminder  |> 
  filter(year==2007 &
          continent %in% c("Americas","Europe", "Asia"))

gapminder_data |> 
  group_by(continent) |> 
  summarise(Mean_life= mean(lifeExp)) |> 
  arrange(Mean_life)

anova_gapdata <- gapminder_data |> 
  aov(lifeExp ~ continent, data = _) 
 
 summary(anova_gapdata)
 
 anova_gapdata |> 
   TukeyHSD()

 anova_gapdata |> 
   TukeyHSD() |> 
   plot()

 
 










