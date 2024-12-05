library(tidyverse)

library(palmerpenguins)
penguins = penguins
penguins |> head()

penguins |> pull(species) |> unique()

penguins |> 
  ggplot(aes(x= flipper_length_mm, y = body_mass_g, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


penguins |> 
  ggplot(aes(x= flipper_length_mm, y = body_mass_g))+
  geom_point(aes(color = species, shape = species,size = ))+
  geom_smooth(method = "lm", se = FALSE)

penguins |> 
  ggplot(aes(x = fct_infreq(species),fill = species))+
  geom_bar()+
  scale_fill_viridis_d(option = "viridis")

penguins |> 
  ggplot(aes(x = fct_infreq(island),fill = species))+
  geom_bar(position  = "fill")+
  scale_fill_viridis_d(option = "viridis")



penguins |> 
  ggplot(aes(x = fct_infreq(island),fill = species))+
  geom_bar()+
  scale_fill_viridis_d(option = "viridis")


penguins |> 
  ggplot(aes(x= flipper_length_mm, y = body_mass_g))+
  geom_point(aes(color = species, shape = species,size = )) +
  facet_wrap(~island)


seq(1,101,10)

seq(from =1,to = 101, by = 10)

penguins |> distinct(island,species)

penguins |> distinct(species,island,.keep_all = TRUE)

penguins |> count(species,island,sort = TRUE)


quant =   quantile(penguins$body_mass_g, probs = seq(0,1,0.25),na.rm = TRUE)

penguins <- penguins |>
  mutate(sizes  = cut(body_mass_g,
            breaks = quant,labels =
            c("small","medium","large","huge"),include.lowest = TRUE),
         .after = body_mass_g) 

  quant =   quantile(penguins$body_mass_g, probs = seq(0,1,0.25),na.rm = TRUE)
           
           
           
           
penguins |> 
pull(body_mass_g) |> 
  quantile(probs = seq(0,1,0.1),na.rm = TRUE)



penguins |> 
  mutate(
    body_mass_quantile = cut(
      body_mass_g, 
      breaks = quantile(body_mass_g, probs = seq(0, 1, 0.25), na.rm = TRUE), 
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE,
    ),
    .after = body_mass_g
  )



penguins |> 
  mutate(
    body_mass_quantile = cut(
      body_mass_g, 
      breaks = quantile(body_mass_g, probs = seq(0, 1, 0.25), na.rm = TRUE), 
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE
    ),.keep = "used")



penguins |> na.omit() |> 
  count(sex,sizes) |> 
  ggplot(aes(x = sizes, y = n, fill = sex))+
  geom_bar(stat = "identity", position = "dodge")











































































































