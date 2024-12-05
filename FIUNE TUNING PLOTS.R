library(tidyverse)
library(gapminder)

data("gapminder")

p0 <- gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(y = lifeExp, x = gdpPercap, colour = continent))+
  geom_point(alpha = 0.3)+
  theme_bw() +
  geom_smooth( method = "lm",se = FALSE)+
  scale_color_brewer(palette = "Set1")

p0

p1 <- p0 + scale_x_log10()

p2 <- p0 + expand_limits( y = 0)
p2

p3 <- p0 + expand_limits(y = c(0,100))
p3

p4 <- p0 + 
  expand_limits(y = c(0,100))+
  coord_cartesian(expand = FALSE)

p4

library(patchwork)
p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "1", tag_prefix = "p")

p5 <- p0 +
  coord_cartesian(ylim = c(70,85), xlim = c(20000,40000))
p5

p6 <- p0 +
  scale_y_continuous(limits = c(70,85))+
  scale_x_continuous(limits = c(20000,40000))
p6

p5 + labs(tag = "p5") + p6 + labs( tag = "p6")


## Axis Tricks

max_value <- gapminder %>% 
  filter(year == 2007) %>% 
  summarise(max_lifeExp =max(lifeExp) ) %>%
  pull(max_lifeExp) %>% 
  round(1)

p7 <- p0 +
  coord_cartesian(ylim = c(0,100), expand = 0)+
  scale_y_continuous(breaks = c(18,50,max_value))
p7


p8 <- p0 +
  coord_cartesian(ylim = c(0,100), expand = 0)+
  scale_y_continuous(breaks = c(18,50,max_value),labels = c("Adults", "50","MAX"))
p8

p7 + labs(tag = "p7")+ p8 + labs(tag = "p8")


p9 <- p0+
  scale_color_brewer(palette = "Paired")
p9


## choosing colors manually

p11 <- p0 +
  scale_color_manual(values = c("red","green","blue","purple","pink"))
p11

p13 <- p0 +
  labs(
    x = "Gross Domestic Product per Capita",
    y = "Life Expectancy",
    title = " Health and Economics",
    subtitle = "Gapminder dataset,2007",
    caption = Sys.Date(),
    tag = "p13"
  )
p13

p14 <- p0+
  annotate("text",
           x = 25000,
           y = 50,
           label = "No points here!")
p14


p15 <- p0+
  annotate("label",
           x = 25000,
           y = 50,
           label = "No points here!")
p15

p16 <- p0+
  annotate("label",
           x = 25000,
           y = 50,
           label = "No points here!",
           hjust = 0)
p16

p14 + labs(tag = "p14") + (p15 + labs(tag = "p15"))/(p16 + labs(tag = "p16"))




































































