library(tidyverse)
library(broom)
nepalbf <- read_csv("nepalbf.csv")
model1 <- nepalbf %>%  
  lm(age_mom~parity,data=.)

summary(model1)
anova(model1)
confint(model1)

tidy(model1)

model1_aug <- augment(model1) |> 
  view()

glance(model1)

model1_aug |> 
  ggplot(aes(x=.fitted,y=.resid)) +
  geom_point(color="steelblue",)+
  geom_smooth(method="lm",se=FALSE,color="red")+
  theme_bw()+
  labs(x="Fitted Values",y="Residuals",title= "Residual Plot")

model1_aug |> 
  ggplot(aes(sample=.fitted))+
  geom_qq()+
  geom_qq_line()

model1_aug |> 
  ggplot(aes(sample=.resid))+
  geom_qq()+
  geom_qq_line()+
  theme_bw()+
  labs(x="Theoretical Quantiles", y= "Sample Quantiles",
       title = "Normal Q-Q Plot of Residuals")




model1_aug |> 
  ggplot(aes(x=.resid))+
  geom_histogram(fill= "steelblue")+
  theme_bw()+
  labs(x="Residuals")
  














model2 <- lm(age_mom~parity,data=nepalbf)

model_2 <- nepalbf |> 
  lm(age_mom~parity,data=_)

model3 <- glm(age_mom~parity,family=gaussian(link="identity"),
              data=nepalbf)
summary(model3)
anova(model3)
confint(model3)


nepalbf |> 
  ggplot(aes(x=parity,y=age_mom))+
  geom_point(color="steelblue")+
  geom_smooth(se=FALSE,method="lm")+
  theme_bw()+
  labs(y="Age of Mother(years)",x=" Number of Kids Mother had born alive")


model4 <- nepalbf |> 
  glm(bf~sex_chld,family=binomial(link="logit"),data=_)
summary(model4)
anova(model4)
confint(model4)
model4 |> 
  tidy()

exp(model4$coefficients)
exp(confint(model4))

























