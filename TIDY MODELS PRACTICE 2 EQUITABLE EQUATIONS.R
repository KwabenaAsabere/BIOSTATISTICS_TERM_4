library(tidymodels)
set.seed(0)
theme_set(theme_bw())
view(airquality)

## split data
aq_split <- initial_split(airquality)
aq_train <- training(aq_split)
aq_test <- testing(aq_split)

## visualize the data

aq_train %>% 
  ggplot(aes(x = Wind,
             y = Temp)) +
  geom_point()+
  geom_smooth(method = 'lm',
              se = FALSE)

## building the model

aq_model<- lm(Temp ~ Wind, data = aq_train)
names(aq_model)

summary(aq_model)
fitted.values(aq_model)
confint(aq_model)

plot(aq_model)


## the broom package

aq_model_tidy <- aq_model %>% 
  augment()

tidy(aq_model)
glance(aq_model)
augment(aq_model)

## Model performance

predict(aq_model,
        aq_test,
        interval = 'confidence')


predict(aq_model,
        aq_test,
        interval = "prediction")

## Root mean square error
sqrt(mean((aq_test$Temp - predict(aq_model,aq_test))^2))


























