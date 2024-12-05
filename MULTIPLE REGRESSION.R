library(tidyverse)
library(broom)

mtcars <- mtcars

model1 <- mtcars |> 
  lm(mpg ~ wt,data= _)
summary(model1)

model2 <- mtcars |> 
  mutate(am=as.factor(am)) |> 
  mutate(am= fct_recode(am,
                        "Automatic"="0",
                        "Manual"= "1")) |> 
  lm(mpg ~ wt*am,data =_)
summary(model2)

mtcars |> 
  ggplot(aes(x=wt,y=mpg,color=as.factor(am)))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme_bw()+
  labs(x="Weight of Cars",
  y=" Fuel Efficiency",
  title=" Fuel Efficiency Explained by Weight of Cars",
  color="Legend")+
  theme(legend.position="bottom")


  

 
##TREE DATASET --------------------------------------
trees |> 
  ggplot(aes(Girth,Volume,color=Height))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme_bw()+
  labs(title="Tree Volume explained by Girth & Height")


modelA <- trees |> 
  lm(Volume ~Girth +Height, data= _)
summary(modelA)



#  CATEGORICA VARIABLES ---------------------------------------------------
mpg <- mpg

mpg |> 
  ggplot(aes(x = displ,
             y=hwy))+ 
  geom_jitter()+
  geom_smooth(method= lm,se = F)+
  theme_bw()+
  labs(title= " Highway Fuel Efficiency Explained by Engine Size",
       x= "Engine Size",
       y= " Highway Fuel Efficiency")

modelB <- mpg |> 
  lm(hwy ~ displ, data = _)
summary(modelB)

mpg |> 
  mutate(drv= fct_recode(drv,
                         "2"="f",
                         "2"="r")) |> 
  ggplot(aes(displ,hwy,color=drv))+
  geom_point()+ 
  geom_smooth(method=lm,se = F)+
  theme_bw()+
  labs(title= " Highway Fuel Efficiency Explained by Engine Size",
       x= "Engine Size",
       y= " Highway Fuel Efficiency",
       color = "Drive")

mpg |> 
  mutate(drv= fct_recode(drv,
                         "2"="f",
                         "2"="r")) |>
  lm(hwy ~ displ + drv,data = _) |> 
  summary()
  
  
























  





