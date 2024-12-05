library(tidyverse)


lfit<-loess( nhanes_2017_20u$current_smoken~ nhanes_2017_20u$RIDAGEYR)


dfphat<-tibble(age=lfit$x, phat=lfit$fitted,lnodds=log(phat/(1-phat)))

ggplot( dfphat, aes(x = age, y= lnodds )) + geom_point()+geom_line()+
  labs(title = "Estimated Ln Odds of Smoking by Age, Smoothed",
       y = "Ln Odds of Smoking",
       x="Age Years")+
  scale_x_continuous(breaks=seq(20,80,by=5))


linear_fit <- nhanes_2017_20u %$% 
  loess(current_smoke ~ RIDAGEYR)
