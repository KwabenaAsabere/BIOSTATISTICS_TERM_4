
# plots for  predicted probabilies of obesity by continous HDL by sex and age catgories (based on,lr2)
mlr2<-glm(obese~female+lbdhdd+ageq,family=binomial(link="logit"),data=nhanesos )
fage1<-nhanesos%>%filter(female=="Female" & as.numeric(ageq)==1)
fage1<-fage1%>%arrange(lbdhdd)
fage1<-fage1%>%mutate(pred=predict(mlr2,fage1, type="response"), group="Females 18-32")

fage2<-nhanesos%>%filter(female=="Female" & as.numeric(ageq)==2)
fage2<-fage2%>%arrange(lbdhdd)
fage2<-fage2%>%mutate(pred=predict(mlr2,fage2, type="response"),group="Females 32-47")

fage3<-nhanesos%>%filter(as.numeric(female)==2 & as.numeric(ageq)==3)
fage3<-fage3%>%arrange(lbdhdd)
fage3<-fage3%>%mutate(pred=predict(mlr2,fage3, type="response"),group="Females 47-62")

fage4<-nhanesos%>%filter(female=="Female" & as.numeric(ageq)==4)
fage4<-fage4%>%arrange(lbdhdd)
fage4<-fage4%>%mutate(pred=predict(mlr2,fage4, type="response"),group="Females 62-80")

ggplot()+
  geom_line(data=fage1, aes(x=lbdhdd,y=pred))+
  geom_line(data=fage2, aes(x=lbdhdd,y=pred), col=2)+
  geom_line(data=fage3, aes(x=lbdhdd,y=pred), col=3)+
  geom_line(data=fage4, aes(x=lbdhdd,y=pred), col=4)+

# unfortunately, when the data are in separate data frames, it's relatively easy to graph the separate lines with ggplot
  # but hardto label them in a legend
# so another approach is to create a new data frame with the predicted probabilites and hdl measures by each subset, stacked
  
  a<-c(fage1$pred,fage2$pred,fage3$pred,fage4$pred)
  b<-c(fage1$lbdhdd,fage2$lbdhdd,fage3$lbdhdd,fage4$lbdhdd)
  c<-c(fage1$group,fage2$group,fage3$group,fage4$group)
  
  predg<-cbind.data.frame (a,b,c)
  colnames(predg)<-c("pred","hdl","group")
  
  ggplot(predg, aes(x=hdl,y=pred))+
    geom_line( aes(col=group))
  
# males

mage1<-nhanesos%>%filter(female=="Male" & as.numeric(ageq)==1)
mage1<-mage1%>%arrange(lbdhdd)
mage1<-mage1%>%mutate(pred=predict(mlr2,mage1, type="response"),group="Males 18-32")

mage2<-nhanesos%>%filter(female=="Male" & as.numeric(ageq)==2,group="Males 32-47")
mage2<-mage2%>%arrange(lbdhdd)
mage2<-mage2%>%mutate(pred=predict(mlr2,mage2, type="response"))

mage3<-nhanesos%>%filter(as.numeric(female)==1 & as.numeric(ageq)==3,group="Mles 47-62")
mage3<-mage3%>%arrange(lbdhdd)
mage3<-mage3%>%mutate(pred=predict(mlr2,mage3, type="response"),)

mage4<-nhanesos%>%filter(female=="Male" & as.numeric(ageq)==4)
mage4<-mage4%>%arrange(lbdhdd)
mage4<-mage4%>%mutate(pred=predict(mlr2,mage4, type="response"),group="Males 62-80")

ggplot()+
  geom_line(data=mage1, aes(x=lbdhdd,y=pred))+
  geom_line(data=mage2, aes(x=lbdhdd,y=pred), col=2)+
  geom_line(data=mage3, aes(x=lbdhdd,y=pred), col=3)+
  geom_line(data=mage4, aes(x=lbdhdd,y=pred), col=4)


###### prediction with  cutoffs of .234 and 0.65
nhanesos<-nhanesos%>%mutate(probobese=predict(mlr2,nhanesos, type="response"))%>%
  mutate(cutoff1=as.numeric(probobese> .234),cutoff2=as.numeric(probobese> .65))

nhanesos%>%count(obese, cutoff2) %>%
  group_by(cutoff2) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  spread(cutoff2, prop)


### roc curves
# need to first install the package "pROC" from the CRAN site (install.packages(pROC))
library("pROC")

predobese<-predict(mlr2,nhanesos, type="response")
rocmlr2<-roc(nhanesos$obese,predobese, plot=T)

#######  Predictors of Breast feeding 


setwd("~/Dropbox/JMCGREADY/Statistics in PH 2 2019/lecture slides/lecture set 2/datasets/Excel")
bfs<-read.csv("bf_12_36_mos_nepal.csv", header=T)


# sex 1 for females
table(bfs$sex_chld)
slr1<-glm(bf~sex_chld,family=binomial(link="logit"),data=bfs)
summary(slr1)
length(slr1$fitted.values)
exp(coef(slr1))
exp(confint(slr1))


# age
# exploring shape of relationship (we did this in R lecture 2 as well)

# part 1, estimate log odds as function of age
    bfs<-bfs%>%
      mutate(phat=predict(loess(bfs$bf~bfs$age_chld)))%>%
      mutate(phat = ifelse(phat>=1, .99, phat))%>%
      mutate(lodds =log(phat/(1-phat)))

# A LOWESS smoothing plot showing the relationship between the log-odds of being breast fed and age 

    ggplot(bfs, aes(x=age_chld, lodds))+
      geom_point()+
      geom_smooth(method = "loess",se=FALSE)



# logistic regression
  slr2<-glm(bf~age_chld,family=binomial(link="logit"),data=bfs )
  summary(slr2)
  
  summary(bfs$age_chld)

# Parity: # of children including this birth
    table(bfs$parity)
    bfs$parcat<-ifelse(bfs$parity>3, 4, bfs$parity)
    #bfs$parcat<-factor(bfs$parcat, levels = c("0 prev chld", "1 prev chld", "2 prev chld", "> 2 prev chld)"))
    table(bfs$parcat)
    
    slr3<-glm(bf~as.factor(parcat),family=binomial(link="logit"),data=bfs )
    summary(slr3)

# need to load package "lmtest" from CRAN
  library("lmtest")
  lrtest(slr3)

# maternal age
  slr4<-glm(bf~age_mom,family=binomial(link="logit"),data=bfs )
  summary(slr4)

# MLRs

# sex and age
  mlr1<-glm(bf~sex_chld+age_chld,family=binomial(link="logit"),data=bfs)
  summary(mlr1)

# sex, age, parity cat
  mlr2<-glm(bf~sex_chld+age_chld+as.factor(parcat),family=binomial(link="logit"),data=bfs)
  summary(mlr2)

# sex, age, parity cat, age mom
  mlr3<-glm(bf~sex_chld+age_chld+as.factor(parcat)+age_mom,family=binomial(link="logit"),data=bfs)
  summary(mlr3)


