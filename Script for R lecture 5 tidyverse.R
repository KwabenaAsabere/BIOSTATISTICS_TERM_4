# R Script for R lecture 5, part 1

######  R Lecture 5 Part 1

# set working directory
setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 5/data/Excel")

# add tidyverse to library
library("tidyverse")


###### First dataset not orignally available as .csv file, but available from R package "Spatial Epi" (although I did include a
# a .csv version for download if you'd prefer to work with that)
  # install Spatial Epi package (once you've done this, there is no need to install it again)

# install.packages("SpatialEpi")
  # add to session library
  library(SpatialEpi)
  
  # now get data which is part of the "Spatial Epi" package
  data(pennLC)
  class(pennLC)
  
  names(pennLC)  
  class(pennLC$spatial.polygon)
  
  # this object is of class list, so names are not column headings, but sub objects contained with thie "list object"
  # a list is collection of pbjects that can differ by type
  
  # the "object" data stored in the list pennLC is a dataframe
  class(pennLC$data)
  
  # assign this to our own dataframee
  plc<-pennLC$data
  class(plc)
  names(plc)
  
  summary(plc$gender)
  summary(plc$cases)
  summary(plc$population)
  
  plc%>%
    summarise(misscases =sum(is.na(cases)), misspop =sum(is.na(population)))
  
  
  # to get overall incidence rate (cases per person year)
    inc<-plc%>%
      summarise(ir=sum(cases,na.rm=T)/sum(population,na.rm=T))
    
    inc$ir*100000
  
  # incidence rates by sex, and irr
    
    irss<-plc%>%
      group_by(gender)%>%
      summarise(ir=sum(cases,na.rm=T)/sum(population))
    
    #  irr female to male
    irss[1,2]/irss[2,2]
    irrftom<-irss[1,2]/irss[2,2]
    
    # irr male to females
    irss[2,2]/irss[1,2]
    irrmtof<-irss[2,2]/irss[1,2]
    
  # incidence rates by age groups
    
    class(plc$age)
    summary(plc$age)
    
    irsage<-plc%>%
      group_by(age)%>%
      summarise(ir=sum(cases)/sum(population))
    
    irsage
    # irrs with < 40 as reference
    
    c(irsage[1,2]/irsage[4,2],irsage[2,2]/irsage[4,2],irsage[3,2]/irsage[4,2])
  
   # output as .csv
    write.csv(plc, file="plc_data.csv")


############ R Lecture 5 Part 2    

########## PBC trial data
    
  pbc<-read.csv("pbc survival.csv")
  class(pbc)
  names(pbc)
  head(pbc)
  
  # one way to figure out if there are missing values
  
  table(pbc$trt)
  summary(pbc$trt)
  summary(pbc$ftime)
  summary(pbc$death)
  
  # another approach to sumarrizing number of missing values
  pbc%>%
    summarise(misstrt=sum(is.na(trt)),misstime=sum(is.na(ftime)),missdeath=sum(is.na(death)))
  
  # incidence rates by treatment, and irr
  irstrt<-pbc%>%
    group_by(trt)%>%
    summarise(ir=sum(death, na.rm=TRUE)/sum(ftime, na.rm=TRUE))
  
  irstrt
  
  irstrt[1,2]/irstrt[2,2]

### now for some survival graphs
  # install/load survival package

  install.packages("survival")
  library(survival)

  
  # ftime is in days
  # let's convert to years, and add a new column to the pbc data frame called "survyr
  pbc<-pbc%>%mutate(survyr=ftime/365.25)
  
  # overall survival for all enrollees
  
  table(pbc$death)
  pbc$SurvObj <- Surv(pbc$survyr, pbc$death)
  
  class(pbc$SurvObj)
  pbc$SurvObj

  kmpbc<-survfit(SurvObj ~1, data = pbc)
  names(kmpbc)
  # the following code does not work
  ggplot(data=kmpbc, aes(x=time,y=surv))+
    geom_line()
  
  # what kind of object is kpbmc?
  
  class(kmpbc)
  
  # need add-on package for ggplot to extract  relevant information from the "survfit" object 
  
  install.packages("survminer")
  library(survminer)
  
  # basic km plot
  ggsurvplot(kmpbc)
  
  # remove confidence interval shading, and add a "number at risk" table, and remove tick marks
  # for censored observations
  
  ggsurvplot(kmpbc,conf.int = F, risk.table = TRUE, censor=F)
  
  # present as cumulative incidence of death 
  ggsurvplot(kmpbc,conf.int = F, risk.table = TRUE, fun="event",censor=F)
  
  
  # now plot separate curves for drug and placebo groups
   
    # a reminder of the group sizes
    summary(pbc$trt)
  
    # create the survfit object allowing for group specific curve, and then plot them with "ggsurvplot"
    
    kmpbc2<-survfit(Surv(survyr, death) ~  trt, data =pbc)
    ggsurvplot(kmpbc2,conf.int = F,risk.table = TRUE, censor=F)
  
    # representation as cumulative mortality

    ggsurvplot(kmpbc2, data = pbc,conf.int = F,risk.table = TRUE, fun="event", censor=F)
   
# Now let's try make it look like the examples in the lecture
    ggsurvplot(kmpbc2, data = pbc,conf.int = F, censor=F,
                                  
                   # different linetypes for each group
                   linetype=c("dashed", "solid"),
                           
                    # axis limits and "breaks"
                                     
                    xlim=c(0,14),
                    break.time.by = 2,
                    ylim=c(0,1),
                    break.y.by = .2,
                              
                   # changing colors of curves
                    palette = c("red", "black"),
                                       
                   # altering legend location and text
                                  
                    legend=c(0.2,0.5),
                    legend.labs=c("DPCA", "Placebo"),
                    legend.title="", 
               
                   # center the title (which will be added shortly), remove background, add lines for x and y-axis, add box around legend
                   
                    ggtheme= theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),axis.line = element_line(color = 'black'),
                                   legend.background = element_rect(fill="white", 
                                                                    size=0.5, linetype="solid",colour ="black"))
               
                    ) +
      
               # add titles and labels
      
              labs(title="KM Estimates of Survival",x = "Follow-up Time (years since study enrollment) \n 312 Patients with Primary Biliary Cirrhosis (PBC)"
                , y = "Percentage Who Have Not Died")
)     

     
     