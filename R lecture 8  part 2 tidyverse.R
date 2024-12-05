#################################################################################
# R Lecture 8 , part 2: creating a function to compute 95% CIs for RRs and ORs
# based on 2X2 table counts

#### first things first
library(tidyverse)

# Relative risk and odds ratio

# for fun, and utility, let's write a function that does this
#  assume 2X2 setups of the following type

#             Exposed     Unexposed
#   Outome       a            b
#  No Outcome    c            d

  rrci <- function(a,b,c,d) {

    testcond = (is.numeric(a) & is.numeric(b) & is.numeric(c) & is.numeric(d))

    if (! (testcond=="TRUE")) stop("a-d must be integers")

    rr<-(a/(a+c))/(b/(b+d))
    or<-(a/c)/(b/d)

    # rr computations
    lnrr<-log(rr) # log base e in R is the function "log"l log base 10 is the function "log10"
    selnrr<-sqrt(1/a-1/(a+c)+1/b-1/(b+d))
    rrcilower<-exp(lnrr-1.96*selnrr)

    rrciupper<-exp(lnrr+1.96*selnrr)

    # or computations
    lnor<-log(or)
    selnor<-sqrt(1/a+1/c+1/b+1/d)
    orcilower<-exp(lnor-1.96*selnor)
    orciupper<-exp(lnor+1.96*selnor)

    resultrr<-c(rr,rrcilower,rrciupper)
    resultor<-c(or,orcilower,orciupper)
    result<-data.frame(rbind(resultrr,resultor))
    colnames(result)<-c("estimate","95% CI lower","95% CI upper")
    rownames(result)<-c("relative risk","odd ratio")
    return(result)
  }

# for the inputs: totals with and without outcome in each group

setwd("~/Dropbox/JMCGREADY/PH Statistics 2019/lecture slides/lecture set 4/data/Excel")
kd<-read.csv("kaggle hiv.csv", header=T)
names(kd)
# created indicator of CD$ count grou

kd<-kd%>%
  mutate(cd4lt250=factor(cd4t0<250,labels = c("CD4 >= 250", " CD4 < 250")))

# Differences in Proportions

# to get cell counts for 2X2 table
ccs<-kd%>%
  count(resp, cd4lt250)%>%
  spread(key = cd4lt250, value = n)

rrci(ccs[2,3],ccs[2,2],ccs[1,3],ccs[1,2])
rrci(as.numeric(ccs[2,3]),as.numeric(ccs[2,2]),as.numeric(ccs[1,3]),as.numeric(ccs[1,2]))
