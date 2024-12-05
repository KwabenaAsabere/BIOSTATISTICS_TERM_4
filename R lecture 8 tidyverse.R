# R lecture 8
# Confidence Intervals for population comparison measures using R

## R Lecture 8, part 1
##### Section 1
######### Differences in Means

  #  Paired Study, where we have the raw data (BP/oral contraceptive, 10 women)
    setwd("~/Dropbox/JMCGREADY/Statistics in PH 2017/lecture slides/lecture set 8/data")
    bpp<-read.csv("blood press oc.csv", header=T)
    names(bpp)

    bpp<-bpp%>%mutate(diff=after_bp-before_bp)

    mean(bpp$diff)
    mean(bpp$after_bp)-mean(bpp$before_bp)

    # 95% CI for mean diff
    mean(bpp$diff)  -qt(.975,length(bpp$diff)-1)*sd(bpp$diff)/sqrt(length(bpp$diff))  # lower limit
    mean(bpp$diff)+qt(.975,length(bpp$diff)-1)*sd(bpp$diff)/sqrt(length(bpp$diff))  # upper limit

  # unpaired study: Heritage LOS by age

    setwd("~/Dropbox/JMCGREADY/Statistics in PH 2017/lecture slides/lecture set 2/data/Excel")
    hhdata<- read.csv("HH for analysis working.csv", header=T)
    names(hhdata)
    # created indicator of age > 40 years

    hhdata<-hhdata%>%
      mutate(agegt40=factor(age>40, labels = c("<= 40 years", " > 40 years")))


    # to get means and sds by group

     ss<-hhdata%>%
          group_by(agegt40)%>%
        summarize(m=mean(los_final,na.rm=T),s=sd(los_final, na.rm=T),n=length(los_final))

    # 95% CI: these are slightly differenct than the values in the lecture (including the mean difference) because
    # I rounded the results in the lecture for means and sds

     # to start, will store mean diff, se and n as simple numerical objects
     md<-ss[2,2]-ss[1,2]
     semd=sqrt( ss[2,3]^2/ss[2,4]+  ss[1,3]^2/ss[1,4])

     dfmd=ss[2,4]+ss[1,4]-2

      md  # mean difference
       md - qt(.975,as.numeric(dfmd))*semd # lower limit
       md + qt(.975,as.numeric(dfmd))*semd # upper limit

    # as before, we could have written a function to do this

    # FYI
    hhmeans<-unname(hhmeans)
    (hhmeans[2]-hhmeans[1])

###########################################################################################
### Section 2
##### Proportions

# All examples are based on the data on 1,000 HIV positive individuals

    setwd("~/Dropbox/JMCGREADY/Statistics in PH 2017/lecture slides/lecture set 4/data/Excel")
    kd<-read.csv("kaggle hiv.csv", header=T)
    names(kd)
    # created indicator of CD$ count grou

    kd<-kd%>%
      mutate(cd4lt250=factor(cd4t0<250,labels = c("CD4 >= 250", " CD4 < 250")))

    # Differences in Proportions

      # to get proportions and ns by group
      ss<-kd%>%
      group_by(cd4lt250)%>%
      summarize(phat=mean(resp,na.rm=T),n=length(resp))

      ss

      # 95% CI for CD4< 250 compared to CD4>= 250
      (ss[2,2]-ss[1,2]) - 1.96*sqrt ( ss[2,2]*(1-ss[2,2])/ss[2,3] +ss[1,2]*(1-ss[1,2])/ss[1,3])# lower
      (ss[2,2]-ss[1,2]) + 1.96*sqrt ( ss[2,2]*(1-ss[2,2])/ss[2,3] +ss[1,2]*(1-ss[1,2])/ss[1,3]) # upper

       ### stay tuned for R Lecture 8, PArt 2, where we set up a function to do CI computations for RRs and ORS

###### Incidence Rate Ratios

      library(SpatialEpi)
      # load data
      data(pennLC)
      names(pennLC)
      # created data frame of pennLC lung cancer counts data
      plc<-pennLC$data
      names(plc)
      dim(plc)

      ### to get incidence rates by  sex we need to aggregate across county, race, and age categories separately
      ### for each sex
      inc<-plc%>%
        group_by(gender)%>%
        summarise(ir=sum(cases,na.rm=T)/sum(population,na.rm=T),casetot = sum(cases,na.rm=T))

      # irr and lnirr:  f/m
      irr= inc[1,2]/inc[2,2]
      lnirr= log(irr)
       selnirr=sqrt(1/inc[1,3]+1/inc[2,3])

      exp(lnirr-1.96*selnirr)  # lower limit for 95% CI for irr
      exp(lnirr+1.96*selnirr)  # upper limit for 95% CI for irr


#################################################################################
# R Lecture 8 , part 2: creating a function to compute 95% CIs for RRs and ORs
# based on 2X2 table counts


    # Relative risk and odds ratio

      # for fun, and utility, let's write a function that does this
      #  assume 2X2 setups of the following type

      #             Exposed     Unexposed
      #   Outome       a            b
      #  No Outcome    c            d

      rrci <- function(a,b,c,d) {

        if (!isInteger(a)|!isInteger(b)|!isInteger(c)|!isInteger(d)) stop("a-d must be integers")
        rr<-(a/(a+c))/(b/(b+d))
        or<-(a*d)/(b*c)

        # rr computations
        lnrr<-log(rr) # log base e in R is the function "log"l log base 10 is the function "log10"
        selnrr<-sqrt(1/a-1/(a+c)+1/b-1/(b+d))
        rrcilower<-exp(lnrr-2*selnrr)
        rrciupper<-exp(lnrr+2*selnrr)

        # or computations
        lnor<-log(or)
        selnor<-sqrt(1/a+1/c+1/b+1/d)
        orcilower<-exp(lnor-2*selnor)
        orciupper<-exp(lnor+2*selnor)

        resultrr<-c(rr,rrcilower,rrciupper)
        resultor<-c(or,orcilower,orciupper)
        result<-data.frame(rbind(resultrr,resultor))
        names(result)<-c("estimate","95% CI lower","95% CI upper")
        return(result)
      }

      # for the inputs: totals with and without outcome in each group
      ab<-tapply(kd$resp,kd$cd4lt250,sum)
      phatns<-tapply(kd$resp,kd$cd4lt250,length)
      cd<-phatns-ab

      rrci(ab[2],ab[1],cd[2],cd[1])

