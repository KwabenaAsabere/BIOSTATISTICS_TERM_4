# R lecture 7 part 2
# Graphic multiple confidence intervals for a population proportion based 
# repeated sampling simulation
     
     #########################
     
     
     # Simulating confidence interval performance (FYI) for a sample proportion
     # assume a population of HIV+ persons where the percentage responding to therapy is 20.6%
     
     library(tidyverse)
     
     # create a "theoretical population population (100,000) with 20.6% responses
     pop1<-c(rep.int(1, .206*1000000), rep.int(0, (1-.206)*1000000))
     
     # make vectors to store 100 phats, and ci lower and upper endpoints
     phat<-rep.int(0,100)
     cil<-rep.int(0,100)
     ciu<-rep.int(0,100)
     inci<-rep.int(0,100)
     
     for (i in 1:100) {
       t<-mean(sample(pop1, 1000))
       phat[i]<-t
       cil[i]<- t-1.96*sqrt(t*(1-t)/1000)
       ciu[i]<- t+1.96*sqrt(t*(1-t)/1000)
       inci[i]<-as.numeric(.206 >= cil[i] & .206<=ciu[i])
     }
     
     table(inci)
     
     # create a data frame with the results, and add a generic id number for each set, from 1 to 100
     
     cis<-data.frame(id=1:100,phat,cil,ciu,inci)
     
     # to make a graph 
     #(and certainly one could make changes to the asethetics, such as differing line colors from the default, etc/// )
     ggplot(cis,aes(x=id,y=phat))+
       geom_point()+
       geom_segment(aes(x = id, y = cil, xend = id, yend = ciu, col=as.factor(inci))) +
       scale_y_continuous(limits =c(round(min(cil),digits=2), round(max(ciu),digits=2)),
                          breaks=seq(round(min(cil),digits=2),round(max(ciu),digits=2),by=.01))  +
       geom_hline(yintercept=.206,color = "red", size=1)+
       labs(title= "95% CIs for p from 100 Samples of n=1000 \nSamples from Population Where True p = .206 ", y="Proportion",x="Sample ID")+
       theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), plot.title =element_text(hjust = 0.5),plot.caption =element_text(hjust = 0.5))
        

            
     