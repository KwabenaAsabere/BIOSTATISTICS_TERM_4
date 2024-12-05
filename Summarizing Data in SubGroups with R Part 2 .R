##### GRouping with R Part 2 (picks up where the video for Part 1 stopped:  there is no video yet for this section, but it is forthcoming)

###### cross tabulations of bike type and membership status, and p-value from chi squared

# base R
  table(bikes$rideable_type, bikes$member_casual) 
  prop.table(table(bikes$rideable_type, bikes$member_casual)) 
  
  # row percentages
  prop.table(table(bikes$rideable_type, bikes$member_casual), margin=1)
  # column percentages
  prop.table(table(bikes$rideable_type, bikes$member_casual), margin=2)


#tidyverse
  bikes%>%group_by(member_casual,rideable_type)%>%
    summarise(count = n())
    mutate(pct = count/sum(count)) %>% 
    spread(rideable_type, pct)


  # proportions out of member type totals
  # need to group first by member type, and then by ride type
  # spread command will display counts/percentage of levels second grouping factor
  # within each level of first grouing factor
  bikes%>%group_by(member_casual,rideable_type)%>%
    summarise(count = n())%>%
    mutate(pct = count/sum(count))%>%
    # need to remove variable count
    select(-count)%>%
    spread(rideable_type, pct)

  #  proportions out of ride type totals
  bikes%>%group_by(rideable_type,member_casual)%>%
    summarise(count = n())%>%
    mutate(pct = count/sum(count))%>%
    # need to remove variable count
    select(-count)%>%
    spread(member_casual, pct)

# chi square test

chisq.test(table(bikes$rideable_type, bikes$member_casual))
chisq.test(table(bikes$member_casual,bikes$rideable_type))

### summary statistics across groups defined by combinations of more than variable

# what if we wanted summary statistics in ride durations by combinations of bike type and membership staus

bikes%>%group_by(member_casual,rideable_type)%>%
  summarise(meandur= mean(duration, na.trm=T))

bikes%>%group_by(member_casual,rideable_type)%>%
  summarise(meandur= mean(duration, na.trm=T))%>%
  spread(rideable_type, meandur)

# what does spread do?
bikes%>%group_by(member_casual,rideable_type)%>%
  summarise(meandur= mean(duration, na.trm=T))%>%
  spread(member_casual, meandur)
