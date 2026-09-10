#Emma Smith
#Review Day 9/8/26

rm(list=ls())

library(tidyverse)
mmmmm
getwd()

chocolate<-read_csv("Data2004-Smith/Data/Raw/chocolate.csv")

glimpse(chocolate)

#select chooses columns
chocolate%>%
  select(ref,company_manufacturer,company_location,rating,cocoa_percent)

#filter filters based on information in a row
chocolate%>%
  filter(company_location=="U.S.A.")

#filter by two requirements (AND)
chocolate%>%
  filter(company_location=="U.S.A."&rating>=3.5) #can also use a comma rather than ampersand

#filter by two requirements (OR)
chocolate%>%
  filter(company_location=="U.S.A"|rating>=3.5)

#this, this, or this
chocolate |> 
  filter(company_location=="U.S.A"|company_location=="France"|company_location=="Canada")
#or
chocolate |> 
  filter(company_location%in%c("U.S.A.","France","Canada"))

#write a call that only shows reviews that are greater than or equal to 3.5 AND only
#for 2021, and the location EITHER USA or Vietnam
chocolate%>%
  filter(rating>=3.5,review_date==2021,company_location%in%c("U.S.A","Vietnam"))

#show only the ten highest rated bars
unique(chocolate$rating)

chocolate%>%
  select(rating,country_of_bean_origin)%>%
  arrange(desc(rating))%>%
  head(10)
#you can also use slice_head(n=10)

#average cocoa percent
chocolate<-chocolate%>%
  mutate(mean_cocoa=mean(cocoa_percent,na.rm=TRUE))
#returns an error because the cocoa_percent is a character with % signs

#turn it into numeric
chocolate<-chocolate%>%
  mutate(cocoa_num=parse_number(cocoa_percent))

#try again
chocolate%>%
  mutate(mean_cocoa=mean(cocoa_num,na.rm=TRUE))

#sample size for company location
chocolate%>%
  group_by(company_location)%>%
  summarise(n=n(),
            avg_rating=mean(rating,na.rm=TRUE),
            avg_cocoa=mean(cocoa_num,na.rm=TRUE))%>%
  slice_head(n=10)

#missingness
chocolate%>%
  group_by(rating)%>%
  summarise(n_missing=sum(is.na(rating)))

sum(is.na(chocolate$review_date))

#visualizations
chocolate%>%
  ggplot(aes(x=rating))+
  geom_histogram()

chocolate%>%
  ggplot(aes(x=rating))+
  geom_bar()

chocolate%>%
  filter(company_location%in%c("U.S.A.","France","Canada"))%>%
  ggplot(aes(x=rating,y=company_location,fill=company_location))+
  geom_boxplot()+
  labs(title="Rating by Company Location",
       x="Rating",
       y="Company Location")

