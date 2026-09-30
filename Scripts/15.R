rm(list=ls())

library(tidyverse)

library(readxl)

fish<-read_excel("Data/Raw/commercial.xlsx",sheet="Erie")

glimpse(fish)

#one row represents one year of the weight of a specific type of fish caught in a region

fish%>%
  ggplot(aes(x=Year,y=`Grand Total`))+
  geom_line()

nrow(fish)

#combine seven columns into one

#multiply the number of rows by the rows we are pivoting
#17,934 rows now once it pivots

#Pivot

fish_long<-fish%>%
  pivot_longer(
    cols=4:10, 
    names_to="region",
    values_to="values"
  )

#alternately: use cols=!c(Year,Lake,Species,Comments)
# or also cols=contains(``)
# or cols=c(Michigan, Ohio, New York, etc)

fish_long%>%
  distinct(region)

#what is the grain now? The grain now includes state and country within regions
#grand total and US total are also not states 

fish_long%>%
  filter(Year==1885&Species=="Lake Whitefish")%>%
  select(region,values)

fish_long%>%
  filter(region!="Grand Total")%>%
  summarise(total=sum(values,na.rm=TRUE))

fish_long%>%
  filter(!region%in%c("U.S. Total","Grand Total"))%>%
  summarise(total=sum(values,na.rm=TRUE))

#now what is our y value

#display by states
fish_long%>%
  filter(region!="U.S. Total",region!="Grand Total")%>%
  mutate(species=fct_lump_n(Species, 6))%>%
  ggplot(aes(x=Year,y=values,color=species))+
  geom_line()+
  ggtitle("Fish Caught")+
  xlab("Year")+
  ylab("Fish Weight")+
  scale_color_manual(values=palette.colors(7,"Okabe-Ito"))

#pivot longer is more common than pivot wider

fish_long%>%
  select(Year,Species,region,values) |> 
  pivot_wider(names_from=region,values_from=values)%>%
  print(width=Inf)

#different lake
fish_huron<-read_excel("Data/Raw/commercial.xlsx",sheet="Huron")

glimpse(fish_huron)

#Grain: one row represents one year of the weight of one species caught in a given region


#the total catch for the lake is:


unique(fish_huron_long$region)
#the regions left are 


#pivots
#the starting number of rows is 2582
#the expected number of rows is 2582*8=20656

fish_huron_long<-fish_huron%>%
  pivot_longer(
    names_to="region",
    values_to="values",
    cols=!c(Year,Species,Lake,Comments)
  )

#the actual number of rows is 20656 after the pivot





