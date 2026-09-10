#Lab 

rm(list=ls())

library(tidyverse)

#grain is what one row represents. The grain of a dataset is "one row represents ____"
#one row represents one penguin, for example. Always explicitly state the grain.

#granularity: the level of detail in the observation
#Country is less granular than state than county for example. The closer or more
#specific something is the more granular.
#a file can have more than one level of granularity

#documentation. How was it acquired, who produced it, units, time period,
#definitions of codes. Documentation or Code Book label

#Workflow: import, inspect, check documentation, declare grain, calculate.
#I will be graded on this. Don't forget

#Three questions:
# What is the total population of the US in 2025 according to this file
# Which Kentucky counties or county equivalent records are in this file
# What do we need to do before we can answer all these?

data<-read_csv("Data/Raw/co-est2025-alldata.csv",
               locale=locale(encoding="Latin1"),
               col_types=cols(.default=col_character()))

glimpse(data)
names(data)

#all of the numbers are displaying as characters, use as.numeric or parse_number

#DOCUMENTATION
#ask, look it up. 

#annual resident populations and population changes, from the census bureau.

#What can we determine from this? What can we not?
#We can determine state and county population and changes. 

#SUMLEV indicates whether the row/observation is a county or a state. 040 is 
#for a state and 050 is a county

#The census bureau produced this document. File contains data about population
#and population changes, and the time frame is 2020-2025.

#DIAGNOSTIC VIEW

#these variables determine what one row represents. We also need the rest to answer
#the questions put to us about the US and Kentucky
data_trimmed<-data%>%
  select(SUMLEV,REGION,DIVISION,STATE,COUNTY,STNAME,CTYNAME,POPESTIMATE2024,POPESTIMATE2025,
        NPOPCHG2024,NPOPCHG2025)

#does every row represent the same kind of geographical information?
#No, some are state or county within state. In SUMLEV, 040 is state, 050 is county

data_trimmed%>%
  slice_head(n=10)

#DECLARE THE GRAIN

#One row represents either a county within a given state or a state itself.
#One row represents a specific geographical location
#A mixed state-county grain

#convert columns to numeric:

data_trimmed_numeric<-data_trimmed%>%
  mutate(pop2025=as.numeric(POPESTIMATE2025),
         pop2024=as.numeric(POPESTIMATE2024),
         popchange2025=as.numeric(NPOPCHG2025),
         popchange2024=as.numeric(NPOPCHG2024))

#total US population
data_trimmed_numeric%>%
  filter(SUMLEV=="040")%>%
  summarise(totaluspop25=sum(pop2025))
#341784857

data_trimmed_numeric%>%
  filter(SUMLEV=="040")%>%
  select(STNAME)
#includes Washington DC

data_trimmed_numeric%>%
  filter(STNAME=="Kentucky")%>%
  print(n=125)

#120 county observations and one state observation  

#which kentucky counties grew the most from 2024 to 2025

data_trimmed_numeric%>%
  select(CTYNAME,popchange2025,SUMLEV,STNAME)%>%
  filter(STNAME=="Kentucky"&SUMLEV=="050")%>%
  arrange(desc(popchange2025))

#the county that grew the most in 2025 was Warren County, followed by Fayette and Madison

#is 2025-2024 the same?
data_trimmed_numeric%>%
  filter(STNAME=="Kentucky",SUMLEV=="050")%>%
  mutate(popchgvalid=pop2025-pop2024)%>%
  select(CTYNAME,popchgvalid)%>%
  arrange(desc(popchgvalid))%>%
  print(n=120)

#How many counties or county-equivelent records are in this file

data_trimmed_numeric%>%
  filter(SUMLEV=="050")%>%
  count()

#3144 counties are in this file

