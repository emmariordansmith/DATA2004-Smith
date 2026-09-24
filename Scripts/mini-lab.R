# Lab 2

rm(list=ls())

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("Data/Raw/crashes.csv")
persons <- read_csv("Data/Raw/person.csv")

# 1: Make a table of person records for female pedestrians 

pedestrian_f<-persons%>%
  filter(PERSON_TYPE=="Pedestrian"&PERSON_SEX=="F")

## what does one row represent? 

#One row represents one female pedestrian involved in a crash

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

crashes_condensed<-crashes%>%
  select(COLLISION_ID,BOROUGH,`VEHICLE TYPE CODE 1`,`VEHICLE TYPE CODE 2`,`VEHICLE TYPE CODE 3`,
         `VEHICLE TYPE CODE 4`,`VEHICLE TYPE CODE 5`,)

f_pedestrian_crashes<-crashes_condensed%>%
  semi_join(pedestrian_f,join_by(COLLISION_ID))
  

# does one row still represent one crash? check it. 

#one row representing one crash would mean that the number of collision IDs
#should be equal to the number of observations, 4940.

n_distinct(f_pedestrian_crashes$COLLISION_ID) #4940, correct

# why a filtering join instead of a mutating join? 

#a filtering join keeps all matches, a mutating join keeps observations even if
#they don't have matches

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 

#the number of rows should be the number of observations, 4940, multiplied
#by the number of rows (5), so 4940*5=24700

fp_crashes_long<-f_pedestrian_crashes%>%
  pivot_longer(
    cols=c(`VEHICLE TYPE CODE 1`,`VEHICLE TYPE CODE 2`,`VEHICLE TYPE CODE 3`,
           `VEHICLE TYPE CODE 4`,`VEHICLE TYPE CODE 5`),
    names_to="vehicle code",
    values_to = "vehicle type")

# how many missing values are in the new dataframe? 

sum(is.na(fp_crashes_long$`vehicle type`))
#for the values specifically, 20203. Many.

# why do we think that slots 3, 4, and 5 have so many more missing values? 

#Because each vehicle is an additional vehicle involved in a crash. So having an
#NA for 3, 4, 5 means that only two vehicles were involved in the crash. 

# does every crash have a first vehicle recorded? 

sum(is.na(f_pedestrian_crashes$`VEHICLE TYPE CODE 1`))
#no, there are 601 one entries with female pedestrians that don't have a vehicle
#recorded for first.

# are we safe to drop missing values?

#yes, because if there is no car recorded, the entry is redundant and useless.

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 
fp_crashes_long |> 
  count(`vehicle type`, sort = TRUE) |> 
  print(n = 57)

#aside from NA, mostly Sedan and Station Wagons overwhelmingly
