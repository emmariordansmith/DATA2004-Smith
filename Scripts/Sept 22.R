# pacakges

rm(list=ls())
library(tidyverse)

# let's start with the same persons_core and crashes_core 
crashes <- read_csv("Data/Raw/crashes.csv")
persons <- read_csv("Data/Raw/person.csv")

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

glimpse(crashes_core)
glimpse(persons_core)

# let's do a brief review of our mutating joins :) 

## what are the primary keys? what about the foreign key? 

#primary keys identify one unique observation in a dataset. The crashes primary 
#key is Collision ID
#the person primary key is uniqueId.

#the foreign key of person is collision ID, linking it to crashes

## what are our mutating joins? what's the difference? 

#left join retains all of the left dataset and matches the data of the other to
#those existing observations. Non matches are retained. Unmatched is NA values for other dataset
#inner join only retains the observations that match

## let's check out the homework briefly. 

# Which crashes involved at least one bicyclist? I want one row per crash. 
# we'll start by making a table of just the bicyclist person records. how many are there?

bicyclist<-persons_core%>%
  filter(PERSON_TYPE=="Bicyclist")

#6228 bicyclist records

# does that number answer our question? why not? 

#No, because the question is about crashes, not number of bicyclists total

# if it doesn't, which join should we reach for?

#We should use a left_join using Collision ID as a join

# use nrow() on the join and n_distinct() on that join's collision ID. Why are they different? 

nrow(bicyclist) #6228
n_distinct(bicyclist$COLLISION_ID) #6016

#there could be multiple bikes or tandem bikes 

# we can answer this by thinking about the grain.
# we're joining persons to the crashes grain, so what does one row represent? 

bike_crashes<-crashes_core%>%
  left_join(bicyclist,join_by(COLLISION_ID))

# is it every crash involving a bicyclist? let's check out the first 10 rows.  

bike_crashes%>%
  slice_head(n=10)

# our mutating join adds columns so it has changed our grain, but we don't want it to right now. 

# so we'll need to use *filtering* joins
# we got exposed to one filtering join already: anti_join(). 
# which filtering join that will keep matches instead of non-matches?

# this doesn't add more columns, so we're not working with crash-bicyclists combination

#semi-join! keeps all matches

bike_crashes<-crashes_core%>%
  semi_join(bicyclist,join_by(COLLISION_ID))

#only 6016 individual observations, same as the number of distinct collisions

# now do anti_join for crashes that do not involve a bicyclist. 

no_bikes<-crashes_core%>%
  anti_join(bicyclist,join_by(COLLISION_ID))

# what should the nrow() of each of your filtering joins dataframes be?

# now it's y'all's turn: identify crashes that involve at least one pedestrian, 
# one row per crash. 

pedestrian<-persons_core%>%
  filter(PERSON_TYPE=="Pedestrian")

n_distinct(pedestrian$COLLISION_ID) #should expect 9893 when joined

pedestrian_crashes<-crashes_core%>%
  semi_join(pedestrian,join_by(COLLISION_ID))

nrow(pedestrian_crashes) #9893, correct!

## after that, narrow it down. crashes where at least one pedestrian was recorded as female. 

unique(pedestrian$PERSON_SEX)

pedestrian_f<-pedestrian%>%
  filter(PERSON_SEX=="F")

n_distinct(pedestrian_f$COLLISION_ID) #4940 expected

pedestrian_f_crashes<-crashes_core%>%
  semi_join(pedestrian_f,join_by(COLLISION_ID))

nrow(pedestrian_f_crashes) #4940

# back together
## where did you put the PERSON_SEX condition? why?

#prior to the join, as after the join it will not be specific enough
