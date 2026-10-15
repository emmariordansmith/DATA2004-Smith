# weekly assignment review 

# now let's review the reading together, briefly 

rm(list=ls())

## what does is.na() do? 
#returns a logical true/false if something is missing

## what does na.rm = TRUE mean?
#ignores na values when calculating. within the specified column only

## what is the difference between an explicit and implicit missing value?
#explicit missingness is an NA. Implicit missingness is like a whole row of data 
#missing from the set

# brief introduction to statistical understanding of missingness: 
## there are (basically) three kinds of missingness that statisticians evaluate 
#missing at random (MAR)
#missing not at random (MNAR), structured missing values. Identifiable reason
#Missing completely at random (MCAR)

# packages 
library(tidyverse)

# we will get our data from this package today 
# install.packages("NHANES")

library(NHANES)

data(NHANESraw)

# this is the CDC's National Health and Nutrition Examination Survey; 
# interviews, physical exams, and lab tests, from 2009 to 2012. 
# use ?NHANESraw for documentation

# note: make sure you use NHANESraw NOT NHANES. NHANES is a weighted survey to 
# give inferences about the US population; some people are repeated to weight 
# their respective groups heavier. 

glimpse(NHANESraw)

#one row represents one individual who was interviewed by the CDC between 2009 and 2012

#primary key is ID

# again we'll start with a simple question: what share of adults currently smoke? 
NHANESraw |> 
  summarise(pct_smoke = mean(SmokeNow == "Yes", na.rm = TRUE))

table(NHANESraw$SmokeNow)

#of people who are over 20 years old and have smoked 100 cigarettes in their 
#lifetime, there are 2454 people who said they smoke now.

# is that reasonable? is something else going on here? 
# let's look at how many people that answer actually used, and out of how many

#SmokeNow was only asked to people who answered yes to Smoke100, whether or not 
#they have smoked 100 cigarettes in their lifetime. So this is only asked to people who have
#smoked significantly already.

# check the documentation for SmokeNow. is there an explanation of the missing 
# values?

#yes there is. Its missing values come from those who are not over 20 years old
#or have not smoked 100+ cigarettes in their lives.

# given that, what did na.rm = TRUE actually do? it literally removed missing values, 
# yes, but changed the question that we're asking. think about where the NA values 
# would systematically be located. 

#na.rm reduced the total population used to count this percentage.

# the question ..."Yes", na.rm = TRUE)) is asking isn't "what share of adults currently smoke?"
# it's asking "Among adults who've smoked 100 cigarettes, what share still smoke?"

# how should we deal with these missing values?
# R4DS says that we can use caolese() when a missing value stands for a value 
# that you actually know. 

NHANESraw |> 
  summarise(pct_smoke = mean(coalesce(SmokeNow, "No") == "Yes"))

# is that reasonable? what did coalesce() assume?

#replaces NA with the value we specify (No)
#however, this includes children and those under age 20 now. There are too many nos now

# who else is missing in SmokeNow? look at Age. 
# create a column called age_group and define the groups as 
# 20+ and under 20. 

NHANES_age<-NHANESraw%>%
  mutate(age_group=case_when(Age>=20~"20+",Age<20~"under 20"))


# after that, keep only adults *first*, then use coalesce() again. 

NHANES_age%>%
  group_by(age_group)%>%
  count()

NHANES_age |> 
  filter(age_group=="20+")%>%
  summarise(pct_missing=sum(is.na(SmokeNow)),
    pct_smoke = mean(coalesce(SmokeNow, "No") == "Yes"))

# so how can we create a column called 'smokes' that handles these missing values 
# correctly? 
# hint: we know Age matters here, and we know that participants who responded with 
# "No" in Smoke100 are actually "No"

NHANES_age%>%
  mutate(smokes=case_when(age_group=="20+"&SmokeNow=="Yes"~"Yes",
                          age_group=="20+"&Smoke100=="No"~"No",
                          TRUE~SmokeNow))%>%
  select(Age,age_group,Smoke100,SmokeNow,smokes)

# do we have any missing in our smokes column?

#yes, but only those that are under 20

# should they become "No"? 
# with that, does NA mean actually one thing in this column? 

# your turn :) 

# for each of the following variables: 
## 1. what percent is missing?
## 2. who was eligible to have a value? check ?NHANESraw.
## 3. is the missingness mostly outside that group?
## 4. answer the question, and say exactly who your answer describes.

# A. SleepHrsNight 
## how many hours do people usually sleep on a weeknight? 

# B. Testosterone 
## what's the average testosterone? does it differ between years? 
## hint: after you drop the missing values, count by SurveyYr. where did 2009_10 go?


# let's co one more thing: income 
# let's focus on adults 
adults <- NHANESraw |> 
  filter(Age >= 20)

adults |> 
  summarise(pct_missing = mean(is.na(HHIncomeMid)))

# does the doc give us anything at all here? 

# let's see if the missingness is systematically related to another variable 
adults |> 
  group_by(Education) |> 
  summarise(
    pct_missing = mean(is.na(HHIncomeMid)),
    mean_income = mean(HHIncomeMid, na.rm = TRUE)
  ) |> 
  arrange(Education)

mean(adults$HHIncomeMid, na.rm = TRUE)

# how does the pattern of missigness bias the results? 
# what happens if we just estimate the mean of HHIncomeMid and remove NAs?
