# packages 
rm(list=ls())
library(tidyverse)

# data
requests <- read_csv(
  "Data/Raw/nyc311_2023_sample_50000.csv/nyc311_2023_sample_50000.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

glimpse(requests) 

# what does one row represent? how do you know?

#one row represents one 311 non-emergency request from a person in new york city to 
#the city services/agencies to fix or solve a problem.

# what identifies one request? how do you know?

n_distinct(requests$unique_key) #50000

#the unique_key identifies one request, because the documenation tells us and
#because the number of distinct unique_keys is equal to the number of observations

# take a look at our columns. what are the data types? 

#we have all character data, but there are date and times as well as numbers, 
#Both reading as characters

# pay attention to the *_date columns. what types are those? 

# the columns are dates and times, but is that their actual data type being recognized? 

#nope, it's being recognized as a character

# let's see how this is a problem by dealing with a seeming easy question: 
# how long did each request take to close? 

# what if we just tried the most straightforward option: subtraction? 

requests%>%
  mutate(time_taken=closed_date-created_date)

#it would not work, because they are not numeric. They are listed as characters

# what's the problem here? 

# we can try another approach, let's use day() to pull the day out of a date. 
# bracket the first row of created_date and let's just use day() on it. 

day(requests$created_date[1])
month(requests$created_date[1])
year(requests$created_date[1])

# what is the value? where is that number even coming from??

# let's take a look at day day(), month(), and year() do under the hood. 
as.Date(requests$created_date[1])

# as.Date tries %Y-%m-%d so it's interpreting "02/07/2023 02:38:31 PM" as 
# July 20th, 0002 AD. 

# this is the problem even if we do it in lubridate
ymd(requests$created_date[1])

# let's figure out what's up with our representation. 
# select created_date and look at the first 10 rows. 

requests%>%
  select(created_date)%>%
  slice_head(n=10)

# look at those 10 lines. before we can choose a parser, what do we need to know? 

#these are written in month/day/year, followed by time, hour:minute:second PM/AM

# took at look at line 4 in the output. 
# 01/12/2023 08:36:16 AM

# what is the date? 

#January 12, 2023 at 8:36 AM

# how could you get evidence from the data itself? imagine you're in a scenario where the
# answer isn't abundantly clear from that output. hint: regex can help with things like this. 

#can use /d-d/ digit digit

requests%>%
  filter(str_detect(created_date,"13/\\d/\\d"))
#returns zero rows, as no rows have 13 as the first digit

requests%>%
  filter(str_detect(created_date,"\\d/13/\\d"))
#just checking

requests%>%
  filter(str_detect(created_date,"/2[0-9]/"))
#second number starting with a 2, demonstrating that they cannot be months.

requests%>%
  filter(str_detect(created_date,"/13/"))
#If the middle number is 13, there is not 13th month, so it has to be day.

# what does your regex idea tell you about the data? 

#there are not entries where 13 appears as the first part of the date string,
#so it must be month, day, year.

# since we've done that, we can actually do our parsing now. 
# create a new column called created. keep created_date. 
# how do we do this? 

requests<-requests%>%
  mutate(created_wrong=mdy_hms(created_date))#shows you local time, but holds UTC time

requests<-requests%>%
  mutate(created=mdy_hms(created_date,tz="America/New_York"))
#time zone defaults to UTC, universal coordinated time zone
#however, this takes place in EST, Eastern Standard Time

requests%>%
  select(created_date,created,created_wrong)%>%
  slice_head(n=10) #spits it out in military time for both

requests$created[1]-requests$created_wrong[1]

# let's check out something interesting about parsing. what are the arguments 
# that are allowed in our mdy_hms() function?

# let's return to our new created column. are there any potential problems here? 
## could we have accidentally created some missing values? 
## what do you need to know to assess this? 

requests<-requests%>%
  select(-created_wrong)

requests%>%
  summarise(raw_missing=sum(is.na(created_date)),
            created_missing=sum(is.na(created)),
            newly_missing=created_missing-raw_missing
            )

# are there any problems that could occur even if we didn't have any missing values at all? 
# use this as an example: 01/05/2023 01:30:00 PM
# could something be potentially risky with that? how can we check on 01:30:00 PM?
requests |> 
  count(hour = hour(created)) |> 
  print(n = 24)

# what does this tell you?

#R is succcessfully reading AM & PM, not just 12 hours total

# briefly, what about chronological operations? 
requests |> 
  summarise(
    earliest = min(created, na.rm = TRUE),
    latest = max(created, na.rm = TRUE)
  )

# what would this give us on the original character column? guess first.

requests |> 
  summarise(
    earliest_string = min(created_date, na.rm = TRUE),
    latest_string = max(created_date, na.rm = TRUE)
  )


# here are a few things to do on your own: 

# first, make a plot of requests per week. however, notice there isn't a week column. 
# how should you make one? 

# second, here is a larger bit of practice. 

## your question: which city agencies close 311 requests fastest?
## you should report: 
### how many requests it received. 
### how many you were able to measure. 
### a typical time to close. 

## you should limit your results to only include agencies that had at least 500 requests.  
### 
