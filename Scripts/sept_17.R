library(tidyverse)

crashes <- read_csv("Data/Raw/crashes.csv")
persons <- read_csv("Data/Raw/person.csv")

glimpse(crashes)
glimpse(persons)

# What does one row represent in each table?
#In Crashes, one row represents one crash in new york city
#in persons, one row represents one recorded person involved in a given crash

# What variable appears in both? Does it do the same job in both?


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

# You work for the NYC Department of Transportation and you have been given a media request: 
# How do person-level injury outcomes compare across boroughs? 

# Can you answer this with one dataframe alone? Probably not, or else we wouldn't do this on 
# our join day :)

# So we have to put these together. To do this, we have to work with keys. 

# A PRIMARY KEY uniquely identifies a row in its own table.
# A FOREIGN KEY points at another table's primary key.

# How should we figure out which one COLLISION_ID is?
#collision_id is a primary key for crashes, but a foreign key for persons
#the primary key of persons would be unique_id

crashes_core%>%
  count(COLLISION_ID) #no repeat, primary

persons_core%>%
  count(COLLISION_ID) #must repeat

persons_core%>%
  count(UNIQUE_ID) #no repeat, primary

# Why does COLLISION_ID repeat in the person table?
#because multiple people could be involved in one singular crash

# Could we try to learn something about that wreck with 52 people in it? Let's use the persons df
persons_core%>%
  count(COLLISION_ID)%>%
  arrange(desc(n)) #ID number 4724524 appears 51 times

persons%>%
  filter(COLLISION_ID==4724524)

# Do we have any missing keys?
crashes_core%>%
  filter(is.na(COLLISION_ID)) #none missing

persons_core%>%
  filter(is.na(COLLISION_ID)) #none missing

persons_core%>%
  filter(is.na(UNIQUE_ID)) #none missing

# Cardinality

# Cardinality is how many rows on each side can share a key value. 
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? What does that tell you about what a join will
# do to our rows?

nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference
# tell you before we join anything?

# Let's do our join. What should we join by? What's your guess for the number of rows? 
#the number of rows would be the same as the number of unique IDs in persons

# We'll start by doing a join that keeps observations where both cases exist. 
# Can anyone remember which join this is? 

# Use the console if you don't remember. 

#mutating joins, inner join or left join
#inner join is join matches across both, drops any that don't
#left join is join matches but does not drop those that don't match (NA)

crashes_inner<-crashes_core%>%
  inner_join(persons_core,join_by(COLLISION_ID))

#crashes must be left of the pipe because persons has the repeating collision IDs
#one to many! 

#You keep the grain and population of the lefthand join

# What does one row represent now? Is that the same as before?
#one row still represents one individual person in a given crash

persons_inner<-persons_core%>%
  inner_join(crashes_core,join_by(COLLISION_ID))

#one person with crash level information

# Now the other one. inner_join keeps rows that matched in BOTH tables.
# Which one keeps every row of the LEFT table whether it matched or not.

crashes_left<-crashes_core%>%
  left_join(persons_core,join_by(COLLISION_ID))

glimpse(crashes_left)

# Where did the difference come from?

# What is one row in crash_people_inner? What is one row in
# crash_people_left? Are they the same question?

nrow(crashes_left)
sum(!is.na(crashes_left$UNIQUE_ID))

#left join has more rows than inner join. We know that some crashes do not
#have person level data involved

# If someone asked how many people were involved in crashes, which of
# those two numbers would you hand them?

# When would you want inner_join? When would you want left_join?
#you would want inner join when you didn't want records that aren't exact matches
#left join is useful when those non matches are still useful data

# We said this was one-to-many. We can say that in the code and make R
# check it for us.

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-many")

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-one")

# What happened on the second one? Why would you want that?

# COVERAGE is which rows on each side found a match. The table got
# bigger, so nothing was lost, right?

crashes_core |> #crashes without people
  anti_join(persons_core, join_by(COLLISION_ID)) |> 
  nrow()

persons_core |> #people without crashes
  anti_join(crashes_core, join_by(COLLISION_ID)) |> 
  nrow()


# anti_join keeps left rows with NO match and adds no columns. Does the
# order matter here? Are those two lines asking the same question?

# What kind of crash has no person records?

# We can make R shout about this one too.

persons_core |> 
  left_join(crashes_core, join_by(COLLISION_ID), unmatched = "error")

# What does that argument buy you?

# The four mutating joins:
# inner_join = rows that matched in both
# left_join = every row in the LEFT table, matched or not
# right_join = every row in the right table
# full_join = everything from both

# Given all that, which one do you want for person-level outcomes with
# borough attached? Which table goes on the left?

##### Your turn #####

# Build a person-level table that includes borough.

# Before you write anything:
## What should one row represent when you're done?
## Which table goes on the left?

#one row should represent one person
#persons goes on the left, as it is the grain to maintain

# Then:
## Join them.
## Declare the cardinality with relationship = and see if R agrees.
## Use anti_join() to look at whatever didn't match.
## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.

persons_borough<-persons_core%>%
  left_join(crashes_core,join_by(COLLISION_ID),
            relationship="many-to-one")

persons_core%>%
  anti_join(crashes_core,join_by(COLLISION_ID))
#nothing lacked a match

persons_borough%>%
  count(BOROUGH)

persons_borough%>%
  count(PERSON_TYPE)

persons_borough%>%
  count(PERSON_INJURY)

# Some of those rows will have no borough. Before you filter them out:
# how many are there, and are they all missing for the same reason?

#there are 103,229 rows with no borough, which are missing because the bororugh
#was not recorded in the dataset crashes, and so has been expanded to all individuals
#in that crash