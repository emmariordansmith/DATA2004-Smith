# Lab 2: is this file ready to analyze?

rm(list=ls())

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key

libraries <- read_csv("Data/Raw/PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one library system that was in operation for the given year, 2024.

# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)

#the extra rows are to represent branches, locations, bookmobiles, and other library operations

# which column should identify a row?

n_distinct(libraries$FSCSKEY)

#the column that identifies a row is FSCSKEY, which has a number of unique values
#equal to the number of observations in 'libraries'

# what did those tell you?
# what would it have meant if the count() came back with rows?

#If count() came back with rows instead of distinct ones, you may have a mistaken
#answer that is not counting the distinct values within the column.


# let's focus on VISITS for now. what does this column mean?

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?

#the values that would be impossible could be zero and negative values

libraries |>
  summarise(
    min_visits = min(VISITS, na.rm = TRUE),
    max_visits = max(VISITS, na.rm = TRUE)
  )

libraries |>
  filter(VISITS < 0) |>
  count(VISITS)

# how many different negative values? how many rows of each?

#5 instances of -3 visits, and 69 instances of -1 visits

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?

#in the case of -3, it indicates a missing value. -3 is used in place of NA to 
#represent the data of libraries that were closed for the given year.

#-1 is used as a non-response, and is unrecorded. So it is NA

# does R think any of these are missing?

is.na(libraries$VISITS) |>
  table()

#R does not believe any of these are missing, because none are NA listed.

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?

#the coded numbers are giving us the reason that something is missing rather than
#something is missing in general.

# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?

# do the two codes mean the same thing?

#No, the two codes do not mean the same thing, -3 indicates a closed location
#while -1 indicates missingness (NA)

# every numeric column has a flag column. find the one for VISITS.

libraries%>%
  select(F_VISITS)%>%
  distinct()

#the flag column for visits has eight different flags.

# what does the flag tell you? does it tell the two codes apart?

#the flag tells you any extra details about the value present in VISITS
#so for R_24, it indicates that "the data was reported but not imputed"

# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?

#this allows us to be specific and catch any other odd values

# both codes just turned into NA. what did we lose?
# where can we still find it?

#We can still find it in the original VISITS column

# did it do what we meant? three questions:
# same number of library systems?
# did every code become NA?
# did anything else become NA?

# does any of this matter?

libraries%>%
  select(visits_clean)%>%
  summarise(na=sum(is.na(visits_clean)))


#There are the same number of observations, as we only mutated a new column.
#All listed as -3 or -1 became NA. There are now 74 NAs in visits_clean, which is 
#equal to the added 69+5.
#Nothing else became NA.
#This matters because if you made a mistake you can catch it here.

# why is the raw mean lower? what's in each denominator?

libraries%>%
  summarise(mean_vis=mean(visits_clean,na.rm=TRUE),
            mean_raw=mean(VISITS,na.rm=TRUE))

#mean is total divided by the number of observations. The raw values include negative
#numbers which takes down the numerator of the calculation. The denominator of the
#calculation is the number of observations, and without NA, the clean value is higher

# your turn :)
# get in your group project groups

# do that process for each of the following:
# TOTATTEN - program attendance
# TOTPRO - number of programs
# TOTCIR - total circulation

# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

#TOTATTEN
libraries |>
  summarise(
    min_attend = min(TOTATTEN, na.rm = TRUE),
    max_attend = max(TOTATTEN, na.rm = TRUE)
  )

libraries |>
  filter(TOTATTEN < 0) |>
  count(TOTATTEN)

libraries <- libraries |>
  mutate(totatten_clean = if_else(TOTATTEN %in% c(-1, -3), NA_real_, TOTATTEN))

#TOTPRO
libraries |>
  summarise(
    min_program = min(TOTPRO, na.rm = TRUE),
    max_program = max(TOTPRO, na.rm = TRUE)
  )

libraries |>
  filter(TOTPRO < 0) |>
  count(TOTPRO)

libraries <- libraries |>
  mutate(totpro_clean = if_else(TOTPRO %in% c(-1, -3), NA_real_, TOTPRO))

#TOTCIR
libraries |>
  summarise(
    min_circ = min(TOTCIR, na.rm = TRUE),
    max_circ = max(TOTCIR, na.rm = TRUE)
  )

libraries |>
  filter(TOTCIR < 0) |>
  count(TOTCIR)

libraries <- libraries |>
  mutate(totcir_clean = if_else(TOTCIR %in% c(-1, -3), NA_real_, TOTCIR))

# what should it measure? what would be impossible?

#TOTATTEN should measure people attended. It should not be negative
#TOTPRO is the number of programs put on, and also could be zero but not negative
#TOCIR is measuring circulation, and will not be negative, but could be zero

# range. any odd values? how many of each?

#aside from the odd values of -3 and -1 appearing in each column like above,
#it seems odd that a million people may have attended one library event, or that
#90000 programs were put on. But according to the codebook, only -1 and -3 are 
#indicative values in these columns

# what does the user's guide say they mean?
# ("we couldn't find it" is an answer. "we assumed" isn't.)

#Only -1 and -3 are values that indicate missingness or other conditions in the codebook

# what's the flag column? (the names get shortened. look for it.)

libraries%>%
  select(F_TOTATT)%>%
  distinct()

libraries%>%
  select(F_TOTPRO)%>%
  distinct()

libraries%>%
  select(F_TOTCIR)%>%
  distinct()

# clean version. keep the raw column.

# check it: same rows? every code became NA? nothing else did?

libraries%>%
  select(totatten_clean)%>%
  summarise(na=sum(is.na(totatten_clean))) #perfect

libraries%>%
  select(totpro_clean)%>%
  summarise(na=sum(is.na(totpro_clean))) #great

libraries%>%
  select(totcir_clean)%>%
  summarise(na=sum(is.na(totcir_clean))) #correct

# mean before and after. big change or small?

libraries%>%
  summarise(mean_atten=mean(totatten_clean,na.rm=TRUE),
            mean_raw=mean(TOTATTEN,na.rm=TRUE))

#Total attendance mean is greater than the raw

libraries%>%
  summarise(mean_pro=mean(totpro_clean,na.rm=TRUE),
            mean_raw=mean(TOTPRO,na.rm=TRUE))

#total program mean is greater than the raw

libraries%>%
  summarise(mean_cir=mean(totcir_clean,na.rm=TRUE),
            mean_raw=mean(TOTCIR,na.rm=TRUE))

#total circulation mean is much much larger than the raw value. 

#ALL three of these values are greater because the denominator of the calculation
#is lower than for the raw values in the data

# where are they? 

#they are now listed as NA and not included in calculation!

# recoding to NA fixes the number. does it finish the job?

#no, because there could still be other issues within the data.

# are the coded rows spread out, or do they bunch up?

libraries%>%
  group_by(STABR)%>%
  summarise(na_visits=sum(is.na(visits_clean)),
            na_attend=sum(is.na(totatten_clean)),
            na_cir=sum(is.na(totcir_clean)))%>%
  print(n=56)

#There are occasional missing values throughout the states, but PR and CA have
#specific clusters of missing visit data, and many other states
#have clusters of missing circulation data as well. This indicates bunched up 
#rows of missingness.

# if someone compares circulation across states, what goes wrong?

#comparing circulation among states does not allow for consideration of library size,
#number of branches, number of books, population of that state, or any other consideration
#that affects circulation.

# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.

#As is, it is not ready to analyze, because there are more columns that have 
#flagged and missing values than we have already corrected.
#In addition, the missingness clustered at certain states and certain columns indicate
#that the calculations you do to compare libraries,
#even if you compare library systems within the same state, may not be 
#able to be done because of the missing data.
#Because of the missingness and all the other flags we have yet to cover,
#the file is not yet ready to analyze.


