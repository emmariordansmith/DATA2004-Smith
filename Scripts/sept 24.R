# Strings and categorical representation

rm(list=ls())

library(tidyverse)

pa <- read_delim(
  "Data/Raw/fec_pa_2024_strings.psv",
  delim = "|",
  quote = "",
  na = character(),
  col_types = cols(.default = col_character())
)

# only contributions > $200 are included
# only three of the original columns are there. record_id is mine, not the FEC's
# helps to protect anonymity. 

glimpse(pa)

nrow(pa)
n_distinct(pa$record_id)

# What does one row represent? Is one row one contributor?

#one row is one distinct record id, or one distinct contribution record
#greater than $200 from PA in 2024, not one distinct contributor. 

# Which variable are we interested in today?

#we are interested in employers

# Our task: who employs the people giving money to campaigns?

pa |> 
  count(employer, sort = TRUE) |> 
  print(n = 75)

# Same question as the vehicles. Is this counting employers?

#No, this is counting the number of appearances of each employer/self-employed

# Look for ways someone could say they are self employed. 

#self-employed, self employed, self
#not employed, unemployed,N/A,None, Not-Employed

# What about unemployed? 

n_distinct(pa$employer)

# Does that mean there are 6,950 different employers?

#no, because there are various different ways of writing certain options

# Normalizing representation
examp_text <- "Hello, this  is an example. "

str_to_upper(examp_text) #all upper case, also works witl lower
str_trim(examp_text) #dropped trailing space
str_squish(examp_text) #removed double and trailing space

# let's create employer_normalized, where we make every observation in all caps
# and we remove trailing and leading white spaces. 

pa_clean<-pa%>%
  mutate(
    employer_normalized=employer%>%
      str_to_upper()%>%
      str_squish()
  )

n_distinct(pa_clean$employer_normalized) #only removed about five different ones

# That barely changed. Why not?
# What does that tell you about how this data was recorded?

#there are many more differences by purpose than by accidental spaces 

# What's actually inconsistent?
pa |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF",
      "ELECTRICIANS LOCAL 98", "ELECTRICIANS LOCAL98",
      "HIGHMARK INC", "HIGHMARK, INC.", "HIGHMARK HEALTH"
    )
  ) |> 
  count(employer, sort = TRUE)

# Which of these differences are just representation?

#differences that are between hypenation are differences in representation it seems.
#but INC versus Health may be a difference that isn't just representation

# Which might actually mean different things?

#Highmark Inc, versus Highmark Health

# Which would you change without outside information?

#self employed, not employed, and local electritions should respectively be
#consolidsted for spacing and hyphens

# Punctuation 
## let's get rid of those hyphenations now. 
# we can use str_replace_all(fixed()) to do this. 
# should we use str_squish again?

pa_clean <- pa_clean |> 
  mutate(
    employer_no_hyphen = employer_normalized |> 
      str_replace_all(fixed("-"), " ") |> #instead of doing regular expression
      str_squish()
  )

pa_clean |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF"
    )
  ) |> 
  count(employer_no_hyphen, sort = TRUE)

# What collapsed? What didn't?

#Self and Not employed were collapsed, but not SELF

# SELF looks like it belongs with SELF EMPLOYED. Does it?
# What if Self is a company? An abbreviation for something else?

#that is context you need to look up or already know

# Removing a hyphen changed how two words were written.
# Merging SELF would be a claim about who these people are.


# Exact replacement
# When you know exactly which value you want to change, change that
# value and nothing else.

# Here's where we can use case_when()

pa_clean <- pa_clean |> 
  mutate(
    employer_clean=case_when(
      employer_normalized == "NOT-EMPLOYED" ~ "NOT EMPLOYED",
      employer_normalized == "SELF-EMPLOYED" ~ "SELF EMPLOYED",
      employer_normalized == "ELECTRICIANS LOCAL98" ~ "ELECTRICIANS LOCAL 98",
      employer_normalized == "HIGHMARK, INC." ~ "HIGHMARK INC",
      TRUE ~ employer_clean
    )
  )

# Notice this matches on employer_normalized, not employer_no_hyphen.
# If you had already stripped punctuation, would
# "HIGHMARK, INC." still be there for case_when to find?

#no it would not because R can only pull from what you tell it to


# Why keep raw column?
pa_clean |> 
  filter(employer != employer_clean) |> 
  count(employer, employer_clean, sort = TRUE) |> 
  print(n = 61)

# I wrote four replacement rules. Why did more rows than that change?

# Find ORRICK HERRINGTON  SUTCLIFFE LLP. Why is there a double space
# in the middle of a law firm's name? What did str_squish() do to it?

pa_clean |> 
  select(employer, employer_normalized, employer_clean) |> 
  distinct() |> 
  arrange(employer_clean) |> 
  print(n = 30)

# What would we lose if we overwrote employer?

#you would lose the original

# Weekly 2

coffee <- read_csv("data/coffee_ratings.csv")

coffee |> 
  count(producer, sort = TRUE)

n_distinct(coffee$producer)

# Tuesday: Regular expression 

# Today I told you which values to look at. What if I hadn't?
# Would you read all 6,950?

# Tuesday: how do you find suspicious or related values without
# reading every one?