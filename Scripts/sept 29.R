# let's start with a quick review of Weekly 2

# Regular expressions and messy-key repair

rm(list=ls())

library(tidyverse)

pa <- read_delim(
  "Data/Raw/fec_pa_2024_strings.psv",
  delim = "|",
  quote = "",
  na = character(),
  col_types = cols(.default = col_character())
)

# we'll work with pa again. 
glimpse(pa) 

# what's the grain? 

#a single recorded contribution of over $200 to a campaign

# let's work with occupation rather than employer this time, and we're specifically 
# interested in engineers. 

# let's just start with what seems like a simple question: 
# how many contribution records report an engineering occupation? 

# how should we start this? 
pa |> 
  filter(occupation == "Engineer")

# why zero? what should we do now? 

#that may not be exactly how "engineer" was recorded by every individual.

pa%>%
  filter(occupation=="ENGINEER")

#986 rows listed for this

# are there problems if we left it at this? 
# what does occupation contain? could we want more than that? 

pa%>%
  select(occupation)%>%
  print(n=100)

#different types of engineer, like software, or tech/engineering specialist

# we ultimately don't just want the literal string ENGINEER. we want records where 
# the pattern E-N-G-I-N-E-E-R shows up anywhere. 

# so let's start by using str_detect. 

engineer_candidates<-pa%>%
  filter(str_detect(occupation,"ENGINEER")) #1318 total

#filters all containing the "ENGINEER"

# is there a chance that any of our occupations aren't in full caps? 
pa |> 
  summarise(
    caps_match = sum(str_detect(occupation, "ENGINEER")),
    ignore_match = sum(str_detect(occupation, regex("engineer", ignore_case = TRUE)))
  )
#all 1318 have the same

# what does regex(...) do? what does the output from the code above tell us?

# let's continue working with our engineer_candidates 
# we've got our pattern for engineer, but should this be the end all be all? 
engineer_candidates |> 
  select(occupation) |> 
  print(n = Inf)

# take a look at some of our matches. Are all of these engineers? 
## SENIOR DIRECTOR, RELIABILITY ENGINEERING
## STRATEGY ENGINEER
## SITE RELIABILITY ENGINEER 
## ENGINEERING ASSISTANT
## V.P. SOFTWARE DEVELOPMENT & ENGINEERING


# we still have things like "ENGINEERING" because "ENGINEER" is still literally part 
# of <ENGINEER>ING. we can use word boundaries to start dealing with this. 

# in regex, a word boundary is read like \b, but '\' already means something in R
# so we will need to use \\b to tell R we aren't wanting to use \ how R wants to use it. 

#cat("\d") # \ means 'escape', with \n tells R to start a new line
# uncomment the above line to test it, but comment it back after. it'll throw an error 
# if you try to run this script at another time. 
cat("\\d")

# but this doesn't exactly tell us what a word boundary does yet. 
str_replace_all(c("STRATEGY ENGINEER", "ENGINEERING ASSISTANT",
                  "V.P. & SOFTWARE", "SENIOR DIRECTOR", "SITE RELIABILITY ENGINEER",
                  "ENGINEER-3", "ENGINEER_3"),
                "\\b", "|") #spaces, hyphens, and non-words

# we told R to replace word boundaries with |. So what is counting as a word boundary 
# based on what we're seeing there? note: letters, digits, and _ also count as 
# "word characters" 

# so if we wanted to only retain occupation strings that contain <ENGINEER> what would we 
# need to do? 

# start back with pa. how many records would we retain if we tried: exactly "ENGINEER",
# contains "ENGINEER", and "ENGINEER" as a whole word. 

engineer_candidates%>%
  filter(str_detect(occupation,"\\bENGINEER\\b"))%>%
  print(n=50)

pa%>%
  summarise(exact_matches=sum(occupation=="ENGINEER"),
            pattern_match=sum(str_detect(occupation,"ENGINEER")),
            whole_word=sum(str_detect(occupation,"\\bENGINEER\\b")))

# the word boundary got rid of ENGINEERING. what else did it get rid of? 
engineer_candidates |> 
  filter(!str_detect(occupation, "\\bENGINEER\\b")) |> 
  count(occupation, sort = TRUE) |> 
  print(n = Inf)

# are these all non-engineer? did we lose anything we wanted? 

#these all have "engineering" but not just the word Engineer. We could want 
#just "Engineering" to stay or "software engineer"

# \\b is part of regular expressions, but there are a variety of other regex 
# we will need to use. we'll start with anchors 

# our first two anchors will be ^ and $ where 
# ^ is the start of the string and $ is the end. 

x <- c("ENGINEER", "CIVIL ENGINEER", "ENGINEERING ASSISTANT", "ENGINEER II")

# which of the above examples will each of these give, before running anything?
str_view(x, "^ENGINEER")
str_view(x, "ENGINEER$")
str_view(x, "^ENGINEER$")
str_view(x, "^ENGINEERS$")


pa%>%
  summarise(exact_matches=sum(occupation=="ENGINEER"),
            pattern_match=sum(str_detect(occupation,"ENGINEER")),
            whole_word=sum(str_detect(occupation,"\\bENGINEER\\b")),
            anchor=sum(str_detect(occupation,"^ENGINEER$")))


# there are a few other regex options that we will need to know. 
## \\d = any digit 
## + = one or more of the preceding thing 
## | = or 
## . = any character 
## \\s = any whitespace 
## ? = optional (zero or one of the thing)
## () = a group of things 
## \\. = an actual period 

# let's use some of these in examples. we'll start with extraction. 

# everything so far has been a TRUE/FALSE question: does a string match?
# some of our occupations include things like seniority, e.g., ENGINEER 1 vs ENGINEER 2
# let's create a new column called level that tells us seniority. 
engineer_candidates |> 
  select(occupation) |> 
  filter(str_detect(occupation, "\\d")) |> 
  mutate(level = str_extract(occupation, "\\d+")) # why + here? 

# it worked, but do you think we got all of seniority? 
# how would you want to look for more? 

#some duplicates, issues with roman numerals, and job categorizations (principal?)


# a few more things if we have time or for your review if we don't have time. 
# think about the problems we had in employer. the gaps were more than 
# case and space, there were punctuation problems as well. 

employer_punct <- pa |> 
  mutate(
    employer_key = employer |> 
      str_remove_all("[[:punct:]]") |> 
      str_squish() |> 
      str_remove("\\s+(LLP|LLC|CORP|INC|LP)$")
  )

employer_punct |> 
  filter(employer != employer_key) |> 
  count(employer, employer_key, sort = TRUE) |> 
  print(n = 40)


