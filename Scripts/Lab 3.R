
# lab 3: acquire and document
rm(list=ls())
library(tidyverse)
library(httr2)
library(rvest)

# you are an intern for Fangraphs (https://www.fangraphs.com/) and they 
# have given your group the opportunity to write a blog post about ballparks. 
# the story is internally called: 'MLB ballparks: 2000 vs now." 

# your supervisor, the editor, has asked you for two things: 
## 1. the data: who played where in 2000, who plays where now, and what today's 
## parks are like. 

## 2. a record of where every number came from, because the last intern asked 
## Gemini to do the story and literally every single number was wrong, which 
## is super embarrassing for a website who really only does numbers. 

# if y'all do well, the supervisor has said that your internships will be extended 
# but if y'all do not do what the supervisor asks, they're going to write 
# bad letters for y'all even if y'all don't ask for them. 

# you will ultimately have three saved files in your data/ path 
# and a data/README.md in that same path.

# you'll start by creating a README. 

# you can find a template I adapted from Cornell's Data Services README
# template on Canvas. 
# you will need to rename it and move it into your project's data/ folder. 
# open it in RStudio and fill it in AS YOU GO. DO NOT do it at the end. 
# when you push at the end of the day, you need to push your README as well. 

# let's do AN API LETS GO!

# MLB's teams endpoint: https://statsapi.mlb.com/api/v1/teams
# parameters: sportId (1 = MLB) and season.
# the story is 2000 vs. now, so you need two seasons: 2000 and 2026.

# build a request for each season: teams_req_2000 and teams_req_2026.
# print each one to check the URL before you send anything.

teams_req_2000<-request("https://statsapi.mlb.com/api/v1/teams")%>%
  req_url_query(format="json",
                sportId="1",
                season="2000")

req_dry_run(teams_req_2000)

req_perform(teams_req_2000,path="Data/Raw/mlb_teams_2000.json")

mlb_2000<-jsonlite::read_json("Data/Raw/mlb_teams_2000.json")

#next
teams_req_2026<-request("https://statsapi.mlb.com/api/v1/teams")%>%
  req_url_query(format="json",
                sportId="1",
                season="2026")

req_dry_run(teams_req_2026)

req_perform(teams_req_2026,path="Data/Raw/mlb_teams_2026.json")

mlb_2026<-jsonlite::read_json("Data/Raw/mlb_teams_2026.json")

print(mlb_2026$copyright)

# remember weekly 5: what does MLB do with a parameter it doesn't know?
# so what are you checking for when you print the request?

# the two requests are the same except for one blank. what's the blank?

#The only difference is the season year that is added as a parameter.

# send both and save each to its own file:
# data/raw/mlb_teams_2000.json and data/raw/mlb_teams_2026.json.
# (hint: req_perform() has a path argument)

# how many requests did you send? how many files did you get?

#We sent two requests and got two files.

# open one file and check that it's the season its name says.
# (hint: read it in, then look at the season of the first team)

mlb_2000$teams[[1]] #the season of the first team is 2000, as the request specified

# log both in your README: URLs, when you requested them, status, terms.
# what are the terms? (weekly 5)

#the terms are : "Only individual, non-commercial, non-bulk use of the Materials is permitted and any other use of the Materials is prohibited without prior written authorization from MLBAM.  
#Authorized users of the Materials are prohibited from using the Materials in any commercial manner other than as expressly authorized by MLBAM.

# from here on, you read the files. you never ask the API again.

#did this above
#teams_json_2026 <- jsonlite::read_json("data/raw/mlb_teams_2026.json")
#teams_json_2000 <- jsonlite::read_json("data/raw/mlb_teams_2000.json")

# look at the top level of one file, then one team, one level at a time.

str(mlb_2000,max.level=1) #list of 30
mlb_2000$teams[[2]]

# you need the same table from both files. instead of writing the hoist twice,
# write a recipe once and use it on both. finish this recipe so it also pulls
# location (locationName), venue_id and venue (inside venue), league and
# division (the name inside each):

teams_table <- \(json) {
  tibble(team = json$teams) |>
    hoist(team,
          id   = "id",
          name = "name",
          location="locationName",
          venue_id=c("venue","id"),
          venue=c("venue","name"),
          league=c("league","name"),
          division=c("division","name")
    ) |>
    select(-team)
}

# test it on one file first. does it look right?

tbl_2000<-teams_table(mlb_2000)

glimpse(tbl_2000)

#it does, it looks great

# now make teams_2026 and teams_2000. how many rows each?

teams_2000<-teams_table(mlb_2000) 

teams_2026<-teams_table(mlb_2026)

#30 rows each

# why write a recipe instead of copying the hoist twice?

#ease of typing, less complicated

# is id a key in each table? check.

teams_2000%>%
  distinct(id)%>%
  count() #there are 30 distinct IDs in 2000

teams_2026%>%
  distinct(id)%>%
  count() #and in 2026

# the editor wants to know what has changed.
# so now we get to do our comparisons :)

# join 2000 to 2026 by team NAME. how many teams match?
# (hint: inner_join. suffix = c("_2000", "_2026") keeps the columns apart)

mlb_joined_name<-teams_2000%>%
  inner_join(teams_2026,join_by(name),suffix=c("_2000","_2026"))

#24 teams matched between 2000 and 2026

# which teams didn't match? (hint: a filtering join keeps the ones that
# DIDN'T match)

mlb_joined_anti<-teams_2000%>%
  anti_join(teams_2026,join_by(name))

#six teams did not match. They were the Oakland Athletics, Tampa Bay Devil Rays,
#Florida Marlins, Anaheim Angels, Cleveland Indians, and Montreal Expos

# are those teams gone? look at them.

#those teams are not present in the joined by name dataset. However, their IDs
#still exist in 2026 under different names.

# now join by id. call it then_now. how many match?

then_now<-teams_2000%>%
  inner_join(teams_2026,join_by(id),suffix=c("_2000","_2026"))

# what does that tell you about names vs. ids as keys?

#ID identifies one individual team, regardless of name and location changes. 
#name only represents the name of a team at a given point in time.

# how many teams changed their name? their city? their league? which ones?

then_now%>%
  filter(location_2000!=location_2026) #Oakland and Montreal changed locations

then_now%>%
  filter(name_2000!=name_2026) #the Athletics, Rays, Angels, Indians, Expos, and Marlins
#all changed their names, in addition to 5 others

then_now%>%
  filter(league_2000!=league_2026) #Houston Astros changed leagues

# how many teams play in a park with a different NAME than in 2000?

venue_change<-then_now%>%
  filter(venue_2000!=venue_2026) #23 teams play in a differently named park between
#2000 and 2026

# a new name isn't always a new park: Safeco Field became T-Mobile Park, and
# it's the same building. how could you tell a renamed park from a new one
# using this data? try it. what did you find?

#venue Id!

#you could filter out venue names that don't match, and then determine whether 
#or not the venue IDs also match or don't match

venue_change%>%
  filter(venue_id_2000!=venue_id_2026) #14 of the venues which changed name
#do not have the same venue id in both years, indicating the physical park changed

# https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums
# before you scrape: same checks as thursday. robots.txt, the license, and
# "view page source." write the answers in your README.

# get the page and save it to data/raw/mlb_stadiums.html, then read the saved
# file into R and call it stadium_page.
url <- "https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums"

request(url) |> 
  req_user_agent("Danielle Pruitt, DATA 2004 class, Transylvania University") |> 
  req_perform(path = "Data/Raw/mlb_stadiums.html")

stadium_page <- read_html("Data/Raw/mlb_stadiums.html")

# when was the page last edited? put it in your README.


# get all the data tables as tibbles. how many are there?
# There are 3 tables.
tables <- stadium_page |> 
  html_elements("table.wikitable") |> 
  html_table()

tables[[1]] 

# which one is the current stadiums table? check before you trust a number.
# The first element is the current stadiums table.

# call it stadiums_raw.
stadiums_raw <- tables[[1]]
View(stadiums_raw)

# look at Name, Capacity, and Distance to center field. what's in them that
# isn't a name or a number?
# In Name, there are symbols after some of the names. In Capacity and Distance,
# we have numbers in brackets. Also, in Distance, we have a conversion into   
# miles.

# clean it. keep the originals; make new columns.
# the park name, without the symbols:

stadiums_raw |>
  mutate(park = str_squish(str_remove_all(Name, "[‡†]"))) |>
  select(Name, park)

# what does [‡†] match? (what does [ ] mean in regex?)
# [‡†] matches all strings that contain ‡†. 

# now make stadiums with: park, team, capacity (a number), opened (an
# integer), center_ft (a number), and roof.
# (hint: parse_number() pulls the first number out of text like "41,900" or
#  "400 feet (122 m)")
stadiums <- stadiums_raw |>
  mutate(park = str_squish(str_remove_all(Name, "[‡†]"))) |>
  mutate(capacity = parse_number(Capacity)) |>
  mutate(opened = as.integer(Opened)) |> 
  mutate(center_ft = parse_number(`Distance to center field`)) |>
  select(park, Team, capacity, Opened, center_ft, `Roof type`)


# did R give you a warning? don't skip it. which parks got NA, and what does
# their original cell say? (hint: go back to stadiums_raw)
# Yes, the parks that had footnotes got na values.


# if you ignored that warning, what would happen when you look for the oldest
# park later? (this is how the last intern got fired.)


# look at the Team column too. is anything stuck to the names?
# The Los Angeles Dodgers and Boston Red Sox have brackets and nb and then a
# number next to them.

# fix both. thursday's footnote pattern will help with one of them.
# check that nothing is NA anymore.

stadiums <- stadiums |> 
  mutate(team_name = str_remove(Team, "\\[.*\\]")) 


# now you have two tables from two places. the editor wants one.

# join stadiums to teams_2026 by park name. which parks DON'T match?
# (hint: anti_join, and join_by(park == venue))

single_table<-teams_2026%>%
  inner_join(stadiums,join_by(venue==park)) #only 28 matched

non_matches<-teams_2026%>%
  anti_join(stadiums,join_by(venue==park)) 

# why didn't they match? look closely at both spellings.

#Miami Marlin's stadium is just not spelled the same. There's no whitespace in one.
#the Dodger's stadium has a sponsor name in one and not the other

teams_2026<-teams_2026%>%
  mutate(venue=case_when(venue=="UNIQLO Field at Dodger Stadium"~"Dodger Stadium",
                        venue=="loanDepot park"~"LoanDepot Park",
                        TRUE~venue))


# now try team name instead. which don't match?

single_teams<-teams_2026%>%
  inner_join(stadiums,join_by(name==Team))

anti_team<-teams_2026%>%
  anti_join(stadiums,join_by(name==Team))

#Dodgers and Red Sox both have brackets and such in their names

# which key would you trust, and why? what would you do with the misses?
# write your decision in the README.

#You would likely trust the teams 

# make parks: every 2026 team with its park's details. how many rows?
# should it be 30?

parks<-teams_2026%>%
  select(name,id,venue,location)%>%
  inner_join(stadiums,join_by(venue==park))

#it should have thirty rows, and it does.

# time to answer the editor.

# what are the three oldest parks? the three newest?

parks<-parks%>%
  mutate(opened_year=str_extract(Opened,"\\d+"))

parks<-parks%>%
  mutate(opened_year=as.numeric(opened_year))

parks%>%
  arrange(desc(opened_year))%>%
  head(3)

#the newest parks belong to the Rangers, Braves, and Marlins

parks%>%
  arrange(desc(opened_year))%>%
  tail(3)

#The oldest parks belong to the Dodgers, Cubs, and Red Sox

# how many teams play in a park that opened after 2000?

parks%>%
  filter(opened_year>=2000)%>%
  print(n=30)

#17 teams play in a park opened in 2000 or after.

# the Athletics are playing in a minor league park while a Las Vegas
# stadium is built. what does "current stadium" mean, and who decides? how
# would your answers change if you'd scraped this page a year ago?

#current stadium means current functional stadium being used. If you scraped this
#a year ago, you may have gotten their old stadium name as their stadium.

# save your final table. this is what you hand the editor.
# (raw data goes in data/raw/. data you made goes somewhere else.)

parks<-parks%>%
  select(name,id,venue,location,opened_year,capacity,`Roof type`)

write_csv(parks, "Data/Processed/mlb_parks_2026.csv")

# why keep it separate from data/raw/?

#because it is not raw. It has been processed, and you don't want to confuse it if
#you were to need it again.


# last thing before you push: finish your README. the editor is going to hand
# it to a fact-checker.
