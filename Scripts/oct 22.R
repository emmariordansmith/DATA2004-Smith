# review: APIs, unnesting, hoist, strings, regex, joins
rm(list=ls())
library(tidyverse)
library(httr2)

# the Art Institute of Chicago wants to do some info sessions on their artworks. 
# however, they for some reason given everyone that knows anything about the artworks
# the week off. 

# i am the director, you're all my temps and I need y'all to get this right before 
# i lose my job as the director :(

# when y'all show up to work, you find a sticky note on your desk: 

# 1. pull the records for the 25 pieces on tuesday's tour. but only give me the
# info i actually need 

# 2. clean it up, whenever the last temps pulled it the last time i gave everyone 
# the day off, it was ugly and i had no idea what i was looking at. 

# 3. the gallery labels need each artist's nationality and their dates. 
# they're crammed into one field somewhere, idk where. find them and dig them out. 

# 4. visitors always ask about the artists. that info is all on a different system
# connect them somehow?

# 5. and then, as a favor for me: i'm supposed to be the one giving the tours. 
# but to be honest i don't know anything about these artists, idk why they expect me to. 
# for the favor, i need you to tell me just what i can and can't tell visitors. 

# don't break anything. good luck. my job depends on you. hope you enjoy your first day 
# at the art institute of chicago :)


# from this moment onward, this is your internal monologue. for example, your first thought is 
# "my data2004 professor, Kay Hales, always had such creative and fun prompts. now that i make 
# a ton of money as a data scientist, i think i'm going to send them $5000". 

# your first instinct: probably should ask the API. is it still down? 
request("https://api.artic.edu/api/v1/artworks") |>
  req_url_query(limit = 1, fields = "id") |>
  req_timeout(30) |>
  req_perform()

# what does that mean? 

# thankfully, the people who were already working on this had the tour today already 
# pulled and stored in a json file! 
aic <- jsonlite::read_json("Data/Raw/aic_artworksoct22.json")

# we can't check the API documentation. you know you should still look at the structure 
# of the json file. 

# how do you check the top level of the hierarchy for the entire file? hint: the STRUCTURE of the json file

str(aic,max.level = 1)

str(aic$info,max.level = 1)

# and which of those do you think holds our artworks? 

#data likely holds out artworks

# with that, go ahead and check out the structure of the first element of the list 
# containing the artworks. 

str(aic$data,max.level = 2)

# now you can answer: 
## which fields are single value? 
## which one is a list? 
## named or unnamed? 

#The fields that are a single value are id, title, date_start, date_end, artist_id,
#artist_title

#term_titles is a list, and it is an

# you know the director doesn't understand how to use a json file, so you need to clean it up. 
# you think "this should be one row per piece, one column per thing they need"

# create a tibble, called art_tbl, that takes your list of 25 artworks from where you located it above

art_tbl<-tibble(aic$data)

# how many rows? what does each cell say? 

#25 rows, each cell has a series of those terms I listed above

# you remember that named parts become columns. create an object called art where 
# art becomes one column per field. 
# (if you're stuck, go to chapter 23 of R4DS. you are turning *named* lists into *columns*)

art<-art_tbl%>%
  unnest_wider(`aic$data`)

# if you only want to grab a few things, you know that you can use hoist(). 
# use hoist() on art_tbl to pull out just id, title, artist title, and date_start. 

art_tbl%>%
  hoist(`aic$data`,
        "id","title","artist_title","date_start")

# term_titles is still a list. named or unnamed? so which unnest? 
# make art_terms with one ro per piece term pair. 

#It is a unnamed list, so it can be unnested longer

art_terms<-art%>%
  unnest_longer(term_titles)

art_terms<-art_terms%>%
  distinct(id,term_titles,.keep_all = TRUE)

# the director wants: title, artist, and dates for the gallery labels. 

# look at the columns. you read the artist column carefully before printing any labels. 
# would you put every one of theses on a label as "the artist"? 
# are there any that aren't like the others?

#the only one that seems questionable is the artist_title "English"
#while it could be a last name, it could also be an origin

# then you start by making a date label by gluing start and end together. 
# you remember that you read about 'str_c' at one point
# and that you can str_c(col1, "symbol", col2) and connect col1 and col2 with the symbol
# you create date_label in a new object called labels

art<-art%>%
  mutate(date_label=str_c(date_start,"-",date_end))

# you know you need to check out the new date_label column. there are ugly ones. 
# you know if a piece was made in one year, the label should just say that year. 
# the label should only use a range when there are a range of years. 

art<-art%>%
  mutate(date_label=case_when(date_label=="1794-1794"~"1794",
                   date_label=="1789-1789"~"1789",
                   TRUE~date_label))

#date_start==date_end~date_start???

# you know you can do most of what the director wants can be found in patterns. 

# the director wants to have the pieces laid out thematically (even if those themes aren't very good)

# first, use regex to grab only the titled for unnamed sitters (Portrait of a...)

unnamed_sitters<-art%>%
  filter(str_detect(title,"Portrait of a"))

# use regex to find the artist name with a period in it. 

with_period<-art%>%
  filter(str_detect(artist_title,".\\."))

# then use regex to find the portraits that are of women. 

women<-art_terms%>%
  filter(str_detect(term_titles,"female subject"))

# what about ones that contain "Man"?

contains_man<-art%>%
  filter(str_detect(title,"Man"))

#you use the art_terms object and notice "19th century" is written more than one way. 
# how many ways? 

#19th century, nineteenth century

str(art_terms$term_titles)

art_terms<-art_terms%>%
  mutate(term_titles=case_when(term_titles=="19th century"~"nineteenth century",
         TRUE~term_titles))

# the director wants a tour of men. 
# in art_terms still: find every term with "man" in it

unique(art_terms$term_titles)

men<-art_terms%>%
  filter(str_detect(term_titles,"male subject"))

# did anything make it in that you don't want? 

#contains female subjects still, because female includes male in spelling

men<-art_terms%>%
  filter(str_detect(term_titles,"portraits: male subject"))

# then use anchors, boundaries, and stricter patterns. 