# web scraping 
library(tidyverse)
library(httr2)
install.packages("rvest")
library(rvest)

# let's mess around with Wikipedia today :) paste this link in your browser: 
# https://en.wikipedia.org/wiki/List_of_Cincinnati_Reds_seasons
# scroll around, get familiar with it before we start doing any coding. 

# and then before we keep going, we need to talk a bit about ethics. 
# is this Wikipedia page public? is is non-personal? is it factual?

# now let's open https://en.wikipedia.org/robots.txt
# this isn't a law, but it's what is expected for bots. we aren't bots, hopefully. 
# since we aren't bots, does this apply to us? are we allowed? 
# but we should respect Wikipedia's instructions and preferences.

# then, ctr/cmd + f for "Folks get annoyed". what's that mean? 

# lastly, flip back to the main wikipedia page for the reds' seasons. 
# scroll to the very bottom. what license is the text under? 
# what do you owe wikipedia if you use it? 

# let's get started!
# we'll start with some practice with html. 
# html is how web pages get built, and it's bascially a text with tags. 
tiny <- minimal_html('
  <h1>Reds</h1>
  <p class="note">Founded in <b>1882</b>.</p>
  <p class="note" id="nickname">Also called the <a href="/wiki/Redlegs">Redlegs</a>.</p>
  <p>Not a note.</p>
')

# the tags: 
# <h1>: biggest heading 
# <p>: a paragraph 
# <b>: bold 
# <a>: a link

# these tags also define elements. for example 
# <p> ... </p> 
# the element is the start tag, <p>, the end tag, </p>, and everything between them. 

# take a shot: what elements are in tiny? 

# we also have ATTRIBUTES. an attribute is extra information inside the start tag
# for example, name="value". 
# what are three attributes in tiny? which element is each one in? 








# two of our attributes are special: class and id
## elements can share a class 
## but only one element on a page should have a given id. 
# which elements have class "note"? which has id "nickname"? 

# class and id are usually used for styling and to help with scraping. 
# a class is like a team name. everyone on the reds shares the class team name. 
# but players don't share names or jersey numbers, for example

# we can find elements with CSS selectors. we have three kinds 

## we can do it by class name 
tiny |> 
  html_elements("p") #returns the three paragraph elements in entirety

## we can do it by class by putting a dot in front
tiny |> 
  html_elements(".note") #returns elements with class=note

## and we can do it by id by putting a # in front
tiny |> 
  html_elements("#nickname") #returns elements with id in them

## how many elements did each one find? 

# and to clarify, we have two separate element selectors
tiny |> 
  html_element("p") #first element in a paragraph

tiny |> 
  html_elements("p") #elements plural, all

# what did those separately find? look back at tiny and tell me. 

# and now let's get what's inside our elements 
tiny |> 
  html_elements(".note") |> #returns elements where class=note
  html_text2() #returns text elements, not structure 

# this sets up the basic 2 steps of web scraping: 
# 1. find the elements
# 2: pull something out of them. html_text2() for text, html_attr() for an attribute. 

tiny |> 
  html_elements("a") |> #elements <a> that is a link
  html_attr("href") #pulls out attribute

# before we move on to our larger application: get some practice. 
# get the text of h1

tiny%>%
  html_elements("h1")%>%
  html_text2()

# get the text of the bold element 
tiny%>%
  html_elements("b")%>%
  html_text2()

# get the text of ONLY the paragraph with id nickname
tiny%>%
  html_elements("#nickname")%>%
  html_text2()

# get the text of every paragraph. how many? which one isn't a note? 
tiny%>%
  html_elements("p")%>%
  html_text2() #the third isn't a note

# now let's apply this by focusing on the reds again. 

url <- "https://en.wikipedia.org/wiki/List_of_Cincinnati_Reds_seasons"

request(url) |> 
  req_user_agent("Emma Smith, DATA 2004 class, Translyvania University") |> 
  req_perform(path = "Data/Raw/reds_seasons.html")

#pulled 10/29/26 at 11:48 AM

# why do you think we immediately save it? does it being a wiki matter? 

# now let's read the file and work from there 

reds_page <- read_html("Data/Raw/reds_seasons.html")

# let's use our selectors. y'all first:
# first, what's the page title? 

reds_page%>%
  html_elements("h1")%>%
  html_text2()

# second, go back to the wiki and go down to the bottom where it says "This page was last edited"
# right click that text and then click inspect. 
# what's its id? use that to get that text here in here. 

reds_page%>%
  html_elements("#footer-info-lastmod")%>%
  html_text2()

# third, how many tables are on the page? 

# fourth, right click the big season table, click inspect. 
# what class does <table> have? how mayn tables have that class? 

# let's turn this into a tibble 
tables <- reds_page |> 
  html_elements("table.wikitable") |> 
  html_table()

# html_table() turns an HTML table into a table. if we had a bunch of tables, 
# we'd get a list of tibbles. 

length(tables)

# we'll use our indexing and positions again. we have a length of 5. so let's start with 
tables[[1]] #desc of table headers and inclusions
tables[[2]] #sub table above the one we want
tables[[3]] #the data we want

# use that to find the season table. once you find it, call that seasons_raw

seasons_raw<-tables[[3]]

# now, look at seasons_raw. scroll all the way through it. 
# notice anything odd? anything we should note? 

#years with no data have no proper NA
#denotations of team names are interspersed within the data
#footnotes included
#possible duplication of rows

# then, what type is the Season column? Wins? why? 

# let's start cleaning by removing footnotes. let's use regex.

# something that will help us: ".*" = any character. 
# so: ".*+" means what exactly? 

# and then what was ^? and what was \\d?

# let's look back at our table. season has footnotes stuck to it. 
# so we get things like "1972[e]". how should we remove them? 

seasons_raw |> 
  mutate(season = str_remove(Season, "\\[.*\\]")) 

# so we're just asking for \\[ = an actual opening bracket 
# then we want .* so any character
# \\] = a literal closing bracket. 
# that lets us find [e]

str_remove("1972[e]", "\\[.*\\]") 
str_remove("1975", "\\[.*\\]")

# and then to be thorough, let's only keep seasons that have 4 numbers in the year. 
# how would you finish the code below: 
# you want season to start with a digit, contain only 4 digits, and then finish after 
# those four digits. 
seasons_raw |> 
  filter(str_detect(Season, "^\\d\\d\\d\\d$"))%>%
  print(n=146)#or ^\\d{4}$

# so on your own: 
# finish what i started here. use the regex above and then create 
# columns that are integers that make season, wins, and losses all numeric columns. 
# then select season, league, division, finish, wins, losses, and playoffs. 

# how many rows? 
# how many seasons is 1882 through 2026?
seasons <- seasons_raw |>
  mutate(season = str_remove(Season, "\\[.*\\]"))%>%
  filter(str_detect(Season, "^\\d\\d\\d\\d$"))

seasons<-seasons%>%
  mutate(Wins=as.numeric(Wins),
         Losses=as.numeric(Losses),
         Season=as.numeric(Season))%>%
  select(Season,League,Division,Finish,Wins,Losses,Playoffs)
