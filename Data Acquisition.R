#Data Acquisition, 1980, 85, 90

rm(list=ls())
library(tidyverse)
library(httr2)
library(rvest)

#1980s
url_1980<-"https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_1980"

request(url_1980)%>%
  req_user_agent("Emma Smith, Data 2004 Class, Transylvania University")%>%
  req_perform(path="Data/Raw/music_1980.html")

#pulled 11/5/2026 at 11 AM

music_1980<-read_html("Data/Raw/music_1980.html")

table_1980<-music_1980%>%
  html_elements("table.wikitable")%>%
  html_table()
  
table_1980<-table_1980[[1]]

#1985

url_1985<-"https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_1985"

request(url_1985)%>%
  req_user_agent("Emma Smith, Data 2004 Class, Transylvania University")%>%
  req_perform(path="Data/Raw/music_1985.html")

#pulled 11/6/26 at 11:26 PM

music_1985<-read_html("Data/Raw/music_1985.html")

table_1985<-music_1985%>%
  html_elements("table.wikitable")%>%
  html_table()

table_1985<-table_1985[[1]]

#1990

url_1990<-"https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_1990"

request(url_1990)%>%
  req_user_agent("Emma Smith, Data 2004 Class, Transylvania University")%>%
  req_perform(path="Data/Raw/music_1990.html")

#pulled 11/6/26 at 11:27 PM

music_1990<-read_html("Data/Raw/music_1990.html")

table_1990<-music_1990%>%
  html_elements("table.wikitable")%>%
  html_table()

table_1990<-table_1990[[1]]

