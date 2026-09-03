rm(list=ls())

#install.packages("tidyverse")

library(tidyverse)

getwd()

#look at the file before loading to check what needs to be done

population<-read_csv("Data/Raw/API_SP.POP.TOTL_DS2_en_csv_v2_285942.csv",skip=4)

head(population)
tail(population)

glimpse(population)

population%>%
  select(...71)%>%
  unique()

sum(is.na(population$...71))
#there is not data in 71

population<-population |> 
  select(-...71)

#tidy data, each column is a variable and every row an observation.
#so there should be a column for year

#install.packages("readxl")

library(readxl)

population2<-read_excel("Data/Raw/API_SP.POP.TOTL_DS2_en_excel_v2_290931.xls",sheet=1,skip=3)

glimpse(population2)


#JSON file

download.file(url="https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?date=2020%3A2024&format=json&per_page=20000",
              destfile="Data/Raw/population_json.json",
              mode="wb")

install.packages("jsonlite")
library(jsonlite)

population3<-read_json("Data/Raw/population_json.json",simplifyVector=TRUE)

glimpse(population3)

population_obs<-population3[[2]]

glimpse(population_obs)



