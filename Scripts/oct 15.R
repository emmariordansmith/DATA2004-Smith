# packages 
rm(list=ls())
library(tidyverse)
#install.packages("httr2")
library(httr2)

# quick review of the reading :) 

# the R4DS chapter focused on hierarchical data, let's do an example  
# that is formatted like a JSON file. these are fake, but it's set up 
# to look like the earthquake data we will work with today. 
mini_json <- r"({
  "type": "FeatureCollection", #columns of hierarchical data!
  "metadata": {"count": 2, "title": "a made-up example"},
  "features": [
    { "type": "Feature",
      "properties": {"mag": 2.1, "place": "5 km N of Lexington, Kentucky",
                     "felt": 12, "type": "earthquake"},
      "geometry": {"type": "Point", "coordinates": [-84.50, 38.09, 8.0]},
      "id": "fake001" },
    { "type": "Feature",
      "properties": {"mag": 1.4, "place": "10 km E of New Madrid, Missouri",
                     "felt": null, "type": "earthquake"},
      "geometry": {"type": "Point", "coordinates": [-89.43, 36.59, 11.2]},
      "id": "fake002" }
  ]
})"

# before we move on, what do {} and [] identify in json files? 

# {} are COLUMNS 
# [] are ROWS (usually, let's see what happens to coordinates)

# these are earthquakes, and the data is stored hierarchically. 
# so we should read this as 

#hierarchical data: lists of items, 
#columns are type, metadata, and features
#metadata has a subcolumn list, count and title
#features have subcolumns, type, properties, geometry, id
#

# earthquake 
## type "Feature" 
## properties { mag, place, felt, type }
## geometry { type, coordinates }
## id "fake001"

mini <- jsonlite::parse_json(mini_json)
str(mini, max.level = 2) #structure of the json file, id is not present
#changing the level lets you see nested data

# which of those three parts will have the earthquakes? 

mini_tbl <- tibble(feature = mini$features)
mini_tbl

# why do we have 2 rows? what is in each row? 
mini_tbl$feature[[1]]
mini_tbl$feature[[2]]

# let's widen this out and see what's going on

mini_tbl |> 
  unnest_wider(feature) #similar to pivot. Make it un-hierarchical

# so now we have 4 columns, but more lists. why does the code below give us an error?

mini_tbl |> 
  unnest_wider(feature) |> 
  unnest_wider(properties) #can't have duplicate column names

# think about what's actually contained in the columns. 
# feature already gave us a columns called type ("Feature")
# properties has its own type inside it ("earthquake")

#name separations
mini_tbl |>
  unnest_wider(feature) |>
  unnest_wider(properties, names_sep = "_") |>
  unnest_wider(geometry, names_sep = "_") |>
  unnest_wider(geometry_coordinates, names_sep = "_") |> 
  print(width = Inf)


# now to the fun part! requests and responses! 

# in the slides we looked at one URL at a time. 
# that URL is a request, we want a count of the earthquakes between x and y date 
# and in a and b coordinates. 
# we ultimately want something that we can repeat, change, and save, especially 
# when we move on to next week and we have multiple requests for multiple responses. 

# let's do what we did in the slides, but using httr2

ky_request <- request("https://earthquake.usgs.gov/fdsnws/event/1/query") |>
  req_url_query(
    format = "geojson",
    starttime = "2025-01-01",
    endtime = "2026-01-01",
    minlatitude = 36.5,
    maxlatitude = 39.2,
    minlongitude = -89.6,
    maxlongitude = -81.9
  )


# we didn't request anything yet. we're setting up the request 
ky_request

ky_request |> 
  req_dry_run()

# does the dry run look right?

ky_response <- ky_request |> 
  req_perform()

resp_status(ky_response)

# what does that 200 mean?
# i put this link in the syllabus: https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Status
# you can use that to check out what the codes you look up mean  

# let's check out a failure 
ky_request_bad <- request("https://earthquake.usgs.gov/fdsnws/event/1/query") |>
  req_url_query(
    format = "geojson",
    starttime = "2025-01-01",
    endtime = "2026-01-01",
    minlatitud = 36.5, #misspelling
    maxlatitude = 39.2,
    minlongitude = -89.6,
    maxlongitude = -81.9
  )

ky_request_bad

ky_response_bad <- ky_request_bad |> 
  req_perform()

# where did our error come from? it just tells us HTTP 400 Bad Request. what makes this a bad request?

last_response() |>
  resp_body_string()

# this failed out loud, we got a 400 code. but what about this? 
ky_request_maybe_bad <- request("https://earthquake.usgs.gov/fdsnws/event/1/query") |>
  req_url_query(
    format = "geojson",
    minlatitude = 36.5, #without start or end date
    maxlatitude = 39.2,
    minlongitude = -89.6,
    maxlongitude = -81.9
  )

ky_response_maybe_bad <- ky_request_maybe_bad |> 
  req_perform()

resp_status(ky_response_maybe_bad)

ky_response_maybe_bad

ky_response_maybe_bad |> 
  resp_body_json() |> 
  pluck("features") |> 
  length() #defaulting to something we are not intending. Last 30 days

# let's go back to our correct response, let's see what we got :) 
quakes <- ky_response |> 
  resp_body_json()

names(quakes)

# how many features are there? does that match metadata$count? 

# and then let's take a look at one earthquake. 
quakes$features[[1]] |> 
  str(max.level = 2)

# now we can make a table. let's just make one row per earthquake. 
# we want these columns: id, magnitude, place, time, longitude, latitude, and depth. 
# go to the documentation. where do i find each of these? 
# specifically, go to the "Output" section and use it to find each of those columns. 

#properties$mag
#properties$time
#

quake_table<-tibble(feature=quakes$features)%>%
  hoist(feature, 
        id="id",
        magnitude=c("properties","mag"), #specify a path for nested variable
        place=c("properties","place"),
        time=c("properties","time")
        )

quake_table$time #strange, R gives it in miliseconds, calculate to change

quake_table<-quake_table%>%
  mutate(time_clean=time/1000)


# in normal language: for every row, reach into the list in this column and pull these 
# specific pieces out into their own columns. 
# so tibble(feature = quakes$features) gives us one column called feature with 60 rows. 
# then hoist(feature, ...) says: 
## go into the feature column and use that to identify the new columns 
## make a column called id from the "id" named object in feature. 
## then we want something called coords. to fill it, go into geometry, then go into 
## coordinates. it's just the path :)


# now your turn :) 
# if we have time in class, pair up with someone and do this. 
# if not, use this as practice. 

# select a new location, a different span of time, and a minimum magnitude. 
# then answer: 
## what did you ask? 
## what did the server fill in that you didn't ask for? 
# use the count endpoint to find out how many come back and how many exist. 

new_ky_request <- request("https://earthquake.usgs.gov/fdsnws/event/1/query") |>
  req_url_query(
    format = "geojson",
    starttime = "2026-01-01",
    endtime = "2026-09-30",
    minlatitude = 36.5,
    maxlatitude = 39.2,
    minlongitude = -89.6,
    maxlongitude = -81.9,
    minmagnitude=0.1
  )

new_ky_request |> 
  req_dry_run()

# does the dry run look right?

new_ky_response <- new_ky_request |> 
  req_perform()

resp_status(new_ky_response)

# next, let's evaluate the location 
# look at the place column. how many of those are in Kentucky? 
# how would you check this out? could you use regex, maybe? 

