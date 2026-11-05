# let's start with our world bank data again. 
# you should already have these, but, if not, just run this script below. 
# then run the cleaning scripts so that we can focus all of our attention on visualization

rm(list=ls())

library(tidyverse)
library(httr2)

base <- "https://api.worldbank.org/v2/country/all/indicator/"

request(str_c(base, "SP.POP.TOTL")) |>
  req_url_query(date = 2023, format = "json", per_page = 300) |>
  req_perform(path = "Data/Raw/wb_pop_2023.json")

request(str_c(base, "SP.DYN.LE00.IN")) |>
  req_url_query(date = 2023, format = "json", per_page = 300) |>
  req_perform(path = "Data/Raw/wb_life_2023.json")

request(str_c(base, "NY.GDP.PCAP.CD")) |>
  req_url_query(date = 2023, format = "json", per_page = 300) |>
  req_perform(path = "Data/Raw/wb_gdp_2023.json")

request("https://api.worldbank.org/v2/country") |>
  req_url_query(format = "json", per_page = 400) |>
  req_perform(path = "Data/Raw/wb_countries.json")

get_records <- \(path) jsonlite::read_json(path)[[2]]

wdi <- tibble(rec = c(
  get_records("Data/Raw/wb_pop_2023.json"),
  get_records("Data/Raw/wb_life_2023.json"),
  get_records("Data/Raw/wb_gdp_2023.json")
)) |>
  hoist(rec,
        iso3 = "countryiso3code",
        country = c("country", "value"),
        indicator = c("indicator", "id"),
        value = "value"
  ) |>
  select(-rec) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  rename(
    population = SP.POP.TOTL,
    life_exp = SP.DYN.LE00.IN,
    gdp_pc = NY.GDP.PCAP.CD
  )

places <- tibble(rec = jsonlite::read_json("Data/Raw/wb_countries.json")[[2]]) |>
  hoist(rec,
        iso3 = "id",
        region = c("region", "value"),
        income = c("incomeLevel", "value")
  ) |>
  select(iso3, region, income) |>
  mutate(
    region = str_squish(region),
    income = str_squish(income)
  )

countries <- wdi |>
  semi_join(places |> filter(region != "Aggregates"), join_by(iso3)) |>
  left_join(places, join_by(iso3))

countries <- countries |>
  mutate(income = fct_relevel(income,
                              "Low income", "Lower middle income", "Upper middle income", "High income"))

# now let's get to the fun stuff :)
# y'all have all made plots and visualizations before in DATA1004. 
# if you want a larger review, please review Dr. LeVan's book 
# plus the two chapters i noted in the syllabus. 

# we'll do a slight bit of review, but i care a lot more about if you can 
# make, articulate, and defend the choices that we make when we make visualizations. 

# let's start there: what decisions do we make when we build visualizations?

#type of graph, color, axes, inclusion, exclusion, scale, 

# now let's try to make a simple plot, bare bones to start.
# if i had the substantive question: 
# do people in richer countries tend to live longer? 

# how would i visualize this relationship in ggplot? 

ggplot(countries,aes(x=life_exp,y=income))+
  geom_boxplot()+
  ggtitle("Life Expectancy by Country Income")+
  xlab("Income Level")+
  ylab("Life Expectancy")

ggplot(countries,aes(x=gdp_pc,y=life_exp))+
  geom_point(na.rm=TRUE)

install.packages("ggpubr")
library(ggpubr)

ggscatter(countries,x="gdp_pc",y="life_exp")

# this is our very simple plot. 
## is it good? 
## what does it tell us? 
## is it communicated well?

#it communicates it but its not drastic

# we also have a warning. what was it?
#13 rows outside range removed

# how should we deal with the warnings? 
# once we do that, is the plot now perfect? 

#na.rm=TRUE


# we have another problem that we need to deal with, though. let's pay attention 
# to the x-axis. 

# is the difference between $1,000 and $2,000 the same kind of difference as 
# $50,000 and $51,000?

# do we remember how to transform the scale of our x axis? do that below

#scale_x_continuous, scale_x_log10

ggplot(countries,aes(x=gdp_pc,y=life_exp))+
  geom_point(na.rm=TRUE)+
  ggtitle("Life Expectancy by GDP")+
  xlab("GDP")+
  ylab("Life Expectancy")+
  scale_x_log10()

# but now look at how ugly the x-axis looks. what does 1e+03 mean? right now
# tell me what that means. 

#that means 1.0x10^3, or 1000

# just kidding :) we don't need to actually interpret log values. 


# we can add a 'labels = ' argument inside of how we transformed our x-axis 
# to tell R to label these as dollar values! 

ggplot(countries,aes(x=gdp_pc,y=life_exp))+
  geom_point(na.rm=TRUE)+
  ggtitle("Life Expectancy by GDP")+
  xlab("GDP")+
  ylab("Life Expectancy")+
  scale_x_log10(labels=scales::dollar_format())

# but we've now changed what the visualization looks like by quite a bit. 
# is this dishonest? are we lying to the people we are communicating to?


# let's think about color now! 

# what if we wanted to know whether the pattern looks different in different regions 
# of the world? 

ggplot(countries,aes(x=gdp_pc,y=life_exp,color=region))+
  geom_point(na.rm=TRUE)+
  ggtitle("Life Expectancy by GDP")+
  xlab("log 10 of GDP")+
  ylab("Life Expectancy")+
  scale_x_log10(labels=scales::dollar_format())

# if you sent this plot to me, i wouldn't give you a very good grade because 
# i'd ask why you made everything brown. this means that i can't really 
# tell what is being communicated to me. 

# let me show y'all what this plot looks like to people like me and to someone 
# with a related kind of colorblindness. 

# luckily, R gives us some options to deal with this. Okabe-Ito is my favorite 
# color palette, but I am very open to y'all looking through color palettes yourself 
# and finding ones that you like. but since i set today up, we use my favorite colors :) 

# we'll skip black and work with the 7 chromatic colors 
palette.colors(palette = "Okabe-Ito")
okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[2:8])

# now we can specify our colors 
countries |> 
  ggplot(aes(x = gdp_pc, y = life_exp, color = region)) +
  geom_point() +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  scale_color_manual(values = okabe_ito)

# and then, briefly: 
# how do we change the size of the dots? 

# the argument size= in geom_point

# now that we've done that: 
# why is region inside aes(), but our argument for changing the size of the points 
# outside of aes()?

#because it is a part of the visualization specifically. Size or shape wouldn't help 
#much within a box or bar plot

# but should we always work with color the same way? 
# region doesn't have any order that matters at all. alphabetical? 
# the order you see them looking left-right vs right-left on a globe? 
# none of that matters. but we can have categorical variables where the ordering 
# matters a lot. 

# think about income group. 
## low 
## lower middle 
## upper middle 
## high 

# should we communicate this differently with color? 


# let's use a different color palette, virdis_d, that is explicitly made 
# to communicate ordered categories accessibly. 
# let's do colors for income now. 
countries |> 
  ggplot(aes(x = gdp_pc, y = life_exp, color = income)) +
  geom_point() +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  scale_color_viridis_d(direction = -1) #changes direction of hue +/-


# but now we've hit something: 
# is color doing something that our x-axis isn't? 
# does region tell us something beyond our x-axis? 

# if you had to choose to only apply color to region or income for *this* plot 
# with *this* specific question, which would you like? 

ggplot(countries,aes(x=gdp_pc,y=income))+
  geom_boxplot()

countries |> 
  ggplot(aes(x = gdp_pc, y = life_exp, color = region)) +
  geom_point() +
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  scale_color_manual(values = okabe_ito)+
  facet_wrap(~income)

# but can we use facets to still present both of them?