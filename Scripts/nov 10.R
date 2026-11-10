# visualization day 2: what is in the data?

rm(list=ls())
library(tidyverse)

# last thursday: every choice in a chart changes what the reader sees.
# today: before you choose colors or titles, you have to
# know what each dot, bar, and line on your chart actually stands for.


# one question all day, and it's the one from your weekly:
# where does mammoth cave sit among the national parks?


# for the weekly, we saw that it was 33/60 in 2016. but what does that answer 
# not tell us? what is hiding under that answer? 

#yearly variation, regional trends, single year applicability

# before we code:
# in ch. 9, wilke takes one set of temperatures and draws it several ways.
# one version shows only the average for each month. another shows a box
# for each month. another shows every single day as its own dot.
# if you had to plan an outdoor wedding from one of those charts, which
# would you want? why?



# you built this table in the weekly, so today it's given. run it.
# (if your file is somewhere else, change the path.)

parks_raw <- read_csv("Data/Raw/national_parks.csv")

np <- parks_raw |>
  filter(year != "Total", unit_type == "National Park") |>
  mutate(year = as.integer(year)) |>
  distinct(unit_code, year, .keep_all = TRUE)

# the colorblind-safe palette from last thursday
okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[2:8])


# 1. what does one point stand for?

# in the weekly you only looked at 2016. the file goes back much further.
# make a line chart of mammoth cave's visitors over time: year on x,
# visitors on y.
# (hint: its unit_code is "MACA". scales::label_comma() fixes the y labels.)

maca<-np%>%
  filter(unit_code=="MACA")

np%>%
  filter(unit_name=="Mammoth Cave National Park")%>%
  ggplot(aes(x=year,y=visitors))+
  geom_point()+
  geom_line()+
  ggtitle("Mammoth Cave Yearly Visitation")

# something happens in the mid-2000s. find the exact years and the exact
# numbers. we'll do this for 2003-2008 and just look at year and visitors. 

maca%>%
  filter(year>=2003,year<=2008)%>%
  select(year,visitors)

#2006 is when it drops sharply.

# what are some things that could explain a drop like that?

# wilke makes a point in ch. 3 that's easy to skip: the SHAPE of a figure
# changes what it emphasizes. a tall, narrow chart makes ups and downs look
# dramatic. a short, wide chart flattens them. the data don't change.

# try it. drag your plot pane so the chart is tall and narrow, then short
# and wide. how does the story about mammoth cave change?

#it looks less drastic or more drastic

# mark the year of the drop so a reader can't miss it.
# (hint: geom_vline(xintercept = ..., linetype = "dashed"))

np%>%
  filter(unit_name=="Mammoth Cave National Park")%>%
  ggplot(aes(x=year,y=visitors))+
  geom_point()+
  geom_line()+
  ggtitle("Mammoth Cave Yearly Visitation")+
  geom_vline(xintercept=2006,linetype="dashed")

# each point on your line has a year and a number. what does that number stand
# for: the people who visited, or the visits the park service recorded?
# are those always the same thing? when would they come apart?

# 2. was it the park, or was it everybody?

# one of your guesses was probably "people stopped coming." here's a way to
# test it: if something big happened in 2006, the other parks should show
# it too. this table puts each park's 2005 and 2006 side by side. 

two_years <- np |>
  filter(year %in% c(2005, 2006)) |>
  select(unit_name, year, visitors) |>
  pivot_wider(names_from = year, values_from = visitors, names_prefix = "y") |>
  mutate(change = y2006 / y2005 - 1) |>
  arrange(change)

two_years %>%
  print(n=60)

# what's mammoth cave's change? what's the next biggest drop? what does a
# typical park look like?

two_years%>%
  ggplot(aes(x=y2005,y=y2006))+
  geom_point()+
  geom_abline(slope=1,intercept=0)

# that's ugly. let's make this a nicer looking visual :)


# if a park had exactly the same visitors both years, where would its dot
# be? add a line that shows "nothing changed."
# (hint: geom_abline(slope = 1, intercept = 0))


# so what does it mean for a dot to be above the line? below it?

#below the line is lost visitors
#above means gained visitors

# look closely at your chart. both axes are visitors, but is one million
# visitors the same length going across as going up? wilke gives us some helpful guidance here :)

# fix it. what angle is the "nothing changed" line now?
# (hint: add coord_equal())

two_years%>%
  ggplot(aes(x=y2005,y=y2006))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  coord_equal()

# next problem: where are most of your sixty dots?

# wilke's ch. 18 is about dots landing on top of each other. his first fix
# is making them transparent, so piles look darker than single dots.
# try it.

two_years%>%
  ggplot(aes(x=y2005,y=y2006))+
  geom_point(alpha=0.25)+
  geom_abline(slope=1,intercept=0)+
  coord_equal()

# did that fix it? is the problem here really too many dots in one place,
# or is it something we already solved last thursday with GDP?
# fix it the other way.

#Scale log 10

two_years%>%
  ggplot(aes(x=y2005,y=y2006))+
  geom_point(alpha=0.25)+
  geom_abline(slope=1,intercept=0)+
  scale_x_log10(labels=scales::comma_format())+
  scale_y_log10(labels=scales::comma_format())+
  coord_equal()

# find mammoth cave. then make it impossible to miss, the way you did in
# the weekly: every other park gray, mammoth cave in color.
# (hint: a second geom_point() with its own data:
#  data = filter(two_years, unit_name == "Mammoth Cave National Park"))

two_years%>%
  ggplot(aes(x=y2005,y=y2006))+
  geom_point(color="darkgrey")+
  geom_point(data=filter(two_years,unit_name=="Mammoth Cave National Park"),
             color="hotpink")+
  geom_abline(slope=1,intercept=0)+
  scale_x_log10(labels=scales::comma_format())+
  scale_y_log10(labels=scales::comma_format())+
  coord_equal()

# think back to your three guesses from part 1. does this give any evidence? 

#national trends don't work well, as Mammoth Cave is still unique

# the line chart in part 1 and this scatter both show the 2006 drop. what
# does this one tell a reader that the line chart couldn't?

# ggplot gave you a warning on this chart. which park is missing, and why?


# 3. below average compared to what?

# so mammoth cave's 2016 number is probably counted differently than its
# 2005 number. now look at who we compare it to.

# mammoth cave is in the southeast region. is it below average for a
# southeast park? compute the number of parks, the mean, and the median for
# each region in 2016.

np%>%
  filter(region=="SE",year==2016)%>%
  summarise(mean_se=mean(visitors),
            median_se=median(visitors))


# region codes: AK alaska, IM intermountain, MW midwest,
# NC national capital, NE northeast, PW pacific west, SE southeast

parks_2016 <- np |>
  filter(year == 2016)


# make the chart most people would make: a bar for each region's mean.

np%>%
  filter(year==2016)%>%
  group_by(region)%>%
  summarise(mean_region=mean(visitors,na.rm=TRUE))%>%
  ggplot(aes(x=fct_reorder(region,mean_region),y=mean_region,
             fill=fct_reorder(region,mean_region)))+
  geom_col()+
  scale_y_continuous(labels=scales::comma_format())+
  labs(x=NULL,y="Region")

# mammoth cave had 586,514 visitors. according to this chart, how does it
# compare to a southeast park? which region has the busiest parks?


# this is wilke's main argument in ch. 9: a mean turns a whole group into
# one number, and one bar can't tell you what the group looks like. his
# answer is to look at the individual points before you trust the summary.

# so look. one dot per park: visitors on x, region on y.

np%>%
  filter(year==2016)%>%
  ggplot(aes(x=region,y=visitors))+
  geom_point()+
  scale_y_continuous(labels=scales::comma_format())+
  labs(x=NULL,y="Visitors")

np%>%
  filter(year==2016)%>%
  ggplot(aes(x=region,y=visitors,color=region))+
  geom_jitter(width=0.15,height=0,alpha=0.5)+
  scale_y_continuous(labels=scales::comma_format())+
  labs(x=NULL,y="Visitors")

# find the southeast row. its bar said about 2 million. how many southeast
# parks are anywhere near 2 million? what is that mean actually describing?

#That mean is actually describing an outlier

# now look at alaska. how many parks do you see? how many does your table
# say there are?

# when dots sit on top of each other, wilke's fix (ch. 9 and ch. 18) is
# jitter: nudge each dot a little, at random, so you can see them all.
# but this changes what the values look like. is this lying? 
# are there ways we could nudge them that wouldn't misrepresent visitors?


# now put the summary back, on top of the dots: each region's median, in a
# different color AND a different shape. and put n in the region labels,
# so nobody has to guess how many parks are behind a row.
# (hint: join your region summary onto parks_2016, then
#  mutate(region_n = str_c(region, " (n = ", n, ")")) and use region_n on y.
#  the median layer: geom_point(aes(x = median), shape = 18, size = 4, ...))


# look at the northeast and national capital rows. your bar chart drew
# their bars exactly as solid as intermountain's. should it have?


# so, is mammoth cave below average for the southeast? answer it twice:
# once with the mean, once with the median.


# you now have two charts of the same 60 numbers: bars of means, and dots
# with medians. what does the bar chart let you believe that isn't true?
# is there anything the bar chart does better?



# for you

# last night you wrote 3-4 sentences answering "where does mammoth cave sit
# among the national parks?" what would you add or change now?


# thursday we start the coach's trackman data. every pitch in it has a pitch
# type. who do you think decided what each pitch was? why might that matter
# for a chart colored by pitch type?