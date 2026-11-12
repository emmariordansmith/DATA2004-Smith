# packages 

rm(list=ls())

library(tidyverse)

# alright y'all, last day of visualization :(
# but now we get to do the fun parts! by this i, of course, mean that we get to 
# focus on how to communicate data and analyses to someone who (1) does not EVER want 
# to see your code, (2) does NOT care about your actual code, and (3) doesn't know 
# what they want to see, just knows that data can help them. 

# of course, i mean the Transy baseball team!
# we're going to look at Fall scrimmages, Getsee wants to know what his pitchers' stuff 
# looks like. we're going to build one in class together, and this weekend your weekly assignment 
# will be to do the same for another pitcher. 

# this is the glossary for trackman baseball: 
# https://support.trackmanbaseball.com/hc/en-us/articles/5089413493787-Radar-Glossary-Of-V3-Terms
# we will use that as our codebook. 

# let's start with a question: 
## have you ever seen a visualization that communicates way too much information?

# and then let's start with a second and third question :)
## imagine you had to explain your group project to a few people. 
### would you explain it differently to (a) me, (b) your roommate/partner/whoever you live with, 
### and (c) one of your grandparents? 

## next, let's collapse all of those people. now you just have to explain something 
## to some person, who they are does not matter. 
### what do you do differently when you explain something to them versus when you write 
### notes or outlines for yourself?




# now we can work with the data. i cleaned this up a little bit for us today 
# so we could just focus on communication. however, this doesn't mean that it is 
# necessarily perfect. i just took care of the joins and binding. 
trackman <- read_csv("Data/Raw/trackman_fall_2026.csv")
glimpse(trackman)

# what is the grain? 

#one row represents one pitch on a given day by a given pitcher

# we are going to focus on one pitcher for today, Nathan Hale. 
hale <- trackman |> 
  filter(Pitcher == "Hale, Nathan")

# how many pitches? how many days? 

unique(hale$Date)

#208 pitches, over six days

# for students who were in data1004 in the winter, we already did this first part. 
# we want to look at his movement profile, which is to say how the ball moves 
# horizontally and vertically. if you look in the glossary, we are going to 
# focus on three things: 
## HorzBreak: how much the ball moved horizontally in the air 
## InducedVertBreak: how much the ball moved vertically in the air (outside of gravity)
## TaggedPitchType: the pitch the coach told trackman that nathan was throwing 

# let's set our color blind palette 
# but we're going to do something specific. rather than just set a palette, we are going to 
# assign a specific color to a specific pitch type, so that it's consistent across 
# our plots. 
okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[2:8])

pitch_colors <- c(
  "Fastball" = okabe_ito[1],
  "ChangeUp" = okabe_ito[5],
  "Slider" = okabe_ito[3],
  "Cutter" = okabe_ito[7],
  "Undefined" = "gray50"
)

# let's make a scatterplot where horzbreak is on x and inducedvertbreak is on y, 
# coloring the points by their taggedpitchtype, and let's set scale_color_manual to our pitch_colors

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType))+
  geom_point()+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")

# now let's add horizontal and vertical lines to show what 0 would look like 
# for horizontal and vertical movement. 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType))+
  geom_point()+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")

# now let's pretend that you did this, emailed it to the coach, and pat yourself 
# on the back for a day's work finished. 
# what do you think that the coach would ask you? 
## specifically, what questions can we think of right now? 


# let's pay attention to the "color = TaggedPitchType" 

## the plot we just made colors the pitches by its pitch type. 
## so who decided what each pitch was? 

# we have 2 different pitch type columns in this data
## TaggedPitchType: Getsee or someone he told to stand there with the ipad 
## and manually punch in the kind of pitch nathan threw. 

## AutoPitchType: the trackman system used the characteristics of the pitch to predict 

# let's see if they agree. let's count both. 

hale%>%
  count(TaggedPitchType,AutoPitchType)

# so which one should we use? 
# whichever one we pick, we have to make sure we tell Getsee, including if there are 
# any quality problems. 

# some of our tagged pitch types just don't have enough data to give us anything. 
# so let's only keep the pitches that he threw at least 5 times. 

hale <- hale |> 
  filter(TaggedPitchType %in% c("Fastball", "ChangeUp", "Slider"))

# let's run the same plot that we ran above 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType))+
  geom_point()+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")

# see why we assigned specific pitch types to specific colors? if we 
# change pitch_colors to just our okabe_ito palette, we get something that changes a bit.

# we probably don't want getsee to lose track of which colors mean which pitch :)

# but we shouldn't do it by just color, right?
# someone else could be some other kind of color blind, or getsee could end up 
# just printing this out in black and white. let's give each type its own shape too

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2)+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")

# and what does our legend do? 

#the legend absorbs both

# let's think about how humans tend to read plots, starting with how we do it. 
# your eyes likely go: 
## dot, legend, dot, legend, dot, legend, etc 

# let's drop some redundant coding. we first need to create a table 
# qwhere we have some averages. make a table called centers that gives us 
# one row per pitch type. i want the number of pitches throw, the average 
# relspeed, average horzbreak, and average inducedvertbreak. 

centers<-hale%>%
  group_by(TaggedPitchType)%>%
  summarise(n=n(),
            RelSpeed=mean(RelSpeed), 
        HorzBreak=mean(HorzBreak),
         InducedVertBreak=mean(InducedVertBreak))

# now that you've done that, let's put the names on the chart and just turn the legend off. 
# we will use geom_label(data =... , aes(label = label)...) to put the names on the chart 
# and then use theme() to remove the legend. 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,
             color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2)+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  theme(legend.position="none")+
  geom_label(data=centers,aes(label=TaggedPitchType),color="black")


# remember: we need to assume a few things about our audience. 
# we may think about sample sizes, but we can't guarantee that 
# anyone else will think about that. 
# so let's force getsee to see those numbers. 
centers <- centers |> 
  mutate(label = str_c(TaggedPitchType, " (n = ", n, ")"))

# and since both axes are the same scale, inches, what do we need to add? 
# and then let's make our dots a bit more transparent 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,
             color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2,alpha=0.5)+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale Pitches",x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  theme(legend.position="none")+
  geom_label(data=centers,aes(label=label),color="black")+
  coord_equal()

# but at the end of the day, all of this is just colors and data. 
# coach doesn't just want data. coach wants to know something. 

# let's start by looking at fastballs vs changeups. 
# how different are they? side to side? up and down? speed? 

#Horz break similar, but a six inch difference in vert break, fastball higher
#relspeed is greater in Fastballs than changeups by 8mph on average

# now turn that into a single sentence. 

#On average, both Fastballs and Change ups break right differing by only an inch, but
#Fastballs tend to break about 6 inches higher vertically and 
#go about 8mph faster on average than Change ups.

# yippee that sentence is your title :D

# our titles need to tell the reader what we found. we don't want to just say 
# horzbreak vs ivb, that's a description. we want to explain, not describe. 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,
             color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2,alpha=0.5)+
  scale_color_manual(values=pitch_colors)+
  labs(title="Hale's Fastball vs. Changeup",
  subtitle="~6in IVB difference, ~1in HB difference, 8mph difference",
       x="Horizontal Break",y="Induced Vertical Break")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  theme(legend.position="none")+
  geom_label(data=centers,aes(label=label),color="black")+
  coord_equal()

# but this ignores one of our pitches, right? if we just focus on fastballs vs changeups

# so we shouldn't make sliders so visually important should we? 
focus_colors <- c(
  "Fastball" = okabe_ito[1],
  "ChangeUp" = okabe_ito[5],
  "Slider" = "gray60"
)

# so now let's finish the chart: 
## add a title, subtitle, and axis labels that are understandable. 
## then add a caption that says the data came from trackman, which pitch type column we used,
## and the range of dates. 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,
             color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2,alpha=0.5)+
  scale_color_manual(values=focus_colors)+
  labs(title="Hale's Fastball vs. Changeup",
       subtitle="~6in IVB difference, ~1in HB difference, 8mph difference",
       x="Horizontal Break",y="Induced Vertical Break",
       caption="Data collected from Trackman, visualized from TaggedPitchType, 9/10-10/18")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  theme(legend.position="none")+
  geom_label(data=centers,aes(label=label),color="black")+
  coord_equal()

# but we're not finished! 
## we need to draw the gap the title is talking about. we will use annotate(). 
## this will add a thing to any chart, wherever you tell it to. 
## this is what we will fill in from centers. 
## annotate("segment", x = 21.5, xend = 21.5, y = [do it yourself:)], yend = [dont be lazy:)] + 
## annotate("text", x = 22, y = 8.8, label = "[nuh uh]", hjust = 0) + 
## expand_limits(x = 28)

# then let's put theme_minimal() to clean it up, and then LASTLY 
# put the theme() argument that will remove the legend. 

hale%>%
  ggplot(aes(x=HorzBreak,y=InducedVertBreak,
             color=TaggedPitchType,shape=TaggedPitchType))+
  geom_point(size=2,alpha=0.5)+
  scale_color_manual(values=focus_colors)+
  labs(title="Hale's Fastball vs. Changeup",
       subtitle="~6in IVB difference, ~1in HB difference, 8mph difference",
       x="Horizontal Break (inches)",y="Induced Vertical Break (inches)",
       caption="Data collected from Trackman, visualized from TaggedPitchType, 9/10-10/18")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  theme_minimal()+
  theme(legend.position="none")+
  geom_label(data=centers,aes(label=label),color="black",alpha=0.25)+
  coord_equal()+
  annotate("segment",x=21.5,xend=21.5,y=5.75,yend=11.76)+
  annotate("segment",x=20.5,xend=22.5,y=5.75,yend=5.75)+
  annotate("segment",x=20.5,xend=22.5,y=11.76,yend=11.76)+
  annotate("text", x = 22, y = 8.8, label = "6in", hjust = 0)+
  expand_limits(x = 25)
