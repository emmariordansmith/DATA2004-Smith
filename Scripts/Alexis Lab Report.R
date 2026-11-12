#Alexis lab report

rm(list=ls())

library(tidyverse)

acetone<-read_csv("Data/Raw/data - Acetone-E.csv")

names(acetone)[2]<-"co"

co2<-read_csv("Data/Raw/data - CO2 0.5 E.csv")

actually<-read_csv("Data/Raw/data - Actually CO2.csv")

ggplot()+
  geom_line(data=acetone,aes(x=`Wavenumber (cm-1) [Student 1014.sp]`,
                             y=co),color="darkred")+
  geom_line(data=co2,aes(x=wavelength,y=absorbance),color="navy")+
  geom_line(data=actually,(aes(x=wavelength_a,y=absorbance_a)),color="forestgreen")+
  ggtitle("Wavelength & Absorbance")+
  xlab("Wavelength")+
  ylab("Absorbance")+
  theme_minimal()

  