#import shapefiles
library(sf)
library(here)
ontario<-read_sf(here("data/electoral_districts/"))
library(ggplot2)
library(tidyverse)
ontario %>% ggplot()+geom_sf()
#Load dissemination areas
das<-read_sf(here("data/statscan_dsa/data"))
head(das)
names(das)
glimpse(das)

das %>% 
  filter(PRUID=="35")->das
#Defining northern ridings
northern_ridings <- c("Algoma—Manitoulin", "Kiiwetinoong", "Kenora—Rainy River", "Mushkegowuk—James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay—Atikokan", "Thunder Bay—Superior North", "Timiskaming—Cochrane", "Timmins")
length(northern_ridings)
#Creating dummy variable
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)

#Filter the ontario shape files to include only the northern districts
ontario<-subset(ontario, northern==1)
names(das)
names(ontario)
das %>% 
  st_join(., ontario, by="geometry")

