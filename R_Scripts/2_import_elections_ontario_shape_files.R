#import shapefiles
library(sf)
library(here)
ontario<-read_sf(here("data/Electoral District Shapefile - 2022 General Election/"))
library(ggplot2)
ontario %>% ggplot()+geom_sf()
#Load dissemination areas
das<-read_sf(here("data/statscan_dsa/data"))
head(das)
names(das)
glimpse(das)
library(tidyverse)
das %>% 
  filter(PRUID=="35")->das
#Defining northern ridings
northern_ridings <- c("Algoma—Manitoulin", "Kiiwetinoong", "Kenora—Rainy River", "Mushkegowuk—James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay—Atikokan", "Thunder Bay—Superior North", "Timiskaming—Cochrane", "Timmins")
length(northern_ridings)
#Creating dummy variable
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)

ontario %>% 
  filter(northern==1)->ontario
names(ontario)
ontario$ED_ID
ontario %>% 
  filter(ED_ID==105) %>% 
  ggplot()+geom_sf()+
  geom_sf(data=das)

glimpse(das)
