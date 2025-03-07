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

names(ontario)
