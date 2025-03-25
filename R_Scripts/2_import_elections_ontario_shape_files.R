#import shapefiles
library(sf)
library(here)
ontario<-read_sf(here("data/Electoral District Shapefile - 2022 General Election/"))
library(ggplot2)
ontario %>% ggplot()+geom_sf()
#Load dissemination areas
dsa<-read_sf(here("data/statscan_dsa/data"))
head(dsa)
names(dsa)
dsa %>% 
  filter(PRUID==35)
table(dsa$PRUID)
dsa<-subset(dsa, PRUID==35)
