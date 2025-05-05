# Import poll boundaries for sdubury 

library(sf)
library(here)
on18_poll<-st_read(here('data/poll_boundaries/Polling Division Shapefile - 2018 General Election/'))
on22_poll<-st_read(here('data/poll_boundaries/Polling Division Shapefile - 2022 General Election/'))
head(on18_poll)
library(tidyverse)
glimpse(on18_poll)
st_crs(on18_poll)
on18_poll %>% 
  filter(ED_NAME_EN=="Sudbury")->on18_poll
on22_poll %>% 
  filter(ED_NAME_EN=="Sudbury")->on22_poll
on18_poll %>% 
  mutate(Year=rep("2018", nrow(.)))->on18_poll
on18_poll %>% 
  mutate(Year=rep("2022", nrow(.)))->on22_poll
on18_poll %>% 
  bind_rows(on22_poll)->on_poll
ggplot(on_poll)+geom_sf(data=filter(on_poll, Year=="2018"), col="darkgreen", fill=NA)+
  geom_sf(data=filter(on_poll, Year=="2022", fill=NA), col="darkred")+theme_minimal()

#install.packages("tidygeocoder")
library(tidygeocoder)
address<-tibble(street="935 Ramsey Lake Road",
city="Sudbury", province="ON", postalcode="P3E 2C6", country="Canada")
address
laurentian<-geocode(.tbl=address, street="street", city="city", state="province", postalcode="postalcode", country="country")

laurentian <- st_as_sf(laurentian, coords=c('long', 'lat'), crs = "WGS84")

sf_use_s2(FALSE)
#install.packages("lwgeom")
on_poll$distance<-as.vector(st_distance(laurentian, st_transform(on_poll, st_crs(laurentian))))



# Read in poll level results
on22_poll_results<-read.csv(here("data/poll_level_results/Poll Level Results_2022.csv"))
on18_poll_results<-read.csv(here("data/poll_level_results/Poll Level Results_2018.csv"))

on22_poll_results %>% 
  bind_rows(on18_poll_results)->on_poll_results
on_poll_results %>% 
  filter(str_detect(string=EventNameEnglish, "General"))->on_poll_results

# Use the separate command to separate the column 
# ElectoralDistrictNameEnglish into two columns. 
on_poll_results %>% 
  filter(str_detect(ElectoralDistrictNameEnglish,"Sudbury"))->sudbury_poll_results
# Uncomment these lines to see what they do!
#df <- tibble(id = 1:3, x = c("m-123", "f-455", "f-123"))

#df %>% separate_wider_delim(x, delim = "-", names = c("gender", "unit"))

#df %>% separate_wider_position(x, c(gender = 1, 1, unit = 3))

# How to add the party
# sudbury_poll_results %>% 
# mutate(Party=case_when(
#str_detect(NameOfCandidates, "last_name_of_NDP_candidate")~"NDP",
#str_detect(NameOfCandidates, "last_name_of_PC_candidate")~"PC",
# TRUE~'Other'))->sudbury_poll_results

