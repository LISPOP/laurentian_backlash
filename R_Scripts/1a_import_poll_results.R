# Import poll boundaries for sdubury 

library(sf)
library(here)

#Set 2018 URL
on18_poll_boundary.zip<-"https://www.elections.on.ca/content/dam/NGW/sitecontent/2017/preo/Polling%20Division%20Shapefile%20-%202018%20General%20Election.zip"
on22_poll_boundary.zip<-"https://www.elections.on.ca/content/dam/NGW/sitecontent/2017/preo/shapefiles/Polling%20Division%20Shapefile%20-%202022%20General%20Election.zip"
tmp<-tempfile()
tmp2<-tempdir()
on18_poll_boundary<-download.file(url=on18_poll_boundary.zip, destfile=tmp)
unzip(zipfile=tmp, exdir=tmp2)
on18_poll_boundary<-read_sf(tmp2)
tmp<-tempfile()
tmp2<-tempdir()
on22_poll_boundary<-download.file(url=on22_poll_boundary.zip, destfile=tmp)
unzip(zipfile=tmp, exdir=tmp2)
on22_poll_boundary<-read_sf(tmp2)
# on18_poll<-st_read(here('data/poll_boundaries/Polling Division Shapefile - 2018 General Election/'))
# on22_poll<-st_read(here('data/poll_boundaries/Polling Division Shapefile - 2022 General Election/'))
head(on18_poll_boundary)
on18_poll_boundary %>% 
  filter(ED_NAME_EN=="Sudbury") %>% 
  nrow()
on22_poll %>% 
  filter(ED_NAME_EN=="Sudbury") %>% 
  nrow()
on22_poll$PD_LABEL
on22_poll$PD_NUMBER
library(tidyverse)
glimpse(on18_poll)
st_crs(on18_poll)
on18_poll_boundary %>% 
  mutate(Year=rep(2018, nrow(.))) %>% 
  filter(ED_NAME_EN=="Sudbury")->sudbury18_poll_boundary
on22_poll_boundary %>% 
  mutate(Year=rep(2022, nrow(.))) %>% 
  filter(ED_NAME_EN=="Sudbury")->sudbury22_poll_boundary

sudbury18_poll_boundary %>% 
  bind_rows(sudbury22_poll_boundary)->sudbury_poll_boundary
sudbury_poll_boundary %>% 
  select(Year,PD_NUMBER) %>% 
  group_by(Year) %>% 
  count(PD_NUMBER) %>% view()
ggplot(sudbury_poll_bounadry)+
  geom_sf(data=filter(on_poll, Year=="2018"), col="darkgreen", fill=NA)+
  geom_sf(data=filter(on_poll, Year=="2022"), col="darkred", fill=NA)+
  theme_minimal()

#install.packages("tidygeocoder")
library(tidygeocoder)
address<-tibble(street="935 Ramsey Lake Road",
city="Sudbury", province="ON", postalcode="P3E 2C6", country="Canada")
address
laurentian<-geocode(.tbl=address, street="street", city="city", state="province", postalcode="postalcode", country="country")

laurentian <- st_as_sf(laurentian, coords=c('long', 'lat'), crs = "WGS84")

sf_use_s2(FALSE)
#install.packages("lwgeom")
sudbury_poll_boundary$distance<-as.vector(st_distance(laurentian, st_transform(on_poll, st_crs(laurentian))))



# Read in poll level results
on22_poll_results<-read.csv(here("data/poll_level_results/Poll Level Results_2022.csv"))
on18_poll_results<-read.csv(here("data/poll_level_results/Poll Level Results_2018.csv"))

on22_poll_results %>% 
  bind_rows(on18_poll_results)->on_poll_results
on_poll_results %>% 
  filter(str_detect(string=EventNameEnglish, "General"))->on_poll_results
# Turn candidate name to upper
on_poll_results %>% 
  mutate(NameOfCandidates=str_to_title(NameOfCandidates)) ->on_poll_results
names(on22_poll_results)
on_poll_results %>% 
  mutate(Year=case_when(
    str_detect(EventNameEnglish, "2022 Provincial General Election")~2022,
    str_detect(EventNameEnglish, "2018")~2018
  ))->on_poll_results

# Use the separate command to separate the column 
# ElectoralDistrictNameEnglish into two columns. 
on_poll_results %>% 
  filter(str_detect(ElectoralDistrictNameEnglish,"Sudbury"))->sudbury_poll_results

#Check if poll numbers are not in the boundary file 


#Convert poll results to number for physical polls

sudbury_poll_results %>% 
  mutate(PD_NUMBER=case_when(
    PollCategory=="Standard"~as.double(PollNumber),
    PollCategory=="Advanced"~0
  ))->sudbury_poll_results
sudbury_poll_results %>% 
  filter(!is.na(PD_NUMBER)) %>% 
  select(PollCategory, PollNumber, PD_NUMBER) %>% view()

#

sudbury_poll_results %>% 
  left_join(., sudbury_poll_boundary)->sudbury_polls

#Check the sudbury poll numbers
sudbury_poll_results %>% 
  group_by(Year) %>% 
  summarize(min(PD_NUMBER, na.rm=T), max=max(PD_NUMBER, na.rm=T))

#Total party vote share and percent per party.
sudbury_polls %>% 
group_by(Year, PollNumber) %>% 
  mutate(Total=sum(AcceptedBallotCount)) %>% 
  mutate(Percent=AcceptedBallotCount/Total) %>% 
  mutate(Party=case_when(
    str_detect(NameOfCandidates, "West")~"NDP",
    str_detect(NameOfCandidates, "Thibeault|Farrow")~"Liberal",
    str_detect(NameOfCandidates, "Crowder|Despatie")~"PC",
    str_detect(NameOfCandidates, "Robinson")~"Green",
    TRUE~ "Other"
  ))->sudbury_polls
#Calculate PC Vote Share

# Correlate Distance with PC Vote Sahre
sudbury_polls %>% 
  filter(Party=="PC") %>% 
  ggplot(., aes(x=distance, y=Percent))+geom_point()+facet_wrap(~Year)+geom_smooth(method="lm")
ggsave(filename=here("Poster/poll_distance_laurentian.png", dpi=300))
#Which party did better in advance polls
# Calculate change in vote share
sudbury_polls %>% 
  filter(Party=="PC") %>% 
  distinct(Year, PD_NUMBER, PD_NUMBER, geometry, Percent, distance) %>% ungroup() %>% 
  arrange(PD_NUMBER, Year) %>% 
  mutate(delta=Percent-lag(Percent, n=1)) %>% 
  arrange(PD_NUMBER, Year) %>%  
  ggplot(., aes(x=distance, y=delta))+
  geom_point()+geom_smooth(method="lm", se=F)+
  labs(x="Distance From Laurentian University", y="Change in PC Vote Share")
ggsave(here("Poster/distance_laurentian.png"),dpi=300, width=10, height=8)

