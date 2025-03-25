library(rvest)
library(tidyverse)
base_url<-"https://www.elections.on.ca/en/election-results/"
district_numbers<-seq(1,124,1)
#Pad district numbers with 00s to match URLS
district_numbers<-str_pad(district_numbers, 3, side="left", pad="0")
#paste each electoral district to the base_url in order to create a list of URLs
# 1 for each eleactoral district
district_numbers %>% 
  map(., ~paste(base_url, x=.x,".html", sep=""))->district_numbers_url

#Scrape
district_numbers_url %>% 
  #get results
  map(., ~(read_html(.x) %>% html_elements("table") %>% html_table(.))) %>% 
  #Pick the first table 
  map(., 1)->on25


#Convert all Margin values to character
on25 %>% 
  map(., .f=~mutate(.x, Margin=as.character(Margin))) ->on25

#Get names
district_numbers_url %>% 
  #get results
  map(., ~(read_html(.x) %>% html_elements("h2") %>% html_text)) %>% unlist()->electoral_district_names

#assign manes to list on25 
names(on25)<-electoral_district_names

#Convert to data frame and turn characters to numeric 
on25 %>% 
  list_rbind(., names_to="Electoral_District") %>% 
  mutate(Margin=as.numeric(str_remove_all(Margin, ",")))->on25

#Remove text to leave only electoral district name
on25 %>% 
  mutate(Electoral_District=str_remove_all(Electoral_District, " Unofficial Election Results"))->on25

# Remove commas to convert to numbers
on25 %>% 
  mutate(`Number of votes`=as.numeric(str_remove_all(`Number of votes`, ",")))->on25
on25 %>% 
  mutate(`Percentage of votes`=as.numeric(str_remove_all(`Percentage of votes`, "%")))->on25

#Fitler out some empty rows
on25 %>% 
  filter(nchar(Candidate)>1)->on25
view(on25)
#Recode party names
on25 %>% 
  mutate(Party=case_when(
    str_detect(`Political Party`, "PC")~"PC",
    str_detect(`Political Party`, "Liberal")~"Liberal",
    str_detect(`Political Party`, "NDP")~"NDP",
    str_detect(`Political Party`, "Green")~"Green",
    str_detect(`Political Party`, "IND")~"Independent",
TRUE~"Other"
  ))->on25
view(on25)
#gr
on25 %>% 
  group_by(Electoral_District) %>% 
  mutate(place=order(`Percentage of votes`)) %>% 
  arrange(Electoral_District) ->on25

#Scrape the turnout values for each district
district_numbers_url %>% 
  #get results
  map(., ~(read_html(.x) %>% html_elements("table") %>% html_table(.))) %>% 
  #Pick the second table 
  map(., 2)->on25_turnout
names(on25_turnout)<-electoral_district_names
on25_turnout
on25_turnout %>% 
  list_rbind(., names_to="Electoral_District") %>% 
  filter(X1=="Voter turnout") %>% 
  mutate(Electoral_District=str_remove_all(Electoral_District, " Unofficial Election Results")) %>% 
  mutate(Turnout=str_remove_all(X2, "%")) %>% 
  select(Electoral_District, Turnout) %>% 
  left_join(on25, ., by="Electoral_District")->on25

#now rank parties
on25 %>% 
  group_by(Electoral_District) %>% 
  mutate(place=order(`Number of votes`, decreasing=T)) ->on25

#Get first place parties

on25 %>% 
  #group by electoral district
  group_by(Electoral_District) %>% 
  #pick the row with the largest number of votes in each group
  slice_max(`Number of votes`) %>% 
  #Create new variable called First with the value of political party
  # Remaining after slicing i..e. the political party
  #That responded to slice_max per gtrouip
  mutate(First=`Party`) %>% 
  #join the original on25 with the results of what came before 
  #Store in on25
  left_join(on25, .)->on25

on25 %>% 
  #Arrange the grouped data frame by descending number of votes
  arrange(desc(`Number of votes`), .by_group = T) %>% 
  #fill the missing values of First downwards
  fill(., First, .direction=c("down"))->on25

#Repeat to find second place
on25 %>% 
  group_by(Electoral_District) %>% 
  #Arrange the results in descending number of votes
  arrange(desc(`Number of votes`)) %>% 
  #slice the above picking the second row
  slice(., 2) %>% 
  mutate(Second=`Party`) %>% 
  left_join(on25, .) %>% 
  fill(., Second, .direction=c("downup"))->on25

#Repeat to find third place
on25 %>% 
  group_by(Electoral_District) %>% 
  #Arrange the results in descending number of votes
  arrange(desc(`Number of votes`)) %>% 
  #slice the above picking the third row
  slice(., 3) %>% 
  mutate(Third=`Party`) %>% 
  left_join(on25, .) %>% 
  fill(., Third, .direction=c("downup"))->on25
#Calculate total ballots case

on25 %>% 
  group_by(Electoral_District) %>% 
  mutate(`Total_Ballots`=sum(`Number of votes`)) %>% 
  mutate(mv=`Number of votes`/`Total_Ballots`)->on25

