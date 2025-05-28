#packages to install
#Uncomment and execute 

# list.of.packages<-c("Synth","webshot", "dataverse","SCtools","tongfen", "cancensus", "here", "tidyverse", "sf", "rvest", "readxl", "knitr", "kableExtra")
# #INstall if necessary
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
#Load here and readxl (readxl is part of the tidyverse package)
library(here)
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
library(sf)
library(rvest)
library(cancensus)
library(tongfen)
#set_cancensus_api_key('', install=T)
#Run this command to be sure your cancensus api key has been set
#show_cancensus_api_key('')
#To avoid difficulties with large file storage
# set the cache_path to be OUTSIDE THIS PROJECT'S FOLDERS; I.E. SOMEWHERE ELSE ON YOUR HARD DRIVE
#set_cancensus_cache_path(cache_path = "YOUR_OWN_FOLDER")
#CHECK
#show_cancensus_cache_path()
on18 <- read_excel("data/on2018_results.xlsx")
on22<- read_excel("data/on2022_results.xlsx")
names(on18)

#Get Ontario 14
on14<-read.csv(file="https://results.elections.on.ca/api/report-groups/2/report-outputs/488/csv")
on14 %>% 
  filter(IsGeneralElection==1)->on14
# Get on 11
on11<-read.csv(file="https://results.elections.on.ca/api/report-groups/3/report-outputs/499/csv")
on11 %>% 
  filter(IsGeneralElection==1)->on11

# Get ON 99

on99<-read.csv(file="https://results.elections.on.ca/api/report-groups/6/report-outputs/532/csv")
head(on99)
names(on99)
on99 %>% 
  select(1,3,4,ElectoralDistrictName=ElectoralDistrictNameEnglish, 9,11,12,13,14,15)->on99
on99 <- on99 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))
on99 <- on99 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "-", "--"))
names(on99)
on99 %>% 
  filter(IsGeneralElection==1)->on99
# Get ON 03
on03<-read.csv(file="https://results.elections.on.ca/api/report-groups/5/report-outputs/521/csv")
head(on03)
names(on03)
on03 %>% 
  select(1,3,4,ElectoralDistrictName=ElectoralDistrictNameEnglish, 9,11,12,13,14,15)->on03
on03 <- on03 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))
on03 <- on03 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "-", "--"))
on03 %>% 
  filter(IsGeneralElection==1)->on03
# Get ON 07
on07<-read.csv(file="https://results.elections.on.ca/api/report-groups/4/report-outputs/510/csv")

#Replacing hyphenated dashes
names(on07)
on07 %>% 
  select(1,3,4,ElectoralDistrictName=ElectoralDistrictNameEnglish, 9,11,12,13,14,15)->on07
nrow(on07)
on07 %>% 
  filter(IsGeneralElection==1)->on07
nrow(on07)
on07$ElectoralDistrictName
on07 <- on07 %>%
  mutate(ElectoralDistrictName = str_replace_all(str_to_title(ElectoralDistrictName), "—", "--"))
# on07 %>% 
#   group_by(ElectoralDistrictName) %>% 
#   unique() %>% view()
on11 %>% 
  select(1,3,4,ElectoralDistrictName=ElectoralDistrictNameEnglish, 9,11,12,13,14,15)->on11
on11 <- on11 %>%
  mutate(ElectoralDistrictName = str_replace_all(str_to_title(ElectoralDistrictName), "—", "--"))

on14 %>% 
  select(1,3,4,ElectoralDistrictName=ElectoralDistrictNameEnglish, 9,11,12,13,14,15)->on14

on14 <- on14 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))

on18 <- on18 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))

on22 <- on22 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))

#bind_rows
on <- bind_rows(on99, on03,on07,on11,on14, on18, on22)
#check things out
glimpse(on18)
glimpse(on22)
glimpse(on)
on$ElectoralDistrictName

#source("R_Scripts/1a_scrape_PED_population.R")

#Let's save that
on %>%
  #Still form our gorups
  group_by(ElectoralDistrictNumber) %>% 
  #Here instead of summarize, we mutate the dataframe enroll
  mutate(n=sum(TotalValidBallotsCast))->on
#Check
on
#This code creates the variable mv 
#It divides the plurality of victory by the number of ballots cast
#It is effectively a measure of how large the victory was as a percentage of the total ballots cast

on %>%
  mutate(mv=Plurality/n)->on

library(dplyr)
# This code excludes byelections
on %>%
  filter(IsGeneralElection == 1)->on

#This code renames columns to be more legible.
# Rename columns
on <- on %>%
  rename(
    Election = `EventNameEnglish`,
    Date = `PollingDate`,
    #Party = `PoliticalInterestCode`,
    Votes = `TotalValidBallotsCast`,
    Percent = `PercentOfTotalValidBallotsCast`
  )


#This code renames parties' abbreviations to something more comprehensible.
table(on14$PoliticalInterestCode)
table(on07$PoliticalInterestCode)
table(on99$PoliticalInterestCode)
# Recode date
on %>% 
  mutate(Date=case_when(
    str_detect(Election, "1999")~1999,
    str_detect(Election, "2003")~2003,
    str_detect(Election, "2007")~2007,
    str_detect(Election, "2011")~2011,
    str_detect(Election, "2014")~2014,
    str_detect(Election, "2018")~2018,
    str_detect(Election, "2022")~2022
  ))->on
on %>% 
  mutate(Party=case_when(
    Date<2012&PoliticalInterestCode=="PC"~"PC",
    Date<2012&PoliticalInterestCode=="ND"~"NDP",
    Date<2012&PoliticalInterestCode=="GP"~"Green",
    Date<2012&PoliticalInterestCode=="L"~"Liberal",
    Date>2011&PoliticalInterestCode=="GPO"~"Green",
    Date>2011&PoliticalInterestCode=="LIB"~"Liberal",
    Date>2011&PoliticalInterestCode=="PCP"~"PC",
    Date>2011&PoliticalInterestCode=="OLP"~"Liberal",
    Date>2011&PoliticalInterestCode=="NDP"~"NDP",
    TRUE~ 'Other'
  ))->on

#on <- on %>%
#  mutate(Party =recode(Party, 
#                        "GPO" = "Green", 
#                        "PCP" = "PC", 
#                        "LIB" = "Liberal","OLP"="Liberal", "PC"="PC","NDP"="NDP",.default="Other"))

on %>% 
  group_by(Date) %>% 
  count(Party)
on %>% 
  mutate(Election=case_when(
    str_detect(Election, "1999")==TRUE~ "1999 General Election",
    str_detect(Election, "2003")==TRUE~ "2003 General Election",
    str_detect(Election, "2007")==TRUE~ "2007 General Election",
    str_detect(Election, "2011")==TRUE~ "2011 General Election",
    str_detect(Election, "2014")==TRUE~ "2014 General Election",
    str_detect(Election, "2018")==TRUE~ "2018 General Election",
    str_detect(Election, "2022")==TRUE~ "2022 General Election",
    TRUE~Election
  ))->on

#Turn all candidate names to Title Case
on %>% 
  mutate(NameOfCandidates=str_to_title(NameOfCandidates),
         ElectoralDistrictName=str_to_title(ElectoralDistrictName),
         ElectoralDistrictName=str_replace_all(ElectoralDistrictName, "Of", "of"),
         ElectoralDistrictName=str_replace_all(ElectoralDistrictName, "And", "and"),
         ElectoralDistrictName=str_replace_all(ElectoralDistrictName, "The", "the"))->on

#Remember: AT this point we have one row for each candidate
names(on)
on %>% 
  filter(., ElectoralDistrictNumber==124) 



#Filter PCs
names(on)
on %>% 
  distinct(ElectoralDistrictName)

library(dplyr)

# Assign FEDS
#Read in FEDS
load(file=here("data/can_fedtable_final_20221005.RData"))
#Add matching fedcreate variable to the on data set.
on %>% 
  mutate(fedcreate=case_when(
    Date>2014~2013,
    Date<2013 & Date > 2006 ~ 2003,
    TRUE~NA
  ))->on
  names(table)
  #Selet the province name, the date the riding was creaated and the ED name
  names(table)
head(table)
table %>% 
  select(prname, fedcreate, geoname, id)->fed
#Just the 2003 and 2013 representation orders
fed %>% 
  filter(fedcreate>2002&fedcreate<2022) %>% 
  filter(prname=="Ontario")->fed

# Split the fED Names at the -  into English French
fed %>% 
  separate(., col=geoname, sep=" - ", into=c('ElectoralDistrictName', 'ElectoralDistrictNameFrench')) ->fed
fed %>% 
mutate(ElectoralDistrictName=case_when(
  fedcreate==2003~str_replace_all(ElectoralDistrictName, "-", "--"),
  fedcreate==2013~ElectoralDistrictName,
  )) %>% 
  mutate(ElectoralDistrictName=ElectoralDistrictName)->fed

on %>% 
  filter(str_detect(ElectoralDistrictName, "Carleton")) %>% 
  filter(Date>2003) %>% 
  view()
on %>% 
  filter(str_detect(ElectoralDistrictName, "Lambton")) %>% 
  filter(Date>2003) %>% 
  view()

names(fed)
fed%>% 
  filter(str_detect(ElectoralDistrictName, "Carleton")) %>% 
  filter(fedcreate>2002) %>% 
  view()
fed%>% 
  filter(str_detect(ElectoralDistrictName, "Lambton")) %>% 
  filter(fedcreate>2002) %>% 
  view()

fed %>% 
  filter(str_detect(ElectoralDistrictName, "Glengarry"))
#Set Northern Ridings

#Defining northern ridings
#Defining northern ridings
northern_ridings <- c("Algoma--Manitoulin", 
                      "Kiiwetinoong", 
                      "Kenora--Rainy River", 
                      "Mushkegowuk--James Bay", 
                      "Nickel Belt", 
                      "Nipissing", 
                      "Sault Ste. Marie", 
                      "Sudbury", 
                      "Thunder Bay--Atikokan", 
                      "Thunder Bay--Superior North", 
                      "Timiskaming--Cochrane", 
                      "Timmins", "Parry Sound--Muskoka", "Timmins--James Bay", "Algoma--Manitoulin--Kapuskasing", "	
Nipissing--Timiskaming", "Renfrew--Nipissing--Pembroke", "Thunder Bay--Rainy River", "Nipissing--Timiskaming", "Kenora")

#check
on %>% 
  mutate(fedcreate=case_when(
    Date==2007|Date==2011|Date==2014 ~ 2003,
    Date>2017 ~ 2013
  ))->on
#Creating dummy variable in the Ontario districts
on$northern <- ifelse(on$ElectoralDistrictName %in% northern_ridings, 1, 0)
fed %>% 
  filter(str_detect(ElectoralDistrictName, "Ottawa|Orléans"))
on %>% 
  filter(str_detect(ElectoralDistrictName, "Orlé"))
fed %>% 
  filter(str_detect(ElectoralDistrictName, "Orléans"))
on %>% 
  filter(str_detect(ElectoralDistrictName, "Orléans")&fedcreate==2013)
fed%>% 
  filter(str_detect(ElectoralDistrictName, "Orléans")&fedcreate==2013)
fed$ElectoralDistrictName<-recode(fed$ElectoralDistrictName, 
                                  "Carleton--Lanark"="Carleton--Mississippi Mills",
                                 "Middlesex--Kent--Lambton"="Lambton--Kent--Middlesex",
                                 #"Orléans"="Ottawa--Orléans",
                                 "Ottawa--Oréans"="Ottawa--Orléans",
                                 "Clarington--Scugog--Uxbridge"="Durham",
                                 "Grey--Bruce--Owen Sound"="Bruce--Grey--Owen Sound")

on %>% 
  filter(str_detect(ElectoralDistrictName, "Ajax")) %>% 
  filter(Date>2003) %>% 
  view()
fed %>% 
  filter(fedcreate==2003) %>% view()
fed%>% 
  filter(str_detect(ElectoralDistrictName, "Ajax")) %>% 
 # filter(fedcreate==2003) %>% 
  view()
# on %>% filter(str_detect(ElectoralDistrictName, "Grey")) %>% filter(fedcreate==2003)
# fed %>% filter(str_detect(ElectoralDistrictName, "Grey"))%>% filter(fedcreate==2003)
on %>% filter(str_detect(ElectoralDistrictName, "Orléans")) %>% filter(fedcreate==2013)
fed %>% filter(str_detect(ElectoralDistrictName, "Orlé"))%>% filter(fedcreate==2013)

# Check for Barrie
# on %>% 
#   filter(str_detect(ElectoralDistrictName, "Barrie")) %>% view()
# Convert on to Title Case
# 
on %>% left_join(., select(fed, -ElectoralDistrictNameFrench)) %>%
  filter(fedcreate==2013) %>%
  select(fed_code=id, ElectoralDistrictName, Date, northern) %>%
  filter(northern!=1) %>%
  filter(is.na(fed_code)) 

names(on)
on %>% left_join(., select(fed, -ElectoralDistrictNameFrench)) %>% 
  rename(fed_code=id)->on


  #filter(fedcreate==2013) %>%
  #select(fed_code=id, ElectoralDistrictName, Date, northern) %>%
  #filter(northern!=1) ->on

# on %>% left_join(., select(fed, -ElectoralDistrictNameFrench)) %>%
#   filter(fedcreate>2002) %>%
#   #rename id to fed_code to use the 5 digit elecdtions canada code throughout
#   select(fed_code=id, ElectoralDistrictName, Date, northern) %>% 
#   #filter(northern!=1) %>%
#   filter(is.na(fed_code)) 

  
# on %>% 
# left_join(., select(fed, -ElectoralDistrictNameFrench)) %>%  
#   filter(!is.na(id))
# on <- on %>%ddd
#   mutate(FED = case_when(
#    Date>2014& ElectoralDistrictName == "Ajax" ~ 35001,
#    Date>2014& ElectoralDistrictName == "Algoma--Manitoulin--Kapuskasing" ~ 35002,
#    Date>2014& ElectoralDistrictName == "Aurora--Oak Ridges--Richmond Hill" ~ 35003,
#    Date>2014& ElectoralDistrictName == "Barrie--Innisfil" ~ 35004,
#    Date>2014& ElectoralDistrictName == "Barrie--Springwater--Oro-Medonte" ~ 35005,
#    Date>2014& ElectoralDistrictName == "Bay of Quinte" ~ 35006,
#    Date>2014& ElectoralDistrictName == "Beaches--East York" ~ 35007,
#    Date>2014& ElectoralDistrictName == "Brampton Centre" ~ 35008,
#    Date>2014& ElectoralDistrictName == "Brampton East" ~ 35009,
#    Date>2014& ElectoralDistrictName == "Brampton North" ~ 35010,
#    Date>2014&ElectoralDistrictName == "Brampton South" ~ 35011,
#    Date>2014& ElectoralDistrictName == "Brampton West" ~ 35012,
#    Date>2014& ElectoralDistrictName == "Brantford--Brant" ~ 35013,
#    Date>2014& ElectoralDistrictName == "Bruce--Grey--Owen Sound" ~ 35014,
#    Date>2014& ElectoralDistrictName == "Burlington" ~ 35015,
#    Date>2014&ElectoralDistrictName == "Cambridge" ~ 35016,
#    Date>2014& ElectoralDistrictName == "Carleton" ~ 35088,
#    Date>2014&ElectoralDistrictName == "Chatham-Kent--Leamington" ~ 35017,
#    Date>2014& ElectoralDistrictName == "Davenport" ~ 35018,
#    Date>2014&ElectoralDistrictName == "Don Valley East" ~ 35019,
#    Date>2014&ElectoralDistrictName == "Don Valley North" ~ 35020,
#    Date>2014& ElectoralDistrictName == "Don Valley West" ~ 35021,
#    Date>2014&ElectoralDistrictName == "Dufferin--Caledon" ~ 35022,
#    Date>2014&ElectoralDistrictName == "Durham" ~ 35023,
#    Date>2014&ElectoralDistrictName == "Eglinton--Lawrence" ~ 35024,
#    Date>2014&ElectoralDistrictName == "Elgin--Middlesex--London" ~ 35025,
#    Date>2014&ElectoralDistrictName == "Essex" ~ 35026,
#    Date>2014&ElectoralDistrictName == "Etobicoke Centre" ~ 35027,
#    Date>2014&ElectoralDistrictName == "Etobicoke--Lakeshore" ~ 35028,
#    Date>2014&ElectoralDistrictName == "Etobicoke North" ~ 35029,
#    Date>2014& ElectoralDistrictName == "Flamborough--Glanbrook" ~ 35030,
#    Date>2014& ElectoralDistrictName == "Glengarry--Prescott--Russell" ~ 35031,
#    Date>2014&ElectoralDistrictName == "Guelph" ~ 35032,
#    Date>2014&ElectoralDistrictName == "Haldimand--Norfolk" ~ 35033,
#    Date>2014&ElectoralDistrictName == "Haliburton--Kawartha Lakes--Brock" ~ 35034,
#    Date>2014&ElectoralDistrictName == "Hamilton Centre" ~ 35035,
#    Date>2014& ElectoralDistrictName == "Hamilton East--Stoney Creek" ~ 35036,
#    Date>2014& ElectoralDistrictName == "Hamilton Mountain" ~ 35037,
#    Date>2014& ElectoralDistrictName == "Hamilton West--Ancaster--Dundas" ~ 35038,
#    Date>2014& ElectoralDistrictName == "Hastings--Lennox and Addington" ~ 35039,
#    Date>2014& ElectoralDistrictName == "Huron--Bruce" ~ 35040,
#    Date>2014& ElectoralDistrictName == "Kanata--Carleton" ~ 35041,
#    Date>2014& ElectoralDistrictName == "Kenora" ~ 35042,
#    Date>2014&ElectoralDistrictName == "King--Vaughan" ~ 35043,
#    Date>2014&ElectoralDistrictName == "Kingston and the Islands" ~ 35044,
#    Date>2014& ElectoralDistrictName == "Kitchener Centre" ~ 35045,
#    Date>2014& ElectoralDistrictName == "Kitchener--Conestoga" ~ 35046,
#    Date>2014& ElectoralDistrictName == "Kitchener South--Hespeler" ~ 35047,
#    Date>2014& ElectoralDistrictName == "Lambton--Kent--Middlesex" ~ 35048,
#    Date>2014& ElectoralDistrictName == "Lanark--Frontenac--Kingston" ~ 35049,
#    Date>2014& ElectoralDistrictName == "Leeds--Grenville--Thousand Islands and Rideau Lakes" ~ 35050,
#    Date>2014& ElectoralDistrictName == "London--Fanshawe" ~ 35051,
#    Date>2014&ElectoralDistrictName == "London North Centre" ~ 35052,
#    Date>2014&ElectoralDistrictName == "London West" ~ 35053,
#    Date>2014&ElectoralDistrictName == "Markham--Stouffville" ~ 35054,
#    Date>2014& ElectoralDistrictName == "Markham--Thornhill" ~ 35055,
#    Date>2014& ElectoralDistrictName == "Markham--Unionville" ~ 35056,
#    Date>2014& ElectoralDistrictName == "Milton" ~ 35057,
#    Date>2014& ElectoralDistrictName == "Mississauga Centre" ~ 35058,
#    Date>2014& ElectoralDistrictName == "Mississauga East--Cooksville" ~ 35059,
#    Date>2014& ElectoralDistrictName == "Mississauga--Erin Mills" ~ 35060,
#    Date>2014& ElectoralDistrictName == "Mississauga--Lakeshore" ~ 35061,
#    Date>2014& ElectoralDistrictName == "Mississauga--Malton" ~ 35062,
#    Date>2014&ElectoralDistrictName == "Mississauga--Streetsville" ~ 35063,
#    Date>2014& ElectoralDistrictName == "Nepean" ~ 35065,
#    Date>2014& ElectoralDistrictName == "Newmarket--Aurora" ~ 35065,
#    Date>2014&ElectoralDistrictName == "Niagara Centre" ~ 35066,
#    Date>2014& ElectoralDistrictName == "Niagara Falls" ~ 35067,
#    Date>2014& ElectoralDistrictName == "Niagara West" ~ 35068,
#    Date>2014& ElectoralDistrictName == "Nickel Belt" ~ 35069,
#    Date>2014& ElectoralDistrictName == "Nipissing--Timiskaming" ~ 35070,
#    Date>2014& ElectoralDistrictName == "Northumberland--Peterborough South" ~ 35071,
#    Date>2014&ElectoralDistrictName == "Oakville" ~ 35072,
#    Date>2014& ElectoralDistrictName == "Oakville North--Burlington" ~ 35073,
#    Date>2014& ElectoralDistrictName == "Orléans" ~ 35076,
#    Date>2014& ElectoralDistrictName == "Oshawa" ~ 35074,
#    Date>2014& ElectoralDistrictName == "Ottawa Centre" ~ 35075,
#    Date>2014& ElectoralDistrictName == "Ottawa South" ~ 35077,
#    Date>2014&ElectoralDistrictName == "Ottawa--Vanier" ~ 35078,
#    Date>2014&ElectoralDistrictName == "Ottawa West--Nepean" ~ 35079,
#    Date>2014&ElectoralDistrictName == "Oxford" ~ 35080,
#    Date>2014&ElectoralDistrictName == "Parkdale--High Park" ~ 35081,
#    Date>2014& ElectoralDistrictName == "Parry Sound--Muskoka" ~ 35082,
#    Date>2014& ElectoralDistrictName == "Perth--Wellington" ~ 35083,
#    Date>2014& ElectoralDistrictName == "Peterborough--Kawartha" ~ 35084,
#    Date>2014& ElectoralDistrictName == "Pickering--Uxbridge" ~ 35085,
#    Date>2014& ElectoralDistrictName == "Renfrew--Nipissing--Pembroke" ~ 35086,
#     ElectoralDistrictName == "Richmond Hill" ~ 35087,
#     ElectoralDistrictName == "St. Catharines" ~ 35089,
#     ElectoralDistrictName == "Toronto--St. Paul's" ~ 35090,
#     ElectoralDistrictName == "Sarnia--Lambton" ~ 35091,
#     ElectoralDistrictName == "Sault Ste. Marie" ~ 35092,
#     ElectoralDistrictName == "Scarborough--Agincourt" ~ 35093,
#     ElectoralDistrictName == "Scarborough Centre" ~ 35094,
#     ElectoralDistrictName == "Scarborough--Guildwood" ~ 35095,
#     ElectoralDistrictName == "Scarborough North" ~ 35096,
#     ElectoralDistrictName == "Scarborough--Rouge Park" ~ 35097,
#     ElectoralDistrictName == "Scarborough Southwest" ~ 35098,
#     ElectoralDistrictName == "Simcoe--Grey" ~ 35099,
#     ElectoralDistrictName == "Simcoe North" ~ 35100,
#     ElectoralDistrictName == "Spadina--Fort York" ~ 35101,
#     ElectoralDistrictName == "Stormont--Dundas--South Glengarry" ~ 35102,
#     ElectoralDistrictName == "Sudbury" ~ 35103,
#     ElectoralDistrictName == "Thornhill" ~ 35104,
#     ElectoralDistrictName == "Thunder Bay--Rainy River" ~ 35105, 
#     ElectoralDistrictName == "Thunder Bay--Superior North" ~ 35106,
#     ElectoralDistrictName == "Timmins--James Bay" ~ 35107,
#     ElectoralDistrictName == "Toronto Centre" ~ 35108,
#     ElectoralDistrictName == "Toronto--Danforth" ~ 35109,
#     ElectoralDistrictName == "Toronto--St. Paul's" ~ 35090,
#     ElectoralDistrictName == "University--Rosedale" ~ 35110,
#     ElectoralDistrictName == "Vaughan--Woodbridge" ~ 35111,
#     ElectoralDistrictName == "Waterloo" ~ 35112,
#     ElectoralDistrictName == "Wellington--Halton Hills" ~ 35113,
#     ElectoralDistrictName == "Whitby" ~ 35114,
#     ElectoralDistrictName == "Willowdale" ~ 35115,
#     ElectoralDistrictName == "Windsor--Tecumseh" ~ 35116,
#     ElectoralDistrictName == "Windsor West" ~ 35117,
#     ElectoralDistrictName == "York Centre" ~ 35118,
#     ElectoralDistrictName == "York--Simcoe" ~ 35119,
#     ElectoralDistrictName == "York South--Weston" ~ 35120,
#     ElectoralDistrictName == "Humber River--Black Creek" ~ 35121,
#     TRUE ~ NA_real_  # Default case for unmatched districts
#   ))->on



#check

#

on %>% 
  filter(northern==1) %>% group_by(Date) %>% count()
#### Get Ontario Provincial Boundary files 
ontario<-read_sf(here("data/electoral_districts/"))
names(ontario)
#Replace long hyphens with double-dashes for uniformity. Fuck this is frustrating. 
ontario %>% 
  mutate(ENGLISH_NA = str_replace_all(ENGLISH_NA, "—", "--"))->ontario

#Creating dummy variable in the Ontario districts
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)
#Filter out northern ridings
ontario %>% 
  filter(northern==1)->northern
# This should read 11 and then 13 for 2018 and 2022
on %>% 
  filter(northern==1) %>% 
  filter(Party=="PC") %>% 
  group_by(Date) %>% summarize(n=length(unique(ElectoralDistrictName)))


# import the 2011 boundaries
library(sf)
library(here)
ontario11<-read_sf(here("data/electoral_districts/2012_ontario_ped/"))
head(ontario11)
nrow(ontario11)
library(tidyverse)
ontario11 %>% 
  group_by(ED_ID) %>% 
  summarize(geometry=st_union(geometry)) ->ontario11

on11 %>% 
  select(ElectoralDistrictNumber, ElectoralDistrictName) %>% 
  distinct()
#This gets the Electoral District ames and adds them to ontario 11
ontario11 %>% 
  left_join(.,on11, c("ED_ID"="ElectoralDistrictNumber")) %>% 
  select(ED_ID, ElectoralDistrictName, geometry) %>% 
  distinct()->ontario11

