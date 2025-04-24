#packages to install
#Uncomment and execute 

list.of.packages<-c("Synth", "SCtools","tongfen", "cancensus", "here", "tidyverse", "sf", "rvest", "readxl", "knitr", "kableExtra")
#INstall if necessary
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
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
#set_cancensus_api_key('CensusMapper_287500bb91a374ec69fdcf270fb20ff7')
set_cancensus_cache_path(cache_path = here("data/statscan"))

on18 <- read_excel("data/on2018_results.xlsx")
on22<- read_excel("data/on2022_results.xlsx")

#Replacing hyphenated dashes
on18 <- on18 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))

on22 <- on22 %>%
  mutate(ElectoralDistrictName = str_replace_all(ElectoralDistrictName, "—", "--"))

#bind_rows
on <- bind_rows(on18, on22)
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
    Party = `PoliticalInterestCode`,
    Votes = `TotalValidBallotsCast`,
    Percent = `PercentOfTotalValidBallotsCast`
  )


#This code renames parties' abbreviations to something more comprehensible.

on <- on %>%
  mutate(Party = recode(Party, 
                        "GPO" = "Green", 
                        "PCP" = "PC", 
                        "LIB" = "Liberal"))

on %>% 
  mutate(Election=case_when(
    str_detect(Election, "2022")==TRUE~ "2022 General Election",
    TRUE~Election
  ))->on

#Turn all candidate names to Title Case
on %>% 
  mutate(NameOfCandidates=str_to_title(NameOfCandidates))->on

#Remember: AT this point we have one row for each candidate
names(on)
on %>% 
  filter(., ElectoralDistrictNumber==124) 
on
# Recode date
on %>% 
  mutate(Date=case_when(
    str_detect(Election, "2018")~2018,
    str_detect(Election, "2022")~2022
  ))->on

#Filter PCs
names(on)
on %>% 
  distinct(ElectoralDistrictName)

library(dplyr)


on <- on %>%
  mutate(FED = case_when(
    ElectoralDistrictName == "Ajax" ~ 35001,
    ElectoralDistrictName == "Algoma--Manitoulin--Kapuskasing" ~ 35002,
    ElectoralDistrictName == "Aurora--Oak Ridges--Richmond Hill" ~ 35003,
    ElectoralDistrictName == "Barrie--Innisfil" ~ 35004,
    ElectoralDistrictName == "Barrie--Springwater--Oro-Medonte" ~ 35005,
    ElectoralDistrictName == "Bay of Quinte" ~ 35006,
    ElectoralDistrictName == "Beaches--East York" ~ 35007,
    ElectoralDistrictName == "Brampton Centre" ~ 35008,
    ElectoralDistrictName == "Brampton East" ~ 35009,
    ElectoralDistrictName == "Brampton North" ~ 35010,
    ElectoralDistrictName == "Brampton South" ~ 35011,
    ElectoralDistrictName == "Brampton West" ~ 35012,
    ElectoralDistrictName == "Brantford--Brant" ~ 35013,
    ElectoralDistrictName == "Bruce--Grey--Owen Sound" ~ 35014,
    ElectoralDistrictName == "Burlington" ~ 35015,
    ElectoralDistrictName == "Cambridge" ~ 35016,
    ElectoralDistrictName == "Carleton" ~ 35088,
    ElectoralDistrictName == "Chatham-Kent--Leamington" ~ 35017,
    ElectoralDistrictName == "Davenport" ~ 35018,
    ElectoralDistrictName == "Don Valley East" ~ 35019,
    ElectoralDistrictName == "Don Valley North" ~ 35020,
    ElectoralDistrictName == "Don Valley West" ~ 35021,
    ElectoralDistrictName == "Dufferin--Caledon" ~ 35022,
    ElectoralDistrictName == "Durham" ~ 35023,
    ElectoralDistrictName == "Eglinton--Lawrence" ~ 35024,
    ElectoralDistrictName == "Elgin--Middlesex--London" ~ 35025,
    ElectoralDistrictName == "Essex" ~ 35026,
    ElectoralDistrictName == "Etobicoke Centre" ~ 35027,
    ElectoralDistrictName == "Etobicoke--Lakeshore" ~ 35028,
    ElectoralDistrictName == "Etobicoke North" ~ 35029,
    ElectoralDistrictName == "Flamborough--Glanbrook" ~ 35030,
    ElectoralDistrictName == "Glengarry--Prescott--Russell" ~ 35031,
    ElectoralDistrictName == "Guelph" ~ 35032,
    ElectoralDistrictName == "Haldimand--Norfolk" ~ 35033,
    ElectoralDistrictName == "Haliburton--Kawartha Lakes--Brock" ~ 35034,
    ElectoralDistrictName == "Hamilton Centre" ~ 35035,
    ElectoralDistrictName == "Hamilton East--Stoney Creek" ~ 35036,
    ElectoralDistrictName == "Hamilton Mountain" ~ 35037,
    ElectoralDistrictName == "Hamilton West--Ancaster--Dundas" ~ 35038,
    ElectoralDistrictName == "Hastings--Lennox and Addington" ~ 35039,
    ElectoralDistrictName == "Huron--Bruce" ~ 35040,
    ElectoralDistrictName == "Kanata--Carleton" ~ 35041,
    ElectoralDistrictName == "Kenora" ~ 35042,
    ElectoralDistrictName == "King--Vaughan" ~ 35043,
    ElectoralDistrictName == "Kingston and the Islands" ~ 35044,
    ElectoralDistrictName == "Kitchener Centre" ~ 35045,
    ElectoralDistrictName == "Kitchener--Conestoga" ~ 35046,
    ElectoralDistrictName == "Kitchener South--Hespeler" ~ 35047,
    ElectoralDistrictName == "Lambton--Kent--Middlesex" ~ 35048,
    ElectoralDistrictName == "Lanark--Frontenac--Kingston" ~ 35049,
    ElectoralDistrictName == "Leeds--Grenville--Thousand Islands and Rideau Lakes" ~ 35050,
    ElectoralDistrictName == "London--Fanshawe" ~ 35051,
    ElectoralDistrictName == "London North Centre" ~ 35052,
    ElectoralDistrictName == "London West" ~ 35053,
    ElectoralDistrictName == "Markham--Stouffville" ~ 35054,
    ElectoralDistrictName == "Markham--Thornhill" ~ 35055,
    ElectoralDistrictName == "Markham--Unionville" ~ 35056,
    ElectoralDistrictName == "Milton" ~ 35057,
    ElectoralDistrictName == "Mississauga Centre" ~ 35058,
    ElectoralDistrictName == "Mississauga East--Cooksville" ~ 35059,
    ElectoralDistrictName == "Mississauga--Erin Mills" ~ 35060,
    ElectoralDistrictName == "Mississauga--Lakeshore" ~ 35061,
    ElectoralDistrictName == "Mississauga--Malton" ~ 35062,
    ElectoralDistrictName == "Mississauga--Streetsville" ~ 35063,
    ElectoralDistrictName == "Nepean" ~ 35065,
    ElectoralDistrictName == "Newmarket--Aurora" ~ 35065,
    ElectoralDistrictName == "Niagara Centre" ~ 35066,
    ElectoralDistrictName == "Niagara Falls" ~ 35067,
    ElectoralDistrictName == "Niagara West" ~ 35068,
    ElectoralDistrictName == "Nickel Belt" ~ 35069,
    ElectoralDistrictName == "Nipissing--Timiskaming" ~ 35070,
    ElectoralDistrictName == "Northumberland--Peterborough South" ~ 35071,
    ElectoralDistrictName == "Oakville" ~ 35072,
    ElectoralDistrictName == "Oakville North--Burlington" ~ 35073,
    ElectoralDistrictName == "Orléans" ~ 35076,
    ElectoralDistrictName == "Oshawa" ~ 35074,
    ElectoralDistrictName == "Ottawa Centre" ~ 35075,
    ElectoralDistrictName == "Ottawa South" ~ 35077,
    ElectoralDistrictName == "Ottawa--Vanier" ~ 35078,
    ElectoralDistrictName == "Ottawa West--Nepean" ~ 35079,
    ElectoralDistrictName == "Oxford" ~ 35080,
    ElectoralDistrictName == "Parkdale--High Park" ~ 35081,
    ElectoralDistrictName == "Parry Sound--Muskoka" ~ 35082,
    ElectoralDistrictName == "Perth--Wellington" ~ 35083,
    ElectoralDistrictName == "Peterborough--Kawartha" ~ 35084,
    ElectoralDistrictName == "Pickering--Uxbridge" ~ 35085,
    ElectoralDistrictName == "Renfrew--Nipissing--Pembroke" ~ 35086,
    ElectoralDistrictName == "Richmond Hill" ~ 35087,
    ElectoralDistrictName == "St. Catharines" ~ 35089,
    ElectoralDistrictName == "Toronto--St. Paul's" ~ 35090,
    ElectoralDistrictName == "Sarnia--Lambton" ~ 35091,
    ElectoralDistrictName == "Sault Ste. Marie" ~ 35092,
    ElectoralDistrictName == "Scarborough--Agincourt" ~ 35093,
    ElectoralDistrictName == "Scarborough Centre" ~ 35094,
    ElectoralDistrictName == "Scarborough--Guildwood" ~ 35095,
    ElectoralDistrictName == "Scarborough North" ~ 35096,
    ElectoralDistrictName == "Scarborough--Rouge Park" ~ 35097,
    ElectoralDistrictName == "Scarborough Southwest" ~ 35098,
    ElectoralDistrictName == "Simcoe--Grey" ~ 35099,
    ElectoralDistrictName == "Simcoe North" ~ 35100,
    ElectoralDistrictName == "Spadina--Fort York" ~ 35101,
    ElectoralDistrictName == "Stormont--Dundas--South Glengarry" ~ 35102,
    ElectoralDistrictName == "Sudbury" ~ 35103,
    ElectoralDistrictName == "Thornhill" ~ 35104,
    ElectoralDistrictName == "Thunder Bay--Rainy River" ~ 35105, 
    ElectoralDistrictName == "Thunder Bay--Superior North" ~ 35106,
    ElectoralDistrictName == "Timmins--James Bay" ~ 35107,
    ElectoralDistrictName == "Toronto Centre" ~ 35108,
    ElectoralDistrictName == "Toronto--Danforth" ~ 35109,
    ElectoralDistrictName == "Toronto--St. Paul's" ~ 35090,
    ElectoralDistrictName == "University--Rosedale" ~ 35110,
    ElectoralDistrictName == "Vaughan--Woodbridge" ~ 35111,
    ElectoralDistrictName == "Waterloo" ~ 35112,
    ElectoralDistrictName == "Wellington--Halton Hills" ~ 35113,
    ElectoralDistrictName == "Whitby" ~ 35114,
    ElectoralDistrictName == "Willowdale" ~ 35115,
    ElectoralDistrictName == "Windsor--Tecumseh" ~ 35116,
    ElectoralDistrictName == "Windsor West" ~ 35117,
    ElectoralDistrictName == "York Centre" ~ 35118,
    ElectoralDistrictName == "York--Simcoe" ~ 35119,
    ElectoralDistrictName == "York South--Weston" ~ 35120,
    ElectoralDistrictName == "Humber River--Black Creek" ~ 35121,
    TRUE ~ NA_real_  # Default case for unmatched districts
  ))->on


#Set Northern Ridings
#Defining northern ridings
northern_ridings <- c("Algoma--Manitoulin", "Kiiwetinoong", "Kenora--Rainy River", "Mushkegowuk--James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay--Atikokan", "Thunder Bay--Superior North", "Timiskaming--Cochrane", "Timmins", "Parry Sound--Muskoka")
length(northern_ridings) #should be 13
#Creating dummy variable in the Ontario districts
on$northern <- ifelse(on$ElectoralDistrictName %in% northern_ridings, 1, 0)

#check
on %>% 
  filter(is.na(FED)) 
#
on %>% 
  filter(northern==1) 
#### Get Ontario Provincial Boundary files 
ontario<-read_sf(here("data/electoral_districts/"))
names(ontario)
#Replace long hyphens with double-dashes for uniformity. Fuck this is frustrating. 
ontario %>% 
  mutate(ENGLISH_NA = str_replace_all(ENGLISH_NA, "—", "--"))->ontario
#Defining northern ridings
northern_ridings <- c("Algoma--Manitoulin", "Kiiwetinoong", "Kenora--Rainy River", "Mushkegowuk--James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay--Atikokan", "Thunder Bay--Superior North", "Timiskaming--Cochrane", "Timmins", "Parry Sound--Muskoka")
#check
length(northern_ridings) #should be 13
#Creating dummy variable in the Ontario districts
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)
#Filter out northern ridings
ontario %>% 
  filter(northern==1)->northern
