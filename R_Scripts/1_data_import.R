#packages to install
#Uncomment and execute 
#install.packages("here")
#install.packages("tidyverse")
#Load here and readxl (readxl is part of the tidyverse package)
library(here)
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
#push
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
  filter(., ElectoralDistrictNumber==124) %>% 
  view()
on
# Recode date
on %>% 
  mutate(Date=case_when(
    str_detect(Election, "2018")~2018,
    str_detect(Election, "2022")~2022
  ))->on

#Filter PCs
on

library(dplyr)

on <- on %>%
  mutate(ALT_GEO_CODE = case_when(
    ElectoralDistrictName == "Ajax" ~ 35001,
    ElectoralDistrictName == "Aurora--Oak Ridges--Richmond Hill" ~ 35003,
    ElectoralDistrictName == "Barrie--Innisfil" ~ 35005,
    ElectoralDistrictName == "Barrie--Springwater--Oro-Medonte" ~ 35006,
    ElectoralDistrictName == "Brampton Centre" ~ 35007,
    ElectoralDistrictName == "Brampton East" ~ 35008,
    ElectoralDistrictName == "Brampton North" ~ 35009,
    ElectoralDistrictName == "Brampton South" ~ 35010,
    ElectoralDistrictName == "Brampton West" ~ 35011,
    ElectoralDistrictName == "Brantford--Brant" ~ 35012,
    ElectoralDistrictName == "Burlington" ~ 35013,
    ElectoralDistrictName == "Cambridge" ~ 35014,
    ElectoralDistrictName == "Chatham-Kent--Leamington" ~ 35016,
    ElectoralDistrictName == "Davenport" ~ 35020,
    ElectoralDistrictName == "Don Valley East" ~ 35021,
    ElectoralDistrictName == "Don Valley North" ~ 35022,
    ElectoralDistrictName == "Don Valley West" ~ 35023,
    ElectoralDistrictName == "Durham" ~ 35024,
    ElectoralDistrictName == "Eglinton--Lawrence" ~ 35025,
    ElectoralDistrictName == "Essex" ~ 35026,
    ElectoralDistrictName == "Etobicoke Centre" ~ 35027,
    ElectoralDistrictName == "Etobicoke--Lakeshore" ~ 35028,
    ElectoralDistrictName == "Etobicoke North" ~ 35029,
    ElectoralDistrictName == "Flamborough--Glanbrook" ~ 35030,
    ElectoralDistrictName == "Guelph" ~ 35031,
    ElectoralDistrictName == "Haldimand--Norfolk" ~ 35032,
    ElectoralDistrictName == "Hamilton Centre" ~ 35033,
    ElectoralDistrictName == "Hamilton East--Stoney Creek" ~ 35034,
    ElectoralDistrictName == "Hamilton Mountain" ~ 35035,
    ElectoralDistrictName == "Hamilton West--Ancaster--Dundas" ~ 35036,
    ElectoralDistrictName == "Humber River--Black Creek" ~ 35037,
    ElectoralDistrictName == "Huron--Bruce" ~ 35038,
    ElectoralDistrictName == "Kitchener Centre" ~ 35039,
    ElectoralDistrictName == "Kitchener--Conestoga" ~ 35040,
    ElectoralDistrictName == "Kitchener South--Hespeler" ~ 35041,
    ElectoralDistrictName == "Lambton--Kent--Middlesex" ~ 35042,
    ElectoralDistrictName == "Lanark--Frontenac--Kingston" ~ 35043,
    ElectoralDistrictName == "Leeds--Grenville--Thousand Islands and Rideau Lakes" ~ 35044,
    ElectoralDistrictName == "London--Fanshawe" ~ 35045,
    ElectoralDistrictName == "London North Centre" ~ 35046,
    ElectoralDistrictName == "London West" ~ 35047,
    ElectoralDistrictName == "Markham--Stouffville" ~ 35048,
    ElectoralDistrictName == "Markham--Thornhill" ~ 35049,
    ElectoralDistrictName == "Markham--Unionville" ~ 35050,
    ElectoralDistrictName == "Milton" ~ 35051,
    ElectoralDistrictName == "Mississauga Centre" ~ 35052,
    ElectoralDistrictName == "Mississauga East--Cooksville" ~ 35053,
    ElectoralDistrictName == "Mississauga--Erin Mills" ~ 35054,
    ElectoralDistrictName == "Mississauga--Lakeshore" ~ 35055,
    ElectoralDistrictName == "Mississauga--Malton" ~ 35056,
    ElectoralDistrictName == "Mississauga--Streetsville" ~ 35057,
    ElectoralDistrictName == "Nepean" ~ 35058,
    ElectoralDistrictName == "Newmarket--Aurora" ~ 35059,
    ElectoralDistrictName == "Niagara Centre" ~ 35060,
    ElectoralDistrictName == "Niagara Falls" ~ 35061,
    ElectoralDistrictName == "Niagara West" ~ 35062,
    ElectoralDistrictName == "Oakville" ~ 35063,
    ElectoralDistrictName == "Oakville North--Burlington" ~ 35064,
    ElectoralDistrictName == "Oshawa" ~ 35065,
    ElectoralDistrictName == "Ottawa Centre" ~ 35066,
    ElectoralDistrictName == "Ottawa South" ~ 35067,
    ElectoralDistrictName == "Ottawa--Vanier" ~ 35068,
    ElectoralDistrictName == "Oxford" ~ 35069,
    ElectoralDistrictName == "Pickering--Uxbridge" ~ 35070,
    ElectoralDistrictName == "Richmond Hill" ~ 35071,
    ElectoralDistrictName == "Scarborough Centre" ~ 35072,
    ElectoralDistrictName == "Scarborough--Guildwood" ~ 35073,
    ElectoralDistrictName == "Scarborough--Rouge Park" ~ 35074,
    ElectoralDistrictName == "Scarborough Southwest" ~ 35075,
    ElectoralDistrictName == "Simcoe North" ~ 35076,
    ElectoralDistrictName == "St. Catharines" ~ 35077,
    ElectoralDistrictName == "Thornhill" ~ 35078,
    ElectoralDistrictName == "Toronto Centre" ~ 35079,
    ElectoralDistrictName == "Toronto--Danforth" ~ 35080,
    ElectoralDistrictName == "Vaughan--Woodbridge" ~ 35081,
    ElectoralDistrictName == "Waterloo" ~ 35082,
    ElectoralDistrictName == "Wellington--Halton Hills" ~ 35083,
    ElectoralDistrictName == "Whitby" ~ 35084,
    ElectoralDistrictName == "Windsor--Tecumseh" ~ 35085,
    ElectoralDistrictName == "York Centre" ~ 35086,
    TRUE ~ NA_real_  # Default case for unmatched districts
  ))

# Merge with demographics data
# demographics <- read.csv("path_to_demographics_file.csv")  # Load demographics data
# on <- on %>% left_join(demographics, by = c("ALT_GEO_CODE" = "ALT_GEO_CODE"))
# 
# table(is.na(on$ALT_GEO_CODE))

# View merged data
head(on)
