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


#bind_rows
on <- bind_rows(on18, on22)
#check things out
glimpse(on18)
glimpse(on22)
glimpse(on)
on$ElectoralDistrictName


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

