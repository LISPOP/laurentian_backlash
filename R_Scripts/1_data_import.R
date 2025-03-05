#packages to install
#Uncomment and execute 
#install.packages("here")
#install.packages("tidyverse")
#Load here and readxl (readxl is part of the tidyverse package)
library(here)
library(tidyverse)
library(readxl)

#push
on18 <- read_excel("data/on2018_results.xlsx")
on22<- read_excel("data/on2022_results.xlsx")


#bind_rows
on <- bind_rows(on18, on22)
#check things out
glimpse(on18)
glimpse(on22)
glimpse(on)


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

#Remember: AT this point we have one row for each candidate
names(on)
on %>% 
  filter(., ElectoralDistrictNumber==124) %>% 
  view()
on
# It would be informative for our blog post if we were able to say that
# the swing ridings were mostly PC-Liberal, PC-NDP or NDP-Liberal, right? 
# For that though, we need to somehow extract the information for each district
# which party finished first an wd which party finished second. 
# so we need to add one or two more columns at least. 
# I am not sure.


# This is exceptionally hard. It took me about two hours to do. 

#First we can get the first place party for each district
#Always start with the data frame
on %>% 
  #Form the groups of interest
  #In this case, Election and Electoral district 
  #We want the first and second place party names in each district
  #in each riding
  group_by(ElectoralDistrictNumber, Election) %>% 
  #Then we use slice_max, which is the opposite of slice_min
slice_max(Votes) %>% 
  #And we pull out just a couple of columns 
  #specifically The Name of the candidate and the party
  #and we dump it into a new data frame called first_place
  select(Party)->first_place
first_place
#Now, in the first_place data frame
first_place %>% 
  #We take out the groups
  ungroup %>% 
  #And add a new column called Winner
  mutate(Winner=case_when(
    #And we just say
    #If there is any value in the variable called Party
    #i.e. if it is not a missing value
    #Then just return the value for Party
  !is.na(Party)~Party
  )) %>% 
  select(-Party) %>% 
  #Then we merge that data frame with on
  #and re-write over on
  right_join(., on, by=c("ElectoralDistrictNumber", "Election"))->on

#Check it out
#Look for the variable Winner
view(on)

#Then we repeat to find the Second place candidate
#Always start with the data frame you are working with
on %>% 
  #Now, we want to work on the groups made by election and by electoral district number
group_by(ElectoralDistrictNumber, Election) %>%
  #This is different
  #First we have to arrange the data frame in descending number of votes
  arrange(desc(Votes), .by_group=T) %>% 
  #Then we slice the second row, get it, the second place candidate
  #This is like a cousin of slice_max and slice_min
slice(2) %>% 
  #select the name of candidates and the party and dump it into second_place
  select(Party) ->second_place
second_place
#And we repeat the code above
second_place %>% 
  ungroup %>% 
  mutate(Second=case_when(
    !is.na(Party)~Party
  )) %>% 
  select(-Party) %>% 
  right_join(., on) ->on

on %>% filter(ElectoralDistrictNumber==1) %>% view()

#Now we can easily combine those two.
on %>% 
  unite(., Race, c(Winner, Second), sep="-", remove=F)->on
#check

on$Race
#Filter out rows where mv is 0

on_filtered <- on %>% filter(mv !=0)


