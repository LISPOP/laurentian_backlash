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
view(on22)
on %>%
#Still form our gorups
group_by(ElectoralDistrictNumber) %>% 
#Here instead of summarize, we mutate the dataframe enroll
mutate(n=sum(TotalValidBallotsCast))->on
#Check
view(on)
on %>%
mutate(mv=Plurality/n)->on
