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

#Let's save that
view(on22)
on22 %>%
#Still form our gorups
group_by(ElectoralDistrictNumber) %>% 
#Here instead of summarize, we mutate the dataframe enroll
mutate(n=sum(TotalValidBallotsCast))->on22
#Check
view(on22)
on22 %>%
mutate(mv=Plurality/n)->on22
