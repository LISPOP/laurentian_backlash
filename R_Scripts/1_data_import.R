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

library(dplyr)
#Filter out rows where mv is 0
on_filtered <- on %>% filter(mv !=0)
on_filtered <- on %>% filter(Plurality != 0)
#Check
view(on)

#Group by electoral district number and select the 10 smallest mv values per group
on_filtered <- on_filtered %>%
group_by(ElectoralDistrictNumber) %>%
slice_min(mv, n=10)

#View the filtered dataset
view(on_filtered)
