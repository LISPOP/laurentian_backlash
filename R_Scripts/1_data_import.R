#packages to install
#Uncomment and execute 
#install.packages("here")
#install.packages("tidyverse")
#Load here and readxl (readxl is part of the tidyverse package)
library(here)
library(readxl)
library(tidyverse)
#comment
on18 <- read_excel(here("data", "on2018_results.xlsx"))
on22 <- read_excel(here("data", "on2022_results.xlsx"))
