#### Background script
# This calls the scripts that
# import the ontario voting results, the ontario riding boundaries
# gets the non-northern demographics from statistics canada and estimates the demographics
# on the northern districts
source("R_Scripts/2_get_non_northern_demographics.R")


#This deals with the FED variable which is stored as characters
#Brynn, this is what you did, assigning the FED codes to each provincial district
# in Ontario
on$FED<-as.character(on$FED)

#This merges the northern demographicdata data into the `on` dataframe that Brynn and Nicole constructed
# it effectively takes the demographic statistics for northern ridings
# that have been estimated in the script 2_get_northern_demographics
# and merges those statistics to the `on` object

#I have waited a long time to do this. 
# This is the object created by the tongfen procedure
on_da_tongfen %>% 
  #pick out just these key variables
  select(ED_ID, ENGLISH_NA, Population_source, francophones:income) %>% 
  #Drop the geometry e.g. the boudnaries of each district
  st_drop_geometry() %>% 
  #Now join it to `on` matching the EDID variable in on_da_tongfen
  # with the ElectoralDistrictNumber variable in `on`
  right_join(., on, by=c("ED_ID"="ElectoralDistrictNumber"))->on
#Check
names(on)
head(on)

#We are now missing the non_northern_data
# This was pulled directly from Statistics Canada in 2_get_non_northern_demographics.R
view(on)
non_northern_data %>% 
  select(-DGUID)->non_northern_data
on %>% 
  rows_patch(., non_northern_data, by=c("FED")) ->on
on %>% 
  select(-IsGeneralElection, -ResignedMPPName)->on

#This line divides each demographic variable by population to get a percent

on %>% 
  rename(Population=Population_source) %>% 
  mutate(across(francophones:phds, ~(.x/Population), .names="{.col}_pct"))->on
#Which ridings are the most francophone
on %>% 
  distinct(ElectoralDistrictName, francophones_pct) %>% 
  slice_max(francophones_pct, n=10) 

#### The data frame here may need to be filtered to only include PC vote share depending on how you go forward
