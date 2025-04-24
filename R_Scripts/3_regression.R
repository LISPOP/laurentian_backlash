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
names(on_da_tongfen)

#I have waited a long time to do this. 
# This is the object created by the tongfen procedure
on_da_tongfen %>% 
  #pick out just these key variables
  select(ED_ID, ENGLISH_NA, Population_source, francophones:first_nations) %>% 
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
#view(on)
names(non_northern_data)
names(on)
non_northern_data %>% 
  select(-DGUID)->non_northern_data


on %>% 
  rename(population=Population_source) %>% 
  rows_patch(., non_northern_data, by=c("FED")) ->on
on %>% 
  select(-IsGeneralElection, -ResignedMPPName)->on
#Check 
on %>% 
  filter(northern!=1) %>% 
  filter(ElectoralDistrictName=="Ajax")
# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=ajax&DGUIDlist=2013A000435001&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0

#This line divides each demographic variable by population to get a percent
names(on)
on %>% 
  #rename(Population=Population_source) %>% 
  mutate(across(c(francophones, phds, mining, certificate, first_nations), ~(.x/population), .names="{.col}_pct"))->on
#Which ridings are the most francophone
on %>% 
  distinct(ElectoralDistrictName, francophones_pct) %>% 
  slice_max(francophones_pct, n=10)
on$mining
on$mining
#Which ridings are the most mining
#Which ridings are the most francophone
on %>% 
  distinct(ElectoralDistrictName, mining_pct) %>% 
  slice_max(mining_pct, n=10)
#Which ridings are the most francophone
on %>% 
  distinct(ElectoralDistrictName, first_nations_pct) %>% 
  slice_max(first_nations_pct, n=10)

on %>% 
  distinct(ElectoralDistrictName, age) %>% 
  slice_max(age, n=10)

on %>% 
  distinct(ElectoralDistrictName, certificate_pct) %>% 
  slice_max(certificate_pct, n=10)

