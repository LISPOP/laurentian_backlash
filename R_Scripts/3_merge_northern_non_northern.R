#### Background script
# This calls the scripts that
# import the ontario voting results, the ontario riding boundaries
# gets the non-northern demographics from statistics canada and estimates the demographics
# on the northern districts
source("R_Scripts/2_get_non_northern_demographics.R")

#I have waited a long time to do this. 
# This is the object created by the tongfen procedure
names(on)
names(northern_tongfen)
northern_tongfen_complete$density
northern_tongfen_complete %>%
  #pick out just these key variables
  select(ED_ID, ENGLISH_NA, 
         population,
        density, francophones,certificate, income,first_nations, mining, age, phds, fedcreate, northern) %>% 
  #Drop the geometry e.g. the boudnaries of each dist
  st_drop_geometry() %>% 
  #Now join it to `on` matching the EDID variable in on_da_tongfen
  # with the ElectoralDistrictNumber variable in `on`
  right_join(., on, by=c("ED_ID"="ElectoralDistrictNumber", "fedcreate", "northern")) ->on

on %>%
  filter(str_detect(ElectoralDistrictName, "Ajax|Sudbury")) %>%
filter(Date>2003)


#We are now missing the non_northern_data
# This was pulled directly from Statistics Canada in 2_get_non_northern_demographics.R

names(non_northern_data)

non_northern_data_complete %>% 
  #filter(fedcreate==2013) %>% 
  select(fed_code, fed,fedcreate, density)

# on %>% 
#   rows_patch(., select(non_northern_data_complete,c(-fed)), by=c("fed_code", "fedcreate", "northern")) ->out
# out %>%
#   filter(str_detect(ElectoralDistrictName, "Ajax|Sudbury")) %>%
#   filter(Date>2003)%>% view()
on %>%
  rows_patch(., select(non_northern_data_complete,c(-fed)), by=c("fed_code", "fedcreate", "northern")) ->on

# non_northern_data_complete %>% 
#   slice(37)
#con %>% 
 # filter(Date>2006&str_detect(ElectoralDistrictName, "Ajax")) %>% view()
names(on)
#on %>% 
#  select(-IsGeneralElection, -ResignedMPPName)->on

#This line divides each demographic variable by population to get a percent
#names(on)
on %>% 
  #rename(Population=Population_source) %>% 
  mutate(across(c(francophones, certificate,first_nations,mining,phds), ~(.x/population), .names="{.col}_pct")) ->on
#Which ridings are the most francophone
on %>% ungroup() %>% 
  distinct(ElectoralDistrictName, fedcreate,Date,francophones_pct) %>% 
  filter(str_detect(ElectoralDistrictName, "Glengarry")) %>% 
  slice_max(francophones_pct, n=30) 


