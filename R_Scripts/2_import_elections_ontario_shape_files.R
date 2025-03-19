#import shapefiles
library(sf)
library(here)
library(ggplot2)
library(tidyverse)
#install.packages("vroom")
library(vroom)
library(cancensus)

#read in the electoral districts boundary files
ontario<-read_sf(here("data/electoral_districts/"))
ontario
#uncomment this if you want to see a nice map of ontario's electoral districts
#ontario %>% ggplot()+geom_sf()
#Load dissemination areas
#read in Statscan Dissemination Boundary files
das<-get_statcan_geographies(level="DA", census_year="2021")

#das<-read_sf(here("data/statscan_dsa/data"))
#checks
head(das)
names(das)
glimpse(das)
#Filter northern_da_in non-Ontario dissemination Areas
#StatsCan provincial code for ONtairo is 35
das %>% 
  filter(PRUID=="35")->das
#Defining northern ridings
northern_ridings <- c("Algoma—Manitoulin", "Kiiwetinoong", "Kenora—Rainy River", "Mushkegowuk—James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay—Atikokan", "Thunder Bay—Superior North", "Timiskaming—Cochrane", "Timmins")
#check
length(northern_ridings)
#Creating dummy variable in the Ontario districts
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)
#Check the projection systems for each
st_crs(ontario)
st_crs(das)

#Transform the ontario projection system to match Statistics Canadas
ontario<-st_transform(ontario, crs=st_crs(das))
#Now to merge the two
#Start with the statscan DAS
das %>% 
  #Join them to the Ontario ridings 
  #We want to 
  #We want to keep *all* the provincial dissemination areas, even the ones that do not fit neatly 
  # within the boundaries 
  # We do this just to allow for checks because I'm not entirely sure what I'm doing
  #The result contains *all* ONtario dissemination areas
  # with a "northern" variable if it is WITHIN the boundaries
  # of a northern provincial electoral district
  st_join(.,ontario,left=T, join=st_within)->ontario_da_in

#Now we filter those rows that have a northern==1

#Now, find the dissemination areas that OVERLAP
das %>% 
  st_join(., ontario, left=T, join=st_overlaps)->ontario_da_overlap

table(ontario_da_in$northern, useNA = "always")# There are 
# 1400 Dissemination areas that are *within* Northern PED. 
# There are  15644 dissemination areas that are *within* non-northern PED; they have a value of 0 on northern
# There are 3421 that *overlap* but are not *within* southern or northern PEDs
table(ontario_da_overlap$northern, useNA="always")
#There are 321 DAs that *overlap* northern PEDs
# There are 6884 that *overlap* southern PEDs
# there are 17047 that do not overlap PEDs
#Filter the northern ridings from both 
ontario_da_in %>% 
  #and we are left with northern ontario dissemination areas WITHIN
  #PED BOUNDARIES
  filter(northern==1)->northern_da_in
#And do the same for the overlapping DAs
ontario_da_overlap %>% 
  filter(northern==1)->northern_da_overlap


#Count how many DAS in Ontario do not fit into an electoral district?
nrow(northern_da_in)
nrow(northern_da_overlap)

ggplot()+
  geom_sf(data=northern_da_in, col=NA, fill="darkgrey")+
  geom_sf(data=northern_da_overlap, col="black",fill="lightgrey", size=1)+
  geom_sf(data=subset(ontario, northern==1), col="darkred", fill=NA)+
  geom_sf_text(data=subset(ontario, northern==1), aes(label=str_replace_all(ENGLISH_NA, "—", "\n")), size=2, col="darkred")+
  theme_minimal()

ggsave(file=here("Plots", "ontario_dissemination_areas.png", dpi=300), height=6, width=6)

#### Get Dissemination Areas
#REad in CAnada das
##Get the 
#canada_das<-vroom::vroom(file=here("data/statscan_geography_profiles/das/98-401-X2021006_English_CSV_data_Ontario.csv"))
#names(canada_das)
#names(northern_da_in)
#Get 

metadata<-get_statcan_wds_metadata(census_year="2021", level="DA")
names(metadata)
view(metadata)
#Search thtrough to find the characteristics for French Mother tongue
# This may require some fiddling
metadata %>% 
  filter(str_detect(en, "French")) # # 371
#Do the same for earned doctorates
metadata %>% 
  filter(str_detect(en, "doctorate"))
#Filter meta data for those two characteristics and store in vars
metadata %>% 
  filter(ID==2013|ID==371)->vars

vars %>% 
  mutate(ID=as.numeric(ID))->vars
dguids<-northern_da_in %>% 
  pull(DGUID)
northern_da_data<-get_statcan_wds_data(DGUIDs=dguids, members=vars$ID, gender="Total")
get_statcan_wds_data(DGUIDs="2013A000459021",level="FED")
#Take the full list of Canadian dissemination areas
canada_das %>% 
  #and keep only those that appear in the list of northern ontario dissemination areas
  #gathered from the spatial merge above
  filter(.$ALT_GEO_CODE %in% northern_da_in$DAUID) %>% 
  #list_rbind() %>% 
  #filter the rows that include population mother tongue in French, and earned doctorates
  filter(., CHARACTERISTIC_ID==1|CHARACTERISTIC_ID==397|CHARACTERISTIC_ID==2013) %>% 
  #rename ALT_GEO_CODE to be DAUID to match thevariable name in northern_da_in that comes from the
  #list of northern ontario dissemination areas figured northern_da_in rom the spatial join above 
  select(DAUID=ALT_GEO_CODE, CHARACTERISTIC_ID, CHARACTERISTIC_NAME, C1_COUNT_TOTAL)->northern_das
# view(northern_das)
# names(northern_das)
# view(northern_da_in)
#Take the Northern Ontario dissemination areas from the spatial join
northern_da_in %>% 
  #Join them to statistical information contained in northern_das
  left_join(., northern_das, by="DAUID")->northern_da_in
#take northern_da_in
names(northern_da_in)
view(northern_da_in)

northern_da_in %>% 
  #gtroup by electoral district and variable 
  group_by(ED_ID, ENGLISH_NA, CHARACTERISTIC_NAME,CHARACTERISTIC_ID) %>%
  #Sum the counts 
  #this is our estimate of population totals for ONtario provincial districts
  #We use mutate because I don't want to throw northern_da_in a 
  summarize(total=sum(C1_COUNT_TOTAL, na.rm=T))->northern_da_in

?group_by
  
head(northern_da_in)
northern_da_in %>% 
  select(-CHARACTERISTIC_ID) %>% 
  pivot_wider(names_from=c("CHARACTERISTIC_NAME"), values_from=c("C1_COUNT_TOTAL")) %>% 
  mutate(french_pct=French/`Population, 2021`, 
         phd_pct=`Earned doctorate`/`Population, 2021`)->northern_da_in
# 
  #Read in the statistical profile for the FEDs.

fed<-read.csv(file=here("data/statscan_geography_profiles/fed/98-401-X2021010_English_CSV_data.csv"))
#Get starting row from the CSV file starting_row. Ontario is from row 305198 to 626179
fed %>%
  slice(305198:626179)->fed
head(fed)
tail(fed)
fed %>%
  filter(str_detect(GEO_LEVEL,"Federal electoral district"))->fed

names(fed)
fed$ALT_GEO_CODE
fed %>%
  select(-contains("MEN")&-contains("SYMBOL")) ->fed
# # select the measures we want
# # proportion of population with earned doctorates
# 
# # proportion of francophones with mother tongue
# fed %>% 
#   #characteristic 
# filter(CHARACTERISTIC_ID==397) ->french_mother_tongue
#   ggplot(french_mother_tongue, aes(x=C10_RATE_TOTAL))+geom_histogram()
# fed %>% 
#   filter(CHARACTERISTIC_ID==2013)->earned_doctorate
# # Read in the Ontario DAs
# # on_das<-read_csv(file=here("data/statscan_geography_profiles/das/98-401-X2021006_English_CSV_data_Ontario.csv"))
# # # Get Ontario characteristics
# # on_das %>% 
# #   select(-contains("MEN")&-contains("SYMBOL")) ->on_das
#   