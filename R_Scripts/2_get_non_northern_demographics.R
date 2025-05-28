source(here::here("R_Scripts/2_get_northern_demographics.R"))
#Get url of 2021 census by FED
#library(dataverse)

tmp<-tempfile()
unzip(here("data/census2021_fed.csv.zip"), exdir=tmp)
fed21<-read.csv(file=file.path(tmp, "98-401-X2021010_English_CSV_data.csv"))
unlink(tmp)
head(fed21)
names(fed21)

#on_da_tongfen %>% view()
#This gets the federal electoral district metadata
#fed_on_meta<-get_statcan_wds_metadata("2021", level="FED", version="1.3")
#This filters out the metadata for federal electoral districts
# fed_on_meta %>% 
#   filter(str_detect(`Codelist ID`, "CL_GEO_FED"))->fed_on


#We need to find the Ontario federal electoral districts
#This takes the 10th an 11th digits of the DGUID 
# and puts it into a variable called PRUID
fed21 %>% 
  filter(GEO_LEVEL=="Federal electoral district (2013 Representation Order)")->fed21
names(fed21)
fed21$ALT_GEO_CODE
fed21$DGUID
fed21$PRUID<-str_sub(fed21$DGUID, 10, 11)
#35 is the provincial code for ONtario
fed21 %>% 
  filter(PRUID==35)->fed21
#Now find the FED codes
#They are the last 5 didigts of the DGUID
fed21$fed_code<-str_sub(fed21$DGUID, -5)
# on %>% 
#   filter(fedcreate==2013&str_detect(ElectoralDistrictName, "Pembroke")) %>% 
#   select(fedcreate, Date, ElectoralDistrictName, northern, fed_code)
#Now filter out the northern districts
on %>% filter(northern!=1&fedcreate==2013) %>% 
  #Take the unique FED numbers; these came from Brynn
  distinct(fed_code, ElectoralDistrictName)->non_northern

#non_northern %>% view()
#Now take the FEDs from the meta_data call earler
fed21 %>% 
  #Filter the FEDs in the meta_data call to include only those ones that are non_northern
  filter(fed_code %in% non_northern$fed_code)->non_northern_fed21 #These are the DGUIDs that we will submit
#non_northern_dguid %>% view()
names(non_northern_fed21)
non_northern_fed21<-janitor::clean_names(non_northern_fed21)

non_northern_fed21 %>% 
  filter(
    str_detect(characteristic_name, 'Median age')|
      str_detect(characteristic_name, "Population, 2021")|
      str_detect(characteristic_name, "density")|
      str_detect(characteristic_name, "^21 Mining")|
      #str_detect(characteristic_name, "Average total income of household in 2020 ($)")|
      characteristic_id==2013|#Phds
      characteristic_id==397 |# Frencophones
      characteristic_id==1405 | # First Nations
      characteristic_id==2001 |# Certificate 
      characteristic_id==2263 |
      characteristic_id==252 # income
    ) %>% select(dguid, alt_geo_code, geo_name,characteristic_name, c1_count_total) ->fed21
# To get the demographics

# #Get Francophones
# francophones<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=371, version="1.3", gender="Total")
# save(francophones, file=here("data/non_northern_francophones.rds"))
# #Get Mining
# mining<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=2263, version="1.3", gender="Total")
# save(mining, file=here("data/non_northern_mining.rds"))
# #Get Age
#age<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=39, version="1.3", gender="Total")
#save(age, file=here("data/non_northern_age.rds"))
# #Get Earned PhDs
# phds<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=2013, version="1.3", gender="Total")
# save(phds, file=here("data/non_northern_phd.rds"))
# #Get Average Income
# average_hh_income<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=238, version="1.3", gender="Total") 
# save(average_hh_income, file=here("data/non_northern_average_hh_income.rds"))
# #Get Density
# density<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=6, version="1.3", gender="Total")
# save(density, file=here("data/non_northern_density.rds"))
# #Get Certificate
# certificate<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=2001, version="1.3", gender="Total") 
# save(certificate, file=here("data/non_northern_certificate.rds"))
# #Get First Nations Status
# first_nations<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=1391, version="1.3", gender="Total") 
# save(first_nations, file=here("data/non_northern_first_nations.rds"))
#Get Population
#population<-get_statcan_wds_data(DGUIDs = non_northern_dguid$ID, members=1, version="1.3", gender="Total")
#save(population, file=here("data/non_northern_population.rds"))
#The Code above is extremely flaky because Statscan's API is flaky
# So After running it successfully once I saved all the objects in rds
# files
#The code below just loads them 
# load(here("data/non_northern_mining.rds"))
# load(here("data/non_northern_francophones.rds"))
# load(here("data/non_northern_age.rds"))
# load(here("data/non_northern_density.rds"))
# load(here("data/non_northern_phds.rds"))
# load(here('data/non_northern_certificate.rds'))
# load(here('data/non_northern_average_hh_income.rds'))
# load(here('data/non_northern_first_nations.rds'))
# load(here('data/non_northern_population.rds'))
# mining %>% 
#   bind_rows(francophones, age, density, phds, certificate, average_hh_income, first_nations, population)->non_northern_data


fed21 %>% 
pivot_wider(., id_cols=dguid,names_from=characteristic_name, values_from = c1_count_total) %>% 
rename(fed_code=1, population=2, density=3, age=4, income=5, francophones=6, first_nations=7, certificate=8, phds=9, mining=10)->fed21

non_northern_data<-fed21

non_northern_data$fed_code<-str_sub(non_northern_data$fed_code, start=10)
non_northern_data$fed_code
  #filter(str_detect(GEO_NAME, "Renfrew", negate=T)) %>% 
  #select(CHARACTERISTIC_NAME,Value=OBS_VALUE, DGUID=REF_AREA) %>% 
  #pivot_wider(., names_from=c("CHARACTERISTIC_NAME"), values_from=c("Value")) %>% 
  #mutate(fed_code=str_sub(DGUID, -5)) %>%
  #rename(mining=2, francophones=3, age=4, density=5, phds=6, certificate=7, income=8, first_nations=9, population=10)->non_northern_data


# Get 2011 Non-Northern Data
library(dataverse)
fed_nhs11<-get_dataframe_by_name(filename="99-004-XWE2011001-501.csv", 
                 dataset="10.5683/SP3/XDTSNF", .f=function(x) read.csv(x, sep=",", header=T, skip=1),
                 server="borealisdata.ca")

names(fed_nhs11)
#table(fed_nhs11$Topic) %>% view()
fed_nhs11 %>% 
  filter(str_detect(Characteristic, "First Nations \\(North American Indian\\) single identity"))
fed_nhs11 %>% 
  #filter(Prov_Name=="Ontario") %>% 
  select(fed_code=Geo_Code, fed=FED_Name, GNR, Topic, Characteristic, Total) %>% 
  filter(str_detect(Characteristic, "University certificate, diploma or degree above bachelor level")| 
str_detect(Characteristic, "Mining, quarrying, and oil and gas extraction")|
  str_detect(Characteristic, "Postsecondary certificate, diploma or degree")|
    str_detect(Characteristic, "First Nations \\(North American Indian\\) single identity")|
    str_detect(Characteristic, "^  Average household total"))->fed_nhs11

# This gets rid of the duplicate values for each of the education characteristics

fed_nhs11 %>% 
  group_by(fed,Characteristic) %>% 
  arrange(fed,Characteristic) %>%slice(1) ->fed_nhs11
# Get 2011 Non-Northern Census
fed_census11<-get_dataframe_by_name(filename="98-316-XWE2011001-501.csv",
                      dataset="10.5683/SP3/G7RB1F", server="borealisdata.ca",.f=function(x) read.csv(x, sep=",", header=T, skip=1))
fed_census11 %>% 
filter(Prov_Name=="Ontario") %>% 
  select(fed_code=Geo_Code, fed=FED_NAME, Topic, Characteristic, Total)->fed_census11
#Count unique FED codes

fed_census11 %>% 
  filter(str_detect(Topic, "Population and dwelling")&Characteristic=="Population in 2011"|
           str_detect(Characteristic, "Median age")|
           str_detect(Topic, "Detailed mother tongue")&str_detect(Characteristic, "\xa0\xa0\xa0\xa0French\xa0")|
           str_detect(Characteristic, "density"))->fed_census11


fed_census11 
fed_nhs11 %>% 
    bind_rows(., fed_census11) %>% 
    mutate(Variable=case_when(
str_detect(Characteristic,"21 Mining, quarrying, and oil and gas extraction")~"mining",
str_detect(Characteristic,"\xa0\xa0\xa0\xa0French\xa0")~"francophones",
str_detect(Characteristic,"Median age of the population")~"age",
str_detect(Characteristic,"Population density per square kilometre")~"density",
str_detect(Characteristic,"Postsecondary certificate, diploma or degree")~"certificate",
str_detect(Characteristic,"University certificate, diploma or degree above bachelor level")~"phds",
str_detect(Characteristic,"Average household total income")~"income",
str_detect(Characteristic,"Population in 2011")~"population",
str_detect(Characteristic,"First Nation")~"first_nations"
)) ->non_northern_data11

names(non_northern_data11)  
# Filter only the Ontario districts

non_northern_data11 %>% 
  filter(str_detect(as.numeric(fed_code), "^35.+"))->non_northern_data11

#Deal with -
non_northern_data11 %>% 
  mutate(fed=str_replace_all(fed, " - ", "--"))->non_northern_data11
# Pivot out
non_northern_data11 %>% 
  pivot_wider(., id_cols=c("fed_code", "fed"),names_from=c("Variable"), values_from=c("Total"))->non_northern_data11
# Get rid of the northern districts in 11
northern_ridings
non_northern_data11 %>% 
  filter(!fed %in% northern_ridings)->non_northern_data11
#non_northern_data11 %>% 
#  select(fed, fed_code) %>% view()
non_northern_data11 %>% 
  ungroup() %>% 
  mutate(fedcreate=rep(2003, nrow(.)))->non_northern_data11

non_northern_data11 %>% 
  mutate(northern=rep(0, nrow(.)))->non_northern_data11

non_northern_data %>% 
  mutate(fedcreate=rep(2013, nrow(.)))->non_northern_data
#view(non_northern_data)
non_northern_data %>% 
  mutate(northern=rep(0, nrow(.)))->non_northern_data

non_northern_data %>% 
  bind_rows(non_northern_data11)->non_northern_data_complete

#non_northern_data %>% 
 # slice(37) %>% view()
#view(non_northern_data_complete)
