source(here::here("R_Scripts/2_get_northern_demographics.R"))
#on_da_tongfen %>% view()
#This gets the federal electoral district metadata
fed_on_meta<-get_statcan_wds_metadata("2021", level="FED", version="1.3")
#This filters out the metadata for federal electoral districts
fed_on_meta %>% 
  filter(str_detect(`Codelist ID`, "CL_GEO_FED"))->fed_on


#We need to find the Ontario federal electoral districts
#This takes the 10th an 11th digits of the DGUID 
# and puts it into a variable called PRUID
fed_on$PRUID<-str_sub(fed_on$ID, 10, 11)
#35 is the provincial code for ONtario
fed_on %>% 
  filter(PRUID==35)->fed_on
#Now find the FED codes
#They are the last 5 didigts of the DGUID
fed_on$FED<-str_sub(fed_on$ID, -5)
on %>% filter(northern!=1) %>% 
  distinct(FED, ElectoralDistrictName)->non_northern
names(fed_on)
head(fed_on)
fed_on$ID
fed_on %>% 
  filter(FED %in% non_northern$FED)->non_northern_dguid
non_northern_dguid$ID
#Look for income
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "French"))
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "income")) 

fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "Average")) 

#Get Visible Minority
visible<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=1670, gender="Total", version="1.3")
#Unemployed
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "Unempl"))
unemployed<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=2226, gender="Total", version="1.3")
# LFS Participation
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "in the labour force"))
not_labour_force<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=2227, gender="Total", version="1.3")
# Density
density<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=6, gender="Total", version="1.3")
#save(density, file=here("data/non_northern_density.rds"))
# gini
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "Gini"))
gini<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=365, gender="Total", version="1.3")
gini
#Francophones
francophones<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=371, gender="Total", version="1.3")
# Post-seconary
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "certificate")) %>% view()
post_secondary_certificate<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=2017, gender="Total", version="1.3")
# Income
average_household_income<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=238, gender="Total", version="1.3")
average_household_income
#Average_age
average_age<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=39, gender="Total", version="1.3")
#Population
population<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=1, gender="Total", version="1.3")
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") %>% 
  filter(str_detect(en, "30%"))
#Housing Costs
hh_30<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=1453, gender="Total", version="1.3")
# non_northern_dguid$ID %>%
#   map(., function(x){
#    # Sys.sleep(2)
#     get_statcan_wds_data(DGUIDs=x, members=6, gender="Total", version="1.3", refresh=T)
#     }) %>%
#   list_rbind()->density
# 
# save(density, file=here("data/non_northern_density.rds"))
# non_northern_dguid$ID %>%
#   map(., function(x){
#    # Sys.sleep(2)
#     get_statcan_wds_data(DGUIDs=x, members=371, gender="Total", version="1.3")
#     }) %>%
#   list_rbind()->french
# 
# save(french, file=here("data/non_northern_francophones.rds"))

# non_northern_dguid$ID %>% 
#   map(., function(x){
#     # Sys.sleep(2)
#     get_statcan_wds_data(DGUIDs=x, members=2013, gender="Total", version="1.3")
#   }) %>% 
#   list_rbind()->phds
# save(phds, file=here("data/non_northern_phds.rds"))

# non_northern_dguid$ID %>% 
#   map(., function(x){
#     # Sys.sleep(2)
#     get_statcan_wds_data(DGUIDs=x, members=238, gender="Total", version="1.3")
#   }) %>% 
#   list_rbind()->average_total_hh_income_2020
# 
#save(average_total_hh_income_2020, file=here("data/non_northern_average_total_hh_income_2020.rds"))

# non_northern_dguid$ID %>% 
#   map(., function(x){
#     # Sys.sleep(2)
#     get_statcan_wds_data(DGUIDs=x, members=39, gender="Total", version="1.3")
#   }) %>% list_rbind()->average_age
# save(average_age, file=here("data/non_northern_average_age.rds"))
#This loads the non_northern
# load(here("data/non_northern_average_total_hh_income_2020.rds"))
# load(here("data/non_northern_phds.rds"))
# load(here("data/non_northern_francophones.rds"))
# load(here("data/non_northern_average_age.rds"))
visible %>% 
  bind_rows(gini, francophones, unemployed, not_labour_force, density, post_secondary_certificate, average_age, average_household_income, population)->non_northern_data
# non_northern_data %>% 
#   mutate(Variable=case_when(
#     CHARACTERISTIC==1670~"Visible",
#     CHARACTERISTIC==39~"Average Age",
#     CHARACTERISTIC==371~"Francophones",
#     CHARACTERISTIC==238~"Average_HH_Income",
#     CHARACTERISTIC==35~"Gini",
#     CHARACTERISTIC==2227~"Not Labour Force",
#     CHARACTERISTIC==2226~"Unemployed",
#   ))->non_northern_data
# table(non_northern_data$CHARACTERISTIC_NAME)
# phds %>% 
#   bind_rows(., french, average_age, average_total_hh_income_2020)->non_northern_data
# 
# non_northern_data %>% 
#   mutate(Variable=case_when(
#     CHARACTERISTIC==2013~"phds",
#     CHARACTERISTIC==39~"age",
#     CHARACTERISTIC==371~"francophones",
#     CHARACTERISTIC==238~"income",
#     CHARACTERISTIC==238~"income"
#   ))->non_northern_data
non_northern_data %>% 
  select(CHARACTERISTIC_NAME,Value=OBS_VALUE, DGUID=REF_AREA) %>% 
  pivot_wider(., names_from=c("CHARACTERISTIC_NAME"), values_from=c("Value")) %>% 
  mutate(FED=str_sub(DGUID, -5)) %>% 
  rename(`Visible`=2, `Gini`=3, `French`=4, `Unemployed`=5,`Not Labour Force`=6,`Density`=7, `Certificate`=8,`Age`=9, `Average Income`=10,Population=11)->non_northern_data


