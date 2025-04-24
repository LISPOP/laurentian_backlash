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
#Now filter out the northern districts
on %>% filter(northern!=1) %>% 
  #Take the unique FED numbers; these came from Brynn
  distinct(FED, ElectoralDistrictName)->non_northern
#Now take the FEDs from the meta_data call earler
fed_on %>% 
  #Filter the FEDs in the meta_data call to include only those ones that are non_northern
  filter(FED %in% non_northern$FED)->non_northern_dguid #These are the DGUIDs that we will submit
# To get the demographics
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic")  %>% 
  filter(str_detect(en, "Average age")) 
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
load(here("data/non_northern_mining.rds"))
load(here("data/non_northern_francophones.rds"))
load(here("data/non_northern_age.rds"))
load(here("data/non_northern_density.rds"))
load(here("data/non_northern_phds.rds"))
load(here('data/non_northern_certificate.rds'))
load(here('data/non_northern_average_hh_income.rds'))
load(here('data/non_northern_first_nations.rds'))
load(here('data/non_northern_population.rds'))
mining %>% 
  bind_rows(francophones, age, density, phds, certificate, average_hh_income, first_nations, population)->non_northern_data
non_northern_data %>% 
  select(CHARACTERISTIC_NAME,Value=OBS_VALUE, DGUID=REF_AREA) %>% 
  pivot_wider(., names_from=c("CHARACTERISTIC_NAME"), values_from=c("Value")) %>% 
  mutate(FED=str_sub(DGUID, -5)) %>%
  rename(mining=2, francophones=3, age=4, density=5, phds=6, certificate=7, income=8, first_nations=9, population=10)->non_northern_data
non_northern_data %>% names()
# non_northern_dguid$ID

# #Look for income
# fed_on_meta %>% 
#   filter(`Codelist en`=="Characteristic") %>% 
#   filter(str_detect(en, "French"))
# fed_on_meta %>% 
#   filter(`Codelist en`=="Characteristic") %>% 
#   filter(str_detect(en, "income")) 
# 
# fed_on_meta %>% 
#   filter(`Codelist en`=="Characteristic") %>% 
#   filter(str_detect(en, "Average")) 
# 
# 
# # Density
# density<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=6, gender="Total", version="1.3")
# #save(density, file=here("data/non_northern_density.rds"))
# #Francophones
# francophones<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=371, gender="Total", version="1.3")
# # Post-seconary
# fed_on_meta %>% 
#   filter(`Codelist en`=="Characteristic") %>% 
#   filter(str_detect(en, "certificate")) %>% view()
# # Income
# average_hh_income<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=238, gender="Total", version="1.3")
# average_hh_income
# #Average_age
# average_age<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=39, gender="Total", version="1.3")
# #Phds
# phds<-get_statcan_wds_data(DGUIDs=non_northern_dguid$ID, members=2013, gender="Total", version="1.3")

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

# phds %>%
#   bind_rows(., french, average_age, average_total_hh_income_2020)->non_northern_data
# non_northern_data
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

# 
# non_northern_data %>% 
#   mutate(Variable=case_when(
#     CHARACTERISTIC==2013~"phds",
#     CHARACTERISTIC==39~"age",
#     CHARACTERISTIC==371~"francophones",
#     CHARACTERISTIC==238~"income",
#     CHARACTERISTIC==238~"income"
#   ))->non_northern_data


