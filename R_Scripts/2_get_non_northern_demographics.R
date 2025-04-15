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
fed_on_meta %>% 
  filter(`Codelist en`=="Characteristic") 
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
load(here("data/non_northern_average_total_hh_income_2020.rds"))
load(here("data/non_northern_phds.rds"))
load(here("data/non_northern_francophones.rds"))
load(here("data/non_northern_average_age.rds"))

phds %>% 
  bind_rows(., french, average_age, average_total_hh_income_2020)->non_northern_data

non_northern_data %>% 
  mutate(Variable=case_when(
    CHARACTERISTIC==2013~"phds",
    CHARACTERISTIC==39~"age",
    CHARACTERISTIC==371~"francophones",
    CHARACTERISTIC==238~"income"
  ))->non_northern_data
non_northern_data %>% 
  select(Variable, Value=OBS_VALUE, DGUID=REF_AREA) %>% 
  pivot_wider(., names_from=c("Variable"), values_from=c("Value")) %>% 
  mutate(FED=str_sub(DGUID, -5))->non_northern_data


