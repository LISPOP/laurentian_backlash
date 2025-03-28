list.of.packages<-c("tongfen", "cancensus", "here", "tidyverse", "sf", "rvest", "readxl", "knitr", "kableExtra")
#INstall if necessary
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tongfen)
library(cancensus)
library(tidyverse)
library(sf)
library(here)
#### Get Dissemination Blocks
set_cancensus_cache_path(here("data/statscan"))
#list_census_regions(dataset="CA21", use_cache = TRUE, quiet = FALSE) %>% view()
#das<-get_statcan_geographies(regions=list(PR="35"), level="DB", census_year="2021", type="digital")
#Get population for dissemination blocks
db<-get_census(regions=list(PR="35"), level="DB", dataset="CA21")
head(db)
#db_geometry<-get_statcan_geo_suite(level="DB", census_year="2021")
#### Dissemination Block Geometries
# #This is the URL of dissemination block geometries
db_url<-"https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ldb_000b21a_e.zip"
#tempfile
tmp<-tempfile()
#specify directory to extract the zip contents to
outdir<-file.path(here("data/statscan_db/"))
#Increase timeout
getOption("timeout")
options(timeout = max(300, getOption("timeout")))
getOption("timeout")
#download the url to the temp file
download.file(url=db_url, destfile=tmp)
unzip(tmp, exdir=outdir)

#read in dB geometries
db_geometry<-st_read(dsn=here("data/statscan_db/"))
#Filter ontario dissemination blocks
names(db_geometry)
db_geometry %>% 
  filter(PRUID=="35")->db_geometry
#Get DAUID for each DB
db_geometry$DA_UID<-str_sub(db_geometry$DGUID, 9, 17)
head(db_geometry)
#Merge the population counts with the geometry files
names(db)
names(db_geometry)
# db %>% 
#   left_join(., db_geometry_ontario, by=c("GeoUID"="DBUID"))->db_population_geometry
# head(db_population_geometry)

### Get Dissemination Area Populations
da<-get_census("CA21", level="DA", regions=list(PR="35"), geo_format=NA)
#### Get Dissemination Area Geometries

#### Read in Dissemination Area boundaries
#### NOTE: 
library(cancensus)

da_url<-'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000a21a_e.zip'
tmp<-tempfile()
outdir<-file.path(here("data/statscan_da"))
download.file(url=da_url, destfile=tmp)
unzip(tmp, exdir=outdir)

#da_geometry<-get_census("CA21", regions=list(PR="35"),level="DA",geo_format="sf")
da_geometry<-st_read(here('data/statscan_da/'))
da_geometry %>% 
  filter(PRUID=="35")->da_geometry

#da_geometry<-st_read(here("data/statscan_da/"))

#### Get Ontario Provincial Boundary files 
ontario<-read_sf(here("data/electoral_districts/"))
names(ontario)
#Replace long hyphens with double-dashes for uniformity. Fuck this is frustrating. 
ontario %>% 
  mutate(ENGLISH_NA = str_replace_all(ENGLISH_NA, "—", "--"))->ontario

names(ontario)
#Get population Counts
# by calling the scxript that reads in Ontario election results
# part of this script scrapes population counts from wikipedia
#source(here("R_Scripts/1_data_import.R"))
# object on has the 2018 and 2022 election results and pouplation counts
# object ontario has the boundary files
# We want population from on to merge to Ontario
names(ontario)

#Take on that has ED numbers, Population and District names and results
# on %>% 
#   select(c("ElectoralDistrictNumber", "ElectoralDistrictName")) %>% 
#   #Select distinct rows 
#   # Because the data frame has multiple rows per district
#   #one row per candidate
#   distinct() %>% 
#   #join back to ontario that has the boundary files
#   left_join(., ontario, by=c("ElectoralDistrictNumber"="ED_ID"))->ontario
# table(ontario$ENGLISH_NA)
#Defining northern ridings
northern_ridings <- c("Algoma--Manitoulin", "Kiiwetinoong", "Kenora--Rainy River", "Mushkegowuk--James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay--Atikokan", "Thunder Bay--Superior North", "Timiskaming--Cochrane", "Timmins", "Parry Sound--Muskoka")
#check
length(northern_ridings) #should be 13
#Creating dummy variable in the Ontario districts
ontario$northern <- ifelse(ontario$ENGLISH_NA %in% northern_ridings, 1, 0)
#Filter out northern ridings
ontario %>% 
  filter(northern==1)->northern
northern
#Add in population
source("R_Scripts/2_get_northern_populations.R")
northern %>% 
  left_join(., northern_population) -> northern
#Check the projection systems for each
#st_crs(ontario)
#st_crs()
st_crs(northern$geometry)
names(db)

#Transform the ontario projection system to match Statistics Canadas
northern$geometry<-st_transform(northern$geometry, crs=st_crs(db_geometry$geometry))
#ungroup northern; this is a hold-over from when on had the election results
#one row per candidate
northern<-ungroup(northern)
#### Method 1 - Simon's way 

# Put DAs into provincial districts
# First add Population variable to the geometry data frame
da %>% 
  select(GeoUID, Population) %>% 
  left_join(da_geometry, by=c("GeoUID"="DAUID"))->da_geometry

# Join da geometry to the northern boundary files using st_within
st_sf(da_geometry) %>% 
  #If a da is within a provincial district, it is kept.
  st_join(., st_sf(northern), join=st_within, left=F)->northern_da_within
#Now join da_geometry to the northern boundary files using st_touches.
# This is going to pull any DAs that *overlap* but are not *within* a district. 

st_sf(da_geometry) %>% 
  st_join(., st_sf(northern), join=st_overlaps, left=F)->northern_da_overlap

# Check if any DAs that overlap PEDS also show up in the
# within data frame
northern_da_overlap %>% 
  filter(GeoUID %in% northern_da_within$GeoUID)


#Check
ggplot(data=northern)+geom_sf(linewidth=1, col="darkred", fill=NA)+
  geom_sf(data=northern_da_within, fill="lightgrey", col="black", linetype=2)+
  labs(title="Northern Ontario DAs wholly within Northern ONtario Districts")+theme_minimal()

#Check
ggplot(data=northern)+geom_sf(linewidth=1, col="darkred", fill=NA)+
  geom_sf(data=northern_da_overlap, fill="lightgrey", col="black", linetype=2)+
  labs(title="Northern Ontario DAs wholly within Northern ONtario Districts")+theme_minimal()

#Calculate ERrors 
northern_da_within %>% group_by(ENGLISH_NA, Population.y) %>% 
  summarise(Population_hat=sum(Population.x)) %>% 
  mutate(error_pct=((Population_hat-Population.y)/Population.y)) %>% 
  ggplot(., aes(y=fct_reorder(ENGLISH_NA, error_pct), x=error_pct))+geom_col()+
  labs(title="Errors in Estimating PRovincial District POpulation from using DAs\nthat fit only in PEDs")

# In the end, this metric is not that useful. Of course the errors are 
# off. They are all showing negative errors, because we are purposefully
# excluding a whole bunch of DAs. The issue is whether the DAs that are excluded
# are systematically different than the DAs that are included. 
# I could try to get some metrics on DAs (e.g. francophones, income, doctoraotes)
# and see correlations in the st_within and the st_overlap data frames. 
#Vectors of interest


vectors=["v_CA21_1186","v_CA21_386","v_CA21_5910","v_CA21_915"]
vars<-c("Francophone"="v_CA21_1186",
        'doctorate'="v_CA21_5910", 'age'="v_CA21_386", 'income'="v_CA21_915")
vars
da_vars<-get_census(dataset="CA21", regions=list(PR="35"), vectors=vars, geo_format=NA, level="DA")
da_vars %>% 
  filter(GeoUID %in% northern_da_overlap$GeoUID)->northern_da_overlap_vars
da_vars %>% 
  filter(GeoUID %in% northern_da_within$GeoUID)->northern_da_within_vars
northern_da_overlap_vars$DA_set<-rep("Overlap", nrow(northern_da_overlap_vars))
northern_da_within_vars$DA_set<-rep("Within", nrow(northern_da_within_vars))
cor(northern_da_within_vars$`v_CA21_1186: French`, northern_da_overlap_vars$`v_CA21_1186: French`)
northern_da_overlap_vars %>% 
  bind_rows(., northern_da_within_vars) %>% 
  rename(French=12, Doctorate=13, Age=14, Income=15) ->northern_da_vars 
 # pivot_longer(French:Income) %>% 
northern_da_vars %>% 
group_by(DA_set) %>% 
  summarise(Average=mean(Income, na.rm=T))

northern_da_vars %>% 
  group_by(DA_set) %>% 
  summarise(Average=mean(French, na.rm=T))

northern_da_vars %>% 
  group_by(DA_set) %>% 
  summarise(Average=mean(Doctorate, na.rm=T))


#### Try Jens's strategy of tongfen 
#Let's just do the Thunder Bay ridings
northern %>% 
  filter(str_detect(ENGLISH_NA, pattern="Thunder"))->thunder_bay
thunder_bay %>% 
  ggplot()+geom_sf()
#Get metadata for population
meta<-meta_for_additive_variables("CA21", variables=c("Population"))
set_cancensus_api_key(key="CensusMapper_287500bb91a374ec69fdcf270fb20ff7")
set_cancensus_api_key("CensusMapper_e0bb5e9bb16c197f306a580284d35b5b")
thunder_bay_da_tongfen<-tongfen_estimate_ca_census(thunder_bay, level="DA", meta=meta)

thunder_bay_da_tongfen
#Calculate Percent Error
thunder_bay_da_tongfen %>% 
  mutate(error_pct=(Population_CA21-Population)/Population) %>%view()
#that is a goddamn very small error.

# Try to do it for the other variables
meta_vars<-meta_for_ca_census_vectors(vectors=vars)
thunder_bay_da_tongfen2<-tongfen_estimate_ca_census(thunder_bay, level="DA", meta=meta_vars, na.rm=T)
thunder_bay_da_tongfen2 %>% view()

###  Try downsampling
meta$downsample<-c("Population") 
meta_vars
### HAVE I DONE THIS RIGHT
### This returns an error of duplicated name "Population"
thunder_bay_da_tongfen<-tongfen_estimate_ca_census(thunder_bay, 
                                                   level="DA", meta=meta, 
                                                   downsample_level="DB")
#Try with the non-Population variables
meta_vars$downsample<-rep("Population", nrow(meta_vars))
thunder_bay_db_tongfen2<-tongfen_estimate_ca_census(thunder_bay, 
                                                   level="DA", meta=meta_vars, 
                                                   downsample_level="DB", na.rm=T)
#Combine results to compare

thunder_bay_db_tongfen2$Method<-rep("Downsample", 2)
thunder_bay_da_tongfen2$Method<-rep("Tongfen", 2)
thunder_bay_da_tongfen2 %>% 
  bind_rows(thunder_bay_db_tongfen2) %>% 
  select(ENGLISH_NA, Francophone:Method, -geometry) %>% 
  st_drop_geometry() %>% 
  pivot_longer(Francophone:income) %>% 
  select(-c(2:3)) %>% 
 # mutate(row = row_number()) %>%
  pivot_wider(.,  names_from=c(Method), values_from = c(value)) 

#northern_da_tongfen %>% view()
# # northern<-st_sf(northern)
# # db_geometry<-st_sf(db_geometry)
# names(db_geometry)
# names(db)
# 
# #Try tongfen 
# error1<-estimate_tongfen_correspondence(list(northern, db_geometry), 
#                                         c("ElectoralDistrictNumber","DBUID"))
# 
# error1
# names(db)
# names(error1)
# head(error1)
# head(db)
# #Grouping Dissemination blocks into Provincial districts 
# error1 %>% 
#   left_join(., db, by=c("DBUID"="GeoUID")) %>% 
#   group_by(ElectoralDistrictNumber) %>% 
#   summarise(n=sum(Population)) %>% 
#   left_join(., northern) %>% 
#   mutate(error_percent=(n-Population)/Population) ->errors
# errors %>% 
#   ggplot(., aes(y=fct_reorder(as.factor(ElectoralDistrictName), error_percent), x=error_percent))+
#   geom_col()+labs(y="Ontario Electoral District", 
#  title="Diagnosing Tongfen Estimates Aggregating Dissemination Block Population Counts To Ontario Provincial Electoral Districts",
# x="Percent Discrepancy Between Tongfen and Ontario Voter Information Service")
# 
# 
#
# 
# #vars<-"v_CA21_1186"
# 
# meta<-meta_for_additive_variables("CA21", "Population")
# meta$downsample<-c("Population")
# northern %>% 
#   filter(ElectoralDistrictNumber==105|ElectoralDistrictNumber==106)->thunder_bay
# thunder_bay_tongfen<-tongfen_estimate_ca_census(st_sf(thunder_bay$geometry),  
#                                                 meta=meta,level="DA", 
#                                                 downsample_level = "DB")
# 
# thunder_bay %>% 
#   select(2,Population)
# thunder_bay_tongfen
# db_geometry %>% 
# st_filter(., thunder_bay, .predicates=st_intersects)->thunder_bay_db
# thunder_bay %>% 
#   ggplot(., )+geom_sf(col="darkred", linewidth=1)+
#   geom_sf(data=thunder_bay_db, linetype=2, col="darkblue", fill=NA)+theme_minimal()
# 
# ### st_filter is very useful
# 

