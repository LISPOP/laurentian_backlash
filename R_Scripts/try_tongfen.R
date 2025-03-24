list.of.packages<-c("tongfen", "cancensus", "here", "tidyverse", "sf", "rvest", "readxl", "knitr", "kableExtra")
#INstall if necessary
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tongfen)
library(cancensus)
library(tidyverse)
library(sf)
library(here)
set_cancensus_cache_path(here("data/statscan"))
#list_census_regions(dataset="CA21", use_cache = TRUE, quiet = FALSE) %>% view()
#das<-get_statcan_geographies(regions=list(PR="35"), level="DB", census_year="2021", type="digital")
db<-get_census(regions=list(PR="35"), level="DB", dataset="CA21")
#db_geometry<-get_statcan_geo_suite(level="DB", census_year="2021")
# #This is the URL of dissemination block geometries
# db_url<-"https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ldb_000b21a_e.zip"
# #tempfile 
# tmp<-tempfile()
# #specify directory to extract the zip contents to 
# outdir<-file.path(here("data/statscan_db/"))
# #Increase timeout
# getOption("timeout")
# options(timeout = max(300, getOption("timeout")))
# getOption("timeout")
# #download the url to the temp file 
# download.file(url=db_url, destfile=tmp, mode="wb")
# unzip(tmp, exdir=outdir)
#read in dB geometries
db_geometry<-st_read(dsn=here("data/statscan_db/"))
#Filter ontario dissemination blocks
names(db_geometry)
db_geometry %>% 
  filter(PRUID=="35")->db_geometry_ontario

#Merge the population counts with the geometry files
names(db)
names(db_geometry_ontario)
db %>% 
  left_join(., db_geometry_ontario, by=c("GeoUID"="DBUID"))->db_population_geometry
head(db_population_geometry)
# das %>% 
#   filter(PRUID=="35")->das
#read in the electoral districts boundary files
ontario<-read_sf(here("data/electoral_districts/"))
names(ontario)
#Replace long hyphens with double-dashes for uniformity. Fuck this is frustrating. 
ontario %>% 
  mutate(ENGLISH_NA = str_replace_all(ENGLISH_NA, "—", "--"))->ontario
names(ontario)
#Get population Counts
# by calling the scxript that reads in Ontario election results
# part of this script scrapes population counts from wikipedia
source(here("R_Scripts/1_data_import.R"))
# object on has the 2018 and 2022 election results and pouplation counts
# object ontario has the boundary files
# We want population from on to merge to Ontario
names(ontario)
names(on)
#Take on that has ED numbers, Population and District names and results
on %>% 
  select(c("ElectoralDistrictNumber", "Population", "ElectoralDistrictName")) %>% 
  #Select distinct rows 
  # Because the data frame has multiple rows per district
  #one row per candidate
  distinct() %>% 
  #join back to ontario that has the boundary files
  left_join(., ontario, by=c("ElectoralDistrictNumber"="ED_ID"))->ontario
table(ontario$ENGLISH_NA)
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
#Check the projection systems for each
#st_crs(ontario)
#st_crs()
st_crs(northern$geometry)
names(db)
db_population_geometry
#Transform the ontario projection system to match Statistics Canadas
db_population_geometry
northern$geometry<-st_transform(northern$geometry, crs=st_crs(db_population_geometry$geometry))
#ungroup northern; this is a hold-over from when on had the election results
#one row per candidate
northern<-ungroup(northern)

northern<-st_sf(northern)
db_population_geometry<-st_sf(db_population_geometry)
#Try tongfen 
error1<-estimate_tongfen_correspondence(list(northern, db_population_geometry), 
                                        c("ElectoralDistrictNumber","GeoUID"))
head(error1)
names(northern)
head(db)
head(northern)
northern$Population
db$Population
#remove commas
northern$Population<-str_remove_all(northern$Population, ",")
northern$Population<-as.numeric(northern$Population)

error1 %>% 
  left_join(., db, by="GeoUID") %>% 
  group_by(ElectoralDistrictNumber) %>% 
  summarise(n=sum(Population)) %>% 
  left_join(., northern) %>% 
  mutate(error_percent=(n-Population)/Population) ->errors
errors %>% 
  ggplot(., aes(y=as.factor(ElectoralDistrictName), x=error_percent))+geom_col()
errors %>% 
  filter(abs(error_percent)>0.05)

check_tongfen_areas(list(northern, db_population_geometry), error1)
