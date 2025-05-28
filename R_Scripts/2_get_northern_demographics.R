source("R_Scripts/1_data_import.R")


#set theme
theme_set(theme_minimal())


#Add in population
source("R_Scripts/2_get_northern_populations.R")
northern %>% 
  left_join(., northern_population) -> northern
#Check the projection systems for each


# Now get Statistics Canada Dissemination Boundary Files

#da_geometry<-get_census(dataset="CA21", regions=list(PR="35"),level="DA",geo_format="sf")
da_geometry<-get_statcan_geographies("2021", level="DA", type="digital")

#Filter Ontario
da_geometry %>%
 filter(PRUID=="35")->da_geometry

#Turn northern into the coordinate system 
northern<-st_transform(northern, crs=st_crs(da_geometry))
#Add the real population counts from Elections ontario to the da_geometry
#da_geometry %>% 
  #left_join(., on_population, by=c("DAUID"="GeoUID"))->da_data


#Get Metadata for census21
vars<-c("population"='v_CA21_1', #Population
  "francophones"="v_CA21_1186", #mother tongue
        'phds'="v_CA21_5853" ,# University degree bachelor's or higher
        'age'="v_CA21_389", # Median Age
        'income'="v_CA21_915", #average total income of households in 2020
        'density'='v_CA21_6',
        'mining'='v_CA21_6612',# 
        'certificate'='v_CA21_5826',# 15 and over with something over a high school diploma 
        'first_nations'='v_CA21_4210')
meta_vars<-meta_for_ca_census_vectors(vectors=vars)
meta_vars
#Now get the census data for the demographic varirables
meta_vars
# Get 2021 Census Data
#Note the setNames sstuff below basically creates a named vector 
on_da_vars<-get_census("CA21", 
                       level="DA", 
                       regions=list(PR="35"), 
                       vectors=setNames(meta_vars$variable,meta_vars$label), use_cache=F)

da_geometry %>% 
  left_join(on_da_vars, by=c("DAUID"="GeoUID"))->on_vars_data
class
northern_tongfen<-tongfen_estimate(target=northern, source=on_vars_data, meta=meta_vars, na.rm=T)
names(northern_tongfen)


#Calculate Error Percent
northern_tongfen %>% 
  mutate(error_percent=((population-population_real)/population_real)*100) %>% 
  ggplot(., aes(y=fct_reorder(ENGLISH_NA, error_percent), x=error_percent))+geom_col()+
  labs(x="Percent Error", y="District")



#Get 2011 Census DAta
# 
# vars<-c("francophones"="v_CA21_1186", #mother tongue
#         'phds'="v_CA21_5853" ,# University degree bachelor's or higher
#         'age'="v_CA21_389", # Median Age
#         'income'="v_CA21_915", #average total income of households in 2020
#         'density'='v_CA21_6',
#         'mining'='v_CA21_6612',# 
#         'certificate'='v_CA21_5826',# 15 and over with something over a high school diploma 
#         'first_nations'='v_CA21_4210')

#List vectors for the 2011 census and NHS vectors
vars_da_11<-c(
  "population"="v_CA11F_5",
  "francophones"="v_CA11F_227",
        'phds'="v_CA11N_1828" ,
        'age'="v_CA11F_77", # Median age
        'income'="v_CA11N_2563", 
        'area'='v_CA11F_4',
        'mining'='v_CA11N_2017',
        'certificate'='v_CA11N_1780', 
        'first_nations'='v_CA11N_1354'
        )
vars_da_11
#Get metadata
meta_vars_da_11<-meta_for_ca_census_vectors(vectors=vars_da_11)

# Now get the Dissemination Area Data and Boundary files
da11_geometry<-get_census("CA11", level="DA", regions=list(PR="35"), 
                           geo_format = "sf", 
                           setNames(meta_vars_da_11$variable,meta_vars_da_11$label), use_cache=F)

# on %>% 
#   filter(Date==2011) %>% 
#   left_join(., ontario11, by=c("ElectoralDistrictNumber"="ED_ID"))->ontario11

names(ontario11)
#northern<-st_transform(northern, crs=st_crs(da_geometry))
ontario11<-st_transform(ontario11, crs=st_crs(da11_geometry))


# Get northern ontario districts
ontario11 %>% 
  filter(ElectoralDistrictName%in% northern_ridings)->northern11

# Turn off spherical geometry
sf::sf_use_s2(FALSE)
northern11_tongfen<-tongfen_estimate(northern11, 
                                     da11_geometry, 
                                     meta=meta_vars_da_11, na.rm=T)
northern11_tongfen %>% 
  mutate(density=population/area) %>% 
  select(-area)->northern11_tongfen
#northern11_tongfen %>% 
 # mutate(across(.cols=c(francophones, phds, mining, certificate, first_nations), ~.x/population)) %>% view()

names(northern11_tongfen)
## 
northern11_tongfen %>% 
  mutate(fedcreate=rep(2003, nrow(.)))->northern11_tongfen
northern11_tongfen %>% 
  mutate(northern=rep(1, nrow(.)))->northern11_tongfen
northern_tongfen %>% 
  mutate(fedcreate=rep(2013, nrow(.)))->northern_tongfen
northern_tongfen %>% 
  mutate(northern=rep(1, nrow(.)))->northern_tongfen
northern11_tongfen<-st_transform(northern11_tongfen, st_crs(northern_tongfen))
northern_tongfen %>% 
  bind_rows(northern11_tongfen)->northern_tongfen_complete

