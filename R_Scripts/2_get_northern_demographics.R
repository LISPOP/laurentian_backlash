source("R_Scripts/1_data_import.R")


#set theme
theme_set(theme_minimal())

northern
#Add in population
source("R_Scripts/2_get_northern_populations.R")
northern %>% 
  left_join(., northern_population) -> northern
#Check the projection systems for each


# Now get Statistics Canada Dissemination Boundary Files

da_geometry<-get_statcan_geographies("2021", level="DA", type="digital")
#Filter Ontario
da_geometry %>% 
  filter(PRUID=="35")->da_geometry
names(da_geometry)

#Get Population Counts for 
meta<-meta_for_additive_variables("CA21", "Population")
meta
on_population<-get_census("CA21", level="DA", regions=list(PR="35"))
#Now we have to join the data and the geometries

names(da_geometry)
names(on_population)
northern<-st_transform(northern, crs=st_crs(da_geometry))

da_geometry %>% 
  left_join(., on_population, by=c("DAUID"="GeoUID"))->da_data
northern %>% 
  rename(Population_source=Population)->northern
northern_tongfen<-tongfen_estimate(target=northern, source=da_data, meta=meta, na.rm=T)
view(northern_tongfen)
#Calculate Error Percent
northern_tongfen %>% 
  mutate(error_percent=(Population/Population_source)/Population_source) %>% 
  ggplot(., aes(y=fct_reorder(ENGLISH_NA, error_percent), x=error_percent))+geom_col()+
  labs(x="Percent Error", y="District")
#view(northern_tongfen)
#now get rest of demographigcs
vars<-c("francophones"="v_CA21_1186",
        'phds'="v_CA21_5910" ,
        'age'="v_CA21_386", 
        'income'="v_CA21_915")
meta_vars<-meta_for_ca_census_vectors(vectors=vars)
meta_vars
#Now get the census data for the demographic varirables


#Note the setNames sstuff below basically creates a named vector 
on_da_vars<-get_census("CA21", level="DA", regions=list(PR="35"), vectors=setNames(meta_vars$variable,meta_vars$label))
#northern$Population<-northern$Population_source
da_geometry %>% 
  left_join(on_da_vars, by=c("DAUID"="GeoUID"))->on_vars_data

on_da_tongfen<-tongfen_estimate(northern, on_vars_data, meta=meta_vars, na.rm=T)

