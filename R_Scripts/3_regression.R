#### Background script
# This calls the scripts that
# import the ontario voting results, the ontario riding boundaries
# gets the non-northern demographics from statistics canada and estimates the demographics
# on the northern districts
source("R_Scripts/2_get_non_northern_demographics.R")


#This deals with the FED variable which is stored as characters
#Brynn, this is what you did, assigning the FED codes to each provincial district
# in Ontario
on$FED<-as.character(on$FED)

#This merges the northern demographicdata data into the `on` dataframe that Brynn and Nicole constructed
# it effectively takes the demographic statistics for northern ridings
# that have been estimated in the script 2_get_northern_demographics
# and merges those statistics to the `on` object
names(on_da_tongfen)

#I have waited a long time to do this. 
# This is the object created by the tongfen procedure
on_da_tongfen %>% 
  #pick out just these key variables
  select(ED_ID, ENGLISH_NA, Population_source, francophones:first_nations) %>% 
  #Drop the geometry e.g. the boudnaries of each district
  st_drop_geometry() %>% 
  #Now join it to `on` matching the EDID variable in on_da_tongfen
  # with the ElectoralDistrictNumber variable in `on`
  right_join(., on, by=c("ENGLISH_NA"="ElectoralDistrictName"))->on
#Check
names(on)
head(on)
#view(on)
#We are now missing the non_northern_data
# This was pulled directly from Statistics Canada in 2_get_non_northern_demographics.R
#This drops a variable we don't need
non_northern_data %>% 
  select(-DGUID)->non_northern_data

#This renames teh population variable in `on` in order to match
# the population variable in the non_northern data
names(on)
on %>% 
  rename(population=Population_source) %>% 
  #This combines the `on` data with the non_northern data
  rows_patch(., non_northern_data, by=c("FED"))->on
on %>% 
distinct(ENGLISH_NA, Date) %>% 
  count(Date)
#This drops a few variables
on %>% 
  select(-IsGeneralElection, -ResignedMPPName)->on
#This code is a check that our non_northern census data
# matches what STatscan has published
#Check 
on %>% 
  filter(northern!=1) %>% 
  filter(ENGLISH_NA=="Ajax")  
# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=ajax&DGUIDlist=2013A000435001&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0

#This code divides each demographic variable by population to get a percent
on %>% 
  mutate(across(c(francophones, phds, mining, certificate, first_nations), 
                ~(.x/population), .names="{.col}_pct"))->on
#Which ridings are the most francophone
on %>% 
  distinct(ENGLISH_NA, francophones_pct) %>% 
  slice_max(francophones_pct, n=10)

#Which ridings are the most mining
#Which ridings are the most francophone
on %>% 
  distinct(ENGLISH_NA, mining_pct) %>% 
  slice_max(mining_pct, n=10)
#Which ridings are the most francophone
on %>% 
  distinct(ENGLISH_NA, first_nations_pct) %>% 
  slice_max(first_nations_pct, n=10)

on %>% 
  distinct(ENGLISH_NA, age) %>% 
  slice_max(age, n=10)

on %>% 
  distinct(ENGLISH_NA, certificate_pct) %>% 
  slice_max(certificate_pct, n=10)

# Run the did 

on<-on %>% 
  mutate(treated=case_when(
    str_detect(ENGLISH_NA, "Sudbury|Nickel")~1,
    TRUE~0
  ))

# Define closures
on %>% 
  mutate(closure=case_when(
    treated==1&Date==2022~1,
    TRUE~ 0
  ))->on

# Rename District
on %>% 
  rename(District=ENGLISH_NA, Year=Date)->on
library(fixest)
on %>% filter(Party=="PC")->pc
#pc %>% filter(Date==2011&ENGLISH_NA=="Brampton--Springdale") %>% view()
#pc %>% 
#  group_by(Date, ENGLISH_NA, treated) %>% count() %>% filter(n!=1)

pc$Year
# Show treated versus untreated plot
pc %>% 
  group_by(treated,Year) %>% 
  summarize(average=mean(Percent))
# Show Results
# Graph 1
# 
theme_set(theme_minimal(base_size=24))

pc %>% 
  group_by(northern,treated, Year) %>% 
  summarize(Average=mean(Percent))  %>% 
  mutate(Treated=case_when(
    treated==0~"Untreated",
    treated==1~"Treated (Sudbury/Nickel Belt)"
  )) %>% 
  mutate(Treated=factor(Treated, levels=c("Untreated", "Treated (Sudbury/Nickel Belt)"))) %>% 
mutate(Region=case_match(northern, 0~"Southern Ontario", 1~"Northern Ontario")) %>% 
ggplot(., aes(x=as.factor(Year), y=Average, fill=Treated))+
  geom_col(position="dodge")+
  facet_wrap(~fct_relevel(factor(Region), "Southern Ontario"))+
  labs(x="Election", y="Average PC Vote Share")+scale_fill_manual(values=c("darkblue", "lightblue"))+
  theme(legend.position="bottom")
ggsave(filename=here("Poster/southern_northern_ontario.png"), width=12, height=8, dpi=300)


model1<-feols(Percent~closure|District+Year, data=subset(pc, northern==1), cluster=c("District", "Year"))
model2<-feols(Percent~closure|District+Year, data=pc, cluster=c("District", "Year"))
library(modelsummary)
modelsummary(list("Northern Ontario"=model1, "All Ontario"=model2), stars=T, 
             coef_rename=c('closure'='Bankruptcy'), 
             gof_omit = c('BIC|AIC|Within|Adj'), fmt=3)


