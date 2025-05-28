#### Background script
# This calls the scripts that
# import the ontario voting results, the ontario riding boundaries
# gets the non-northern demographics from statistics canada and estimates the demographics
# on the northern districts
source("R_Scripts/3_merge_northern_non_northern.R")
# Rename District


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
  rename(District=ElectoralDistrictName,Year=Date)->on
#This code is a check that our non_northern census data
# matches what STatscan has published
#Check 
on %>% 
  filter(northern!=1) %>% 
  filter(District=="Ajax")  
# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=ajax&DGUIDlist=2013A000435001&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0

#This code divides each demographic variable by population to get a percent
# on %>% 
#   mutate(across(c(francophones, phds, mining, certificate, first_nations), 
#                 ~(.x/population), .names="{.col}_pct"))->on
#Which ridings are the most francophone

on %>% 
  #group_by(fedcreate) %>% 
  filter(!is.na(fedcreate)) %>% 
  ggplot(., aes(x=francophones_pct))+geom_histogram()+facet_grid(~fedcreate)
on %>% 
  distinct(District, francophones_pct) %>% 
  slice_max(francophones_pct, n=10)
# Define francohpone ridings
on %>% 
  mutate(francophone=case_when(
    francophones_pct>0.2~1,
    TRUE~0
  ))->on

#Provid Sudbury dichotomous variable
on %>% 
  mutate(sudbury=case_when(
    str_detect(District, "Sudbury")~"Sudbury",
    TRUE~"Other"
  ))->on
on$sudbury<-factor(on$sudbury,  c("Sudbury", "Other"))
#Which ridings are the most mining
#Which ridings are the most francophone
on %>% 
  distinct(District, mining_pct) %>% 
  slice_max(mining_pct, n=10)
#Which ridings are the most francophone
on %>% 
  distinct(District, first_nations_pct) %>% 
  slice_max(first_nations_pct, n=10)

on %>% 
  distinct(District, age) %>% 
  slice_max(age, n=10)

on %>% 
  distinct(District, certificate_pct) %>% 
  slice_max(certificate_pct, n=10)

# Run the did 
on<-on %>% 
  mutate(treated1=case_when(
    str_detect(District, "Sudbury|Nickel")~1,
    TRUE~0
  ))
# Run the did 
on<-on %>% 
  mutate(treated2=case_when(
    francophone==1~1,
    TRUE~0
  ))
# Define closures
on %>% 
  mutate(closure=case_when(
    treated1==1&Year==2022~1,
    TRUE~ 0
  ))->on
on %>% 
  mutate(closure2=case_when(
    treated2==1&Year==2022~1,
    TRUE~ 0
  ))->on



library(fixest)
on %>% filter(Party=="PC")->pc
 
# 
theme_set(theme_minimal(base_size=24))
theme_update(legend.position="bottom")
pc_cols<-c("darkblue", "lightblue")

# PC vote result
pc %>% 
  group_by(Year) %>% 
  summarize(result=mean(Percent, na.rm=T))

#First show Sudbury versus all others
pc %>%
  ggplot(., aes(x=as.factor(Year), y=Percent, fill=fct_relevel(sudbury, "Other")))+
  geom_col(position="dodge")+
  scale_fill_manual(values=pc_cols)+labs(fill='', x="Election")
# Filter out NOrthern Ontaio
pc %>% 
  group_by(Year, northern ,sudbury) %>% 
  summarize(average=mean(Percent))
  
pc %>% 
  group_by(northern,sudbury, Year) %>% 
  filter(Year>2017) %>% 
  summarize(Average=mean(Percent))  %>% 
 #mutate(Treated=case_when(
  #  treated==0~"Untreated",
   # treated==1~"Treated (Sudbury/Nickel Belt)"
  #)) %>% 
  #mutate(Treated=factor(Treated, levels=c("Untreated", "Treated (Sudbury/Nickel Belt)"))) %>% 
  mutate(Region=case_match(northern, 0~"Southern Ontario", 1~"Northern Ontario")) %>% 
  ggplot(., aes(x=as.factor(Year), y=Average, fill=fct_relevel(sudbury, "Other")))+
  geom_col(position="dodge")+
  facet_wrap(~fct_relevel(factor(Region), "Southern Ontario"))+
  labs(x="Election", y="Average PC Vote Share", fill='')+scale_fill_manual(values=pc_cols)


# Show treated versus untreated plot
pc %>% 
  group_by(northern, treated1,Year) %>% 
  summarize(average=mean(Percent))
# Show Results
# Graph 1

pc %>% 
  filter(Year>2017) %>% 
  group_by(northern,treated1, Year) %>% 
  summarize(Average=mean(Percent))  %>% 
  mutate(Treated=case_when(
    treated1==0~"Untreated",
    treated1==1~"Treated (Sudbury/Nickel Belt)"
  )) %>% 
  mutate(Treated=factor(Treated, levels=c("Untreated", "Treated (Sudbury/Nickel Belt)"))) %>% 
mutate(Region=case_match(northern, 0~"Southern Ontario", 1~"Northern Ontario")) %>% 
ggplot(., aes(x=as.factor(Year), y=Average, fill=Treated))+
  geom_col(position="dodge")+
  facet_wrap(~fct_relevel(factor(Region), "Southern Ontario"))+
  labs(x="Election", y="Average PC Vote Share", fill='')+scale_fill_manual(values=c("darkblue", "lightblue"))+
  theme(legend.position="bottom")
ggsave(filename=here("Poster/southern_northern_ontario.png"), width=12, height=8, dpi=300)

pc %>% 
  select(District, Year, closure, District, Year, Percent) 

model1<-feols(Percent~closure|District+Year, 
              data=subset(pc, northern==1&Year>2017), cluster=c("District", "Year"))
model2<-feols(Percent~closure|District+Year, data=subset(pc, Year>2017), cluster=c("District", "Year"))
model3<-feols(Percent~closure2|District+Year, 
              data=subset(pc, northern==1&Year>2017), cluster=c("District", "Year"))
library(modelsummary)

modelsummary(list("All Ontario"=model2, "Sudbury/Nickel Belt"=model1, "Francophone Northern Ridings"=model3), stars=T, 
             coef_rename=c('closure'='Bankruptcy', 'closure2'='Bankruptcy'), 
             gof_omit = c('BIC|AIC|Within|Adj'), fmt=3, output=here("Poster/model1.png"))


