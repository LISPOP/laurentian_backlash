#install.packages("here")
library(here)
source(here("R_Scripts/3_regression.R"))
#install.packages("Synth")
#install.packages("SCtools")
library(Synth)
library(SCtools)
#Filter on to only have the pc
on$northern
on %>% 
  mutate(Year=case_when(
    str_detect(Election, "2018")~2018,
               str_detect(Election, "2022")~2022)
  )->on
on %>% 
  filter(str_detect(ElectoralDistrictName,"Sudbury"))
on %>% 
  filter(Party=="PC")->pc
pc %>% 
  select(ED_ID, ElectoralDistrictName, Year, Percent, northern, contains("_pct"), density, age, income)->pc
pc$Year
glimpse(pc)
pc<-as.data.frame(pc)
#Francophone ridings as controls
# mining ridings as controls
# university towns as controls
# Geographic neigbhours as controls

# on 
on %>% 
  #group_by(ElectoralDistrictName) %>% 
  summarize(avg=mean(francophones_pct), sd=sd(francophones_pct))
0.016+0.065
pc %>% 
  filter(francophones_pct>0.081) %>% 
  distinct(ElectoralDistrictName) %>% 
  filter(ElectoralDistrictName!="Sudbury")->francophones

# neighbouring districts

pc %>% 
  filter(northern==1) %>% 
  distinct(ElectoralDistrictName)->neighbours
pc
data_francophone<-dataprep(foo=pc, 
         predictors=c("francophones_pct", "mining_pct", "income", "phds_pct", "density", "first_nations_pct"),
         time.predictors.prior=2018,
         time.variable="Year",
         dependent="Percent",
         unit.variable=1,
         unit.names.variable = "ElectoralDistrictName",
         treatment.identifier = 103,
         controls.identifier = francophones$ElectoralDistrictName,
         time.optimize.ssr = 2018,
         time.plot=c(2018,2022)
         )
mod_francophone<-synth(data_francophone)
path.plot(mod_francophone, data_francophone)
names(pc)
head(pc)
francophones
data_neighbours<-dataprep(foo=pc, 
                           predictors=c("francophones_pct", "mining_pct","certificate_pct", "income", "phds_pct", "density", "first_nations_pct", "age"),
                           time.predictors.prior=2018,
                           time.variable="Year",
                           dependent="Percent",
                           unit.variable=1,
                           unit.names.variable = "ElectoralDistrictName",
                           treatment.identifier = 103,
                           controls.identifier = neighbours$ElectoralDistrictName,
                           time.optimize.ssr = 2018,
                           time.plot=c(2018,2022)
)
neighbours
pc %>% 
  filter(ElectoralDistrictName%in% neighbours$ElectoralDistrictName)
mod_neighbours<-synth(data_neighbours)
path.plot(mod_neighbours, data_neighbours)#Note Need a barplot
#In the help documentation for path.plot
# This is the equation that returns the trajectory for the synthetic unit
data_neighbours$Y0plot%*% mod_neighbours$solution.w %>% 
  #Convert to data frame
  as.data.frame() %>% 
  #add years
mutate(Year=c(2018, 2022)) %>% 
  rename(Percent=w.weight) %>% 
  mutate(Group=rep("Synthetic",2))->synthetic_results

#Get control results
pc %>% 
  filter(ElectoralDistrictName%in% neighbours$ElectoralDistrictName) %>% 
  group_by(Year, ElectoralDistrictName) %>% 
  select(Year, Percent) %>% group_by(Year) %>% 
  summarize(Percent=mean(Percent)) %>% 
mutate(Group=rep("Control", nrow(.)))->control_results
control_results
pc %>% 
  filter(., ElectoralDistrictName=="Sudbury") %>% 
  select(Year, Percent) %>% 
  mutate(Group=rep("Sudbury", 2))->sudbury_results
synthetic_results %>% 
  bind_rows(sudbury_results) %>% 
  bind_rows(., control_results) %>% 
  ggplot(., aes(x=as.factor(Year), 
                y=Percent, 
                fill=fct_relevel(Group, "Sudbury", "Synthetic", "Control")))+
  geom_col(position="dodge")+labs(fill="Group")
  #join back to vote results for sudbury

#What are the weights of the controls?
neighbours_tab<-synth.tab(mod_neighbours, data_neighbours)
neighbours_tab

# Try Northern Ontario Cities
pc %>% 
  filter(ElectoralDistrictName%in% neighbours$ElectoralDistrictName) %>% 
  select(Year, ElectoralDistrictName, Percent)
  