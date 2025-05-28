#install.packages("here")
library(here)
source(here("R_Scripts/3_regression.R"))
#install.packages("Synth")
#install.packages("SCtools")
library(Synth)
library(SCtools)
#Filter on to only have the pc

pc<-as.data.frame(pc)
#Francophone ridings as controls
# mining ridings as controls
# university towns as controls
# Geographic neigbhours as controls

# neighbouring districts
# Select unchanged northern boundaries
pc %>% 
  filter(northern==1&sudbury!="Sudbury") %>% 
  filter(Year>2017) %>% 
  select(District) %>% distinct() %>% unlist()->northern_controls

data_northern<-dataprep(foo=subset(pc, Year>2017), 
         predictors=c("francophones_pct", "mining_pct", "income", "phds_pct",  "first_nations_pct", "density"),
         time.predictors.prior=2022,
         time.variable="Year",
         dependent="Percent",
         unit.variable="ED_ID",
         unit.names.variable = "District",
         treatment.identifier = "Sudbury",
         controls.identifier = northern_controls,
         time.optimize.ssr = 2018,
         time.plot=c(2018,2022)
         )

mod_northern<-synth(data_northern)
mod_northern
#What are the weights of the controls?
northern_tab<-synth.tab(mod_northern, data_northern)
northern_tab
#In the help documentation for path.plot
# This is the equation that returns the trajectory for the synthetic unit
data_northern$Y0plot%*% mod_northern$solution.w %>% 
  #Convert to data frame
  as.data.frame() %>% 
  #add years
  mutate(Year=c(2018, 2022)) %>% 
  rename(Percent=w.weight) %>% 
  mutate(Group=rep("Synthetic",2))->synthetic_results_northern


#Get control results=
pc %>% 
  filter(northern==1&sudbury!="Sudbury") %>% 
  filter(Year>2017) %>% 
  group_by(Year, District) %>% 
  select(Year, Percent) %>% group_by(Year) %>% 
  summarize(Percent=mean(Percent)) %>% 
  mutate(Group=rep("Control", nrow(.)))->control_results
control_results
pc %>% 
  filter(., District=="Sudbury"&Year>2017) %>% 
  select(Year, Percent) %>% 
  mutate(Group=rep("Sudbury", 2))->sudbury_results
synthetic_results_northern %>% 
  bind_rows(sudbury_results) %>% 
  bind_rows(., control_results) %>% 
  ggplot(., aes(x=as.factor(Year), 
                y=Percent, 
                fill=fct_relevel(Group, "Control", "Synthetic","Sudbury")))+
  geom_col(position="dodge")+labs(fill="Group", x="Election")+
  scale_fill_manual(values=c("darkblue", "lightblue", "cyan"))+theme(legend.position="bottom")
# data_neighbours<-dataprep(foo=subset(pc, Year>2017), 
#                            predictors=c("francophones_pct", "mining_pct","certificate_pct", "income", "phds_pct", "density", "first_nations_pct", "age"),
#                            time.predictors.prior=2018,
#                            time.variable="Year",
#                            dependent="Percent",
#                            unit.variable="ED_ID",
#                            unit.names.variable = "District",
#                            treatment.identifier = "Sudbury",
#                            controls.identifier = neighbours$District,
#                            time.optimize.ssr = 2018,
#                            time.plot=c(2018,2022)
# )
# 
# 
# mod_neighbours<-synth(data_neighbours)
# path.plot(mod_neighbours, data_neighbours)#Note Need a barplot
# #In the help documentation for path.plot
# # This is the equation that returns the trajectory for the synthetic unit
# data_neighbours$Y0plot%*% mod_neighbours$solution.w %>% 
#   #Convert to data frame
#   as.data.frame() %>% 
#   #add years
# mutate(Year=c(2018, 2022)) %>% 
#   rename(Percent=w.weight) %>% 
#   mutate(Group=rep("Synthetic",2))->synthetic_results
# 
# #Get control results
# pc %>% 
#   filter(District%in% neighbours$District) %>% 
#   group_by(Year, District) %>% 
#   select(Year, Percent) %>% group_by(Year) %>% 
#   summarize(Percent=mean(Percent)) %>% 
# mutate(Group=rep("Control", nrow(.)))->control_results
# control_results
# pc %>% 
#   filter(., District=="Sudbury"&Year>2017) %>% 
#   select(Year, Percent) %>% 
#   mutate(Group=rep("Sudbury", 2))->sudbury_results
# synthetic_results %>% 
#   bind_rows(sudbury_results) %>% 
#   bind_rows(., control_results) %>% 
#   ggplot(., aes(x=as.factor(Year), 
#                 y=Percent, 
#                 fill=fct_relevel(Group, "Control", "Synthetic","Sudbury")))+
#   geom_col(position="dodge")+labs(fill="Group", x="Election")+
#   scale_fill_manual(values=c("darkblue", "lightblue", "cyan"))+theme(legend.position="bottom")
# ggsave(filename=here("Poster/synthetic_control.png"), width=12,height=8)
  #join back to vote results for sudbury


