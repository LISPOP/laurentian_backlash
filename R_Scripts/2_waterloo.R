#This is some quick code to look at Waterloo Region Ridings

source("R_Scripts/1_data_import.R")
#Average margin of victory

on_filtered %>% 
  group_by(Election) %>% 
  summarize(avg=mean(mv, na.rm=T), sd=sd(mv, na.rm=T))
#average margin of victory was 7.7% of total valid ballots cast, standard deviation was 5%
# So, The on average, districts were about 5% away from 7.7%
on_filtered%>% 
  filter(str_detect(ElectoralDistrictName, "Kitchen|Waterloo|Cambr")) %>% 
  arrange(desc(mv)) %>% view()
