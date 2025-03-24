#source(here("R_Scripts/1_data_import.R"))

names(on)
on$ElectoralDistrictName
on %>% 
mutate(district_scrape=str_replace_all(ElectoralDistrictName, "--", "-"))->on
on$ElectoralDistrictNumber
on

library(rvest)
peds<-"https://en.wikipedia.org/wiki/List_of_Ontario_provincial_electoral_districts"
read_html(peds) %>% 
  html_table() %>% 
data.frame() %>% 
  select(1,4) %>% 
  mutate(ElectoralDistrictName=str_replace_all(Electoral.district, "—", "--")) %>% 
  mutate(ElectoralDistrictName=str_remove_all(ElectoralDistrictName, "\n"))->on_scrape
on_scrape %>% filter(str_detect(Electoral.district, "Haliburton"))
on_scrape %>% 
  select(-Electoral.district) %>% 
  right_join(., on, by="ElectoralDistrictName") ->on
#Check 
names(on)
on %>% 
  filter(is.na(Population))

rm(on_scrape)
