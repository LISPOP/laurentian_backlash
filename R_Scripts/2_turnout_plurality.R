source("R_Scripts/1_data_import.R")
#Import 2022 turnotu file 
on22_turnout<-read_csv(file=here("data/turnout_2022.csv"))
on18_turnout<-read_csv(file=here("data/turnout_2018.csv"))
names(on22_turnout)
#Extract electoral district numbers for merging with the original ontario files 
on18_turnout %>% 
  mutate(Number=str_extract(`Electoral District`, "^[:digit:]{3}"), 
         `Electoral District`=str_remove_all(`Electoral District`, "^[:digit:]{3} - "))->on18_turnout

#Extract electoral district numbers for merging with the original ontario files 
on22_turnout %>% 
  mutate(Number=str_extract(`Electoral District`, "^[:digit:]{3}"), 
         `Electoral District`=str_remove_all(`Electoral District`, "^[:digit:]{3} - "))->on22_turnout

on18_turnout$Number<-as.numeric(on18_turnout$Number)
on22_turnout$Number<-as.numeric(on22_turnout$Number)


on18_turnout$Election<-rep("2018 General Election", nrow(on18_turnout))
on22_turnout$Election<-rep("2022 General Election", nrow(on22_turnout))
on18_turnout %>% names()
on18_turnout %>% 
  select(-c(Year, `Election Date`, `Election Type`, `Electoral District`, `Number of Candidates`,  `Rejected Ballots`, `Unmarked Ballots`, `Declined Ballots`, `Valid Ballots`, `...13`))->on18_turnout
on18_turnout
on22_turnout %>% 
  select(-c(Year, `Election Date`, `Election Type`, `Electoral District`, `Number of Candidates`,`Rejected Ballots`, `Unmarked Ballots`, `Declined Ballots`, `Valid Ballots`, `...13`))->on22_turnout

view(on)
on %>% 
 left_join(.,on18_turnout, join_by("Election", "ElectoralDistrictNumber"=="Number")) %>% 
  left_join(., on22_turnout, join_by("Election", "ElectoralDistrictNumber"=="Number")) ->on
view(on)
on$Registered_Voters<-coalesce(on$`Registered Voters.x`, on$`Registered Voters.y`)
on$Votes_Cast<-coalesce(on$`Votes Cast.x`, on$`Votes Cast.y`)
on$Voter_Turnout<-coalesce(on$`Voter Turnout.x`, on$`Voter Turnout.y`)
on$Turnout_Percent<-on$Votes_Cast/on$Registered_Voters
names(on)
on %>% 
  select(-ends_with(".x")&-ends_with(".y"))->on
names(on)
view(on)
on$Party<-factor(on$Party, levels=c("PC", "NDP", "Liberal", "Green", "IND"))
on %>% 
  filter(mv!=0) %>% 
  ggplot(., aes(x=`Turnout_Percent`, y=mv, col=Party))+
  geom_point()+facet_wrap(~Election)+geom_smooth(method="lm", se=F) +theme_minimal()+
  scale_color_manual(values=c("lightblue", "orange", "darkred", "darkgreen", "darkgrey"))
view(on)
view(on)
on %>% 
  select(-IsGeneralElection, -ElectoralDistrictNumber, -Date, -ResignedMPPName) %>% 
  group_by(Election) %>% 
  filter(mv>0) %>% 
mutate(rank=row_number(mv)) ->ranked_on
 ranked_on %>% view()
 library(kableExtra)

on %>% 
  left_join(., ranked_on)%>% 
  group_by(Election, ElectoralDistrictNumber) %>% 
  arrange(rank, .by_group = T) %>% 
  fill(rank, .direction = "down") %>%
  ungroup() %>% names()
  mutate(Election=factor(Election, levels=c("2022 General Election", "2018 General Election"))) %>% 
  select(-IsGeneralElection,-ElectoralDistrictNumber, -Date, -ResignedMPPName, -n, -Voter_Turnout, -Votes_Cast, -Registered_Voters) %>% 
  filter(str_detect(Party, "NDP|Green|PC|Liberal")|str_detect(NameOfCandidates, "Bobbi Ann Brady"))  %>% 
  #group_by(Election, ElectoralDistrictNumber) %>% 
  #mutate(ElectoralDistrictName=fct_reorder(ElectoralDistrictName, desc(rank))) %>% 
arrange(Election, rank, desc(Votes), desc(Percent), .by_group = T) %>% 
  #select(-ElectoralDistrictNumber) %>% 
  #arrange(desc(Percent), .by_group=T) 
  select(-rank) %>% 
  kable(., format="html", digits=2) %>% 
  collapse_rows(columns = 1:2, valign = "top") %>% 
  save_kable(., file=here('Tables', "full_results.html"))

# -------------------------------------------------------------------------


#Try to take the Ontario 2018 turnout and 

