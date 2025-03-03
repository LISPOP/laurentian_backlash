library(here)
library(tidyverse)
on_results<-read_csv(file=here("data/ontario_election_results_by_party.csv"))
on_voters<-read_csv(file=here("data/ontario_vote_totals_by_party.csv"))
glimpse(on_results)
glimpse(on_voters)
on_voters %>% 
  select(Year, TotalNumberOfNames, VoterTurnoutPercentageOfList)->on_voters
on_voters %>% 
  filter(Year>1945) %>% 
  ggplot(., aes(x=Year, y=VoterTurnoutPercentageOfList))+geom_line()+theme_minimal()
on_results
on_voters %>% 
  left_join(on_results, by="Year")->on_voters
table(on_voters$Party)
on_voters$Party2<-car::Recode(on_voters$Party, "'Progressive Conservative Party'='PC';
                        'Progressive Conservative'='PC';
                        'Progressive-Conservative'='PC';
                        'Progressive Conservative Party of Ontario'='PC';
                             'Ontario Liberal Party'='Liberal';
                             'Liberal Party'='Liberal';
                             'New Democratic'='NDP';
                             'New Democratic Party of Ontario'='NDP';
                             'New Democratic Part of Ontario'='NDP'; 
                              'New Democratic Party'='NDP';else=NA")
on_voters %>%
mutate(percent_population=`Votes Cast`/TotalNumberOfNames) %>% 
 # filter(Year==1990) %>% 
  filter(Party2=="NDP"|Party2=="PC"|Party2=="Liberal") %>% 
  ggplot(., aes(x=Year, y=percent_population, col=fct_relevel(Party2, "PC", "Liberal", "NDP")))+geom_line()+
  scale_color_manual(values=c("darkblue",  "darkred", "orange"))+labs(col="Party")+theme_minimal()
  