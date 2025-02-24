source("R_Scripts/1_data_import.R")

# What is the average mv for each election
# Hint:
# Form groups of interest (i.e. eleciton) and then summarize those groups by calculating the 
# average (or mean)

on_filtered %>% 
  group_by(Election) %>% 
  summarize(Average=mean(mv), SD=sd(mv)) %>% 
  kable(., digits=2, format="html") %>% 
  save_kable(., file=here("Tables", "averages.html"))

#Can you make a histogram of the variable mv for each election 
# Hint use ggplot2 
# and you an want one panel 

on_filtered %>% 
  ggplot(., aes(x=mv))+geom_histogram()+
  facet_wrap(~Election) +theme_minimal()+labs(x="Margin of Victory/ Total Votes Cast")
  ggsave(filename=here("Plots", "margins.png"))
#This gets the 15 closest districts in each election

library(knitr)
library(kableExtra)

# Get the Swing ridings
on_filtered %>%
 # group_by(Election) %>%
  #slice_min(mv, n=15) %>% 
  filter(str_detect(Election, "2018")&mv<(0.1-0.03)|
           str_detect(Election, "2022")&mv<(0.08-0.05)) %>% 
  select(Election, ElectoralDistrictName, NameOfCandidates, `Winning Party`=Party, `First-Second`=Race,Margin=mv) %>% 
  group_by(Election, `First-Second`) %>% 
  arrange(Margin, .by_group=T) ->swings
#Export swings in excel
swings %>% 
  mutate(Margin=round(Margin, 3)) %>% 
  write.csv(., file=here("Tables", "swings.csv"))

#Export swings in kable format
  kable(format="html", digits=3) %>% 
  save_kable(., file=here("Tables", "swings.html")) 

#Count the number of races by first and second
on_filtered %>%
  # group_by(Election) %>%
  #slice_min(mv, n=15) %>% 
  filter(str_detect(Election, "2018")&mv<(0.1-0.03)|
           str_detect(Election, "2022")&mv<(0.08-0.05)) %>% 
  select(Election, ElectoralDistrictName, NameOfCandidates, `Winning Party`=Party, `First-Second`=Race,Margin=mv) %>% 
  group_by(Election, `First-Second`) %>% 
count() %>% 
  arrange(desc(Election),desc(n)) ->races
library(kableExtra)
races %>% 
  kable(., format="html") %>% 
  save_kable(., file=here("Tables", "races.html"))
