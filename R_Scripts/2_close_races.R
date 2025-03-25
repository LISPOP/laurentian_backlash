source("R_Scripts/1_data_import.R")
# It would be informative for our blog post if we were able to say that
# the swing ridings were mostly PC-Liberal, PC-NDP or NDP-Liberal, right? 
# For that though, we need to somehow extract the information for each district
# which party finished first an wd which party finished second. 
# so we need to add one or two more columns at least. 
# I am not sure.


# This is exceptionally hard. It took me about two hours to do. 

#First we can get the first place party for each district
#Always start with the data frame
on %>% 
  #Form the groups of interest
  #In this case, Election and Electoral district 
  #We want the first and second place party names in each district
  #in each riding
  group_by(ElectoralDistrictNumber, Election) %>% 
  #Then we use slice_max, which is the opposite of slice_min
  slice_max(Votes) %>% 
  #And we pull out just a couple of columns 
  #specifically The Name of the candidate and the party
  #and we dump it into a new data frame called first_place
  select(Party)->first_place
first_place
#Now, in the first_place data frame
first_place %>% 
  #We take out the groups
  ungroup %>% 
  #And add a new column called Winner
  mutate(Winner=case_when(
    #And we just say
    #If there is any value in the variable called Party
    #i.e. if it is not a missing value
    #Then just return the value for Party
    !is.na(Party)~Party
  )) %>% 
  select(-Party) %>% 
  #Then we merge that data frame with on
  #and re-write over on
  right_join(., on, by=c("ElectoralDistrictNumber", "Election"))->on

#Check it out
#Look for the variable Winner
view(on)

#Then we repeat to find the Second place candidate
#Always start with the data frame you are working with
on %>% 
  #Now, we want to work on the groups made by election and by electoral district number
  group_by(ElectoralDistrictNumber, Election) %>%
  #This is different
  #First we have to arrange the data frame in descending number of votes
  arrange(desc(Votes), .by_group=T) %>% 
  #Then we slice the second row, get it, the second place candidate
  #This is like a cousin of slice_max and slice_min
  slice(2) %>% 
  #select the name of candidates and the party and dump it into second_place
  select(Party) ->second_place
second_place
#And we repeat the code above
second_place %>% 
  ungroup %>% 
  mutate(Second=case_when(
    !is.na(Party)~Party
  )) %>% 
  select(-Party) %>% 
  right_join(., on) ->on

on %>% filter(ElectoralDistrictNumber==1) %>% view()

#Now we can easily combine those two.
on %>% 
  unite(., Race, c(Winner, Second), sep="-", remove=F)->on
#check

on$Race
#Filter out rows where mv is 0

on_filtered <- on %>% filter(mv !=0)


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
  write_csv(., file=here("Tables", "swings.csv"))

#Export swings in kable format
#kable(format="html", digits=3) %>% 
 # save_kable(., file=here("Tables", "swings.html")) 

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
races %>% 
  write_csv(., file=here('Tables', 'races.csv'))
library(kableExtra)

races %>% 
  kable(., format="html") %>% 
  save_kable(., file=here("Tables", "races.html"))

#PRint out hte full results
on_filtered
on_filtered %>% 
  mutate(Election=factor(Election, levels=c("2022 General Election", "2018 General Election"))) %>% 
  group_by(Election) %>% 
  arrange(mv, .by_group = T) %>% 
select(-IsGeneralElection, -Second, -Winner) %>%view()
    kable(., format="html", digits=2) %>% 
  save_kable(., file=here("Tables", "full_results.html"))
