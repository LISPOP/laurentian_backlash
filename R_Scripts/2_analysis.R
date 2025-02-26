source("R_Scripts/1_data_import.R")

# What is the average mv for each election
# Hint:
# Form groups of interest (i.e. eleciton) and then summarize those groups by calculating the 
# average (or mean)
# Calculate the average margin of victory (MV) by election

library(dplyr)

# Compute the average margin of victory (MV) per election
on_summary <- on %>%
  mutate(mv_per_ballot = mv / Votes) %>%
  group_by(Election) %>%
  summarise(avg_mv = mean(mv_per_ballot, na.rm = TRUE))

# View
print(on_summary)

#Can you make a histogram of the variable mv for each election 
# Hint use ggplot2 
# and you an want one panel 

#This gets the 15 closest districts in each election

on_filtered %>%
  group_by(Election) %>%
  slice_min(mv, n=15) %>% view()

