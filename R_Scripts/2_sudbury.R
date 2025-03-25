source("R_Scripts/1_data_import.R")
on %>% 
  filter(Party=="PC") %>% 
  arrange(ElectoralDistrictName, Election) %>% 
  mutate(Delta=Percent-lag(Percent, n=1)) ->on
on %>% 
  filter(str_detect(ElectoralDistrictName, "Sudbury"))
on %>% 
  mutate(Sudbury=case_when(
    str_detect(ElectoralDistrictName, "Sudbury")~"Sudbury",
    TRUE~"Other",
  ))->on
on$Sudbury<-factor(on$Sudbury, levels=c("Other", "Sudbury"))
on %>% 
  group_by(Sudbury) %>% 
  filter(Date>2019) %>% 
  summarize(avg=mean(Delta))
on$Sudbury
mod1<-lm(Percent~Date*Sudbury, data=on)
summary(mod1)
on %>% 
  filter(Party=="PC") %>% 
  ggplot(., aes(x=as.factor(Date), y=Percent, col=Sudbury))+geom_point()+scale_color_manual(values=c("lightgrey", "darkred"))+
  geom_smooth(method="lm")+geom_jitter()

install.packages("kableExtra")

# Load necessary libraries
library(dplyr)
library(stringr)

# Load data
source("R_Scripts/1_data_import.R")

# Filter for PC party and calculate Delta
on <- on %>%
  filter(Party == "PC") %>%
  arrange(ElectoralDistrictName, Election) %>%
  mutate(Delta = Percent - lag(Percent, n = 1))

# List of Northern ridings
northern_ridings <- c("Sudbury", "Thunder Bay-Atikokan", "Thunder Bay-Superior North", 
                      "Timmins", "Kenora-Rainy River", "Mushkegowuk-James Bay", "Algoma-Manitoulin")

# Create ONE dummy variable for Northern ridings
on <- on %>%
  mutate(northern = ifelse(str_detect(ElectoralDistrictName, paste(northern_ridings, collapse = "|")), 1, 0))

# Check that the dummy variable was created correctly
table(on$northern)  # Should show counts of 0s and 1s

# Run regression model using the single northern dummy variable
mod1 <- lm(Percent ~ Date + northern, data = on)

# View model summary
summary(mod1)

