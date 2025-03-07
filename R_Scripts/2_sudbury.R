library(dplyr)
library(ggplot2)

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

#Defining northern ridings
northern_ridings <- c("Algoma-Manitoulin", "Kiiwetinoong", "Kenora-Rainy River", "Mushkegowuk- James Bay", "Nickel Belt", "Nipissing", "Sault Ste. Marie", "Sudbury", "Thunder Bay- Atikokan", "Thunder Bay- Superior North", "Timiskaming- Cochrane", "Timmins")

#Creating dummy variable
on$northern <- ifelse(on$ElectoralDistrictName %in% northern_ridings, 1, 0)

#Results
table(on$northern)

#Calculate change in vote share
on %>%
  dplyr::filter(Date > 2019) %>%
  group_by(northern) %>%
  summarise(avg_change = mean(Delta, na.rm = TRUE))
