library(tidyverse)
#This command samples from 1 and 0 randomly
gender<-sample(c("Male", "Female"),replace=T, size=10)
#check what we have made
gender
#Now make a variable with some random grades
grades<-rnorm(n=10, mean=62, sd=10)
#Check 
grades
#year of study
year<-sample(c(1,2,3,4), replace=T, size=10)
#Check
year
#now make a data frame of all of these variables, called df
df<-data.frame(gender, grades, year)
#Check
df
#use glimpse
glimpse(df)
#Start with the dataframe you are working with and pipe it 
df %>% 
  #then use the filter command to select some rows that meet some criteria on some variable. 
  #For example, we could just look at the males
  filter(gender=="Male")

#Or, we could see the first years
# Note when it is a number, do not use quotation marks
df %>% 
  filter(year==2)
#Or we could pick those that have grades that are passing
df %>% 
  filter(grades>50)
#Or we could also pick some rows that *do not* meet some criteria.
#Let's get those that are *not* in year 4
df %>% 
  filter(year!=4)

#When you have a filter command that meets the criteria you want, then you can save it into a new objectd.

df %>% 
  #Note the -> is the opposite of what we usually do which is<- when it is at the end of a pipel
  filter(year==4)->df2
#Now check
df2
#If you are really certain, you can overwrite the original df.
#In our case, we do want to do that,.
# Remember this is all low-stakes, because if you mess it up, we can just re-write the script. 
#For example.
#Compare
df
#with
df %>% 
  filter(gender=="Male")->df
df


# For Brynn
# What you want to do is something like this:

# LEt's say you have a dataset that has the class enrollment sizes for classes in a school.
# let's make a variable that repots a number of classes
# 
#This samples from the numbers 1,2,3,4 12 times replacing each time
classroom<-sample(seq(1,4,1), size=12, replace=T)
#Check
classroom
#Then let's make some fake schools. 
#This just samples from the words below 12 times, replacing a word each time
schools<-sample(c("brook", "wood", "heights", "forest"), size=12, replace=T)
#check
schools

#then let's make some fake enrollments
sizes<-rnorm(n=12, mean=25, sd=4)
#Check
sizes
#Let's get rid of the decimeals
sizes<-round(sizes, 0)
#Check
sizes

#Put all this in a dataframe called enroll

enroll<-data.frame(classroom, schools, sizes)
enroll

#Now we know the number of students in each classroom, but we want to know the total number of students in the *school*
#in the same way that we know the number of ballots each candidate got in each district, but we want to know how many ballots in total were counted
# So we need to form *groups* of schools and then do something with them. Specifically add up the *sizes* in each group.

#always start with your data frame and pipe
enroll %>% 
  #Form the groups
group_by(schools) %>% 
  #what do we want to do?
  #summarize each group 
  summarize(n=sum(sizes))
#That seems to work
# But the only trick is.
# we want those numbers in a column appended to the original data frame so that we can divide 
# the plurality by the total number of votes cast.
# So we want to *add* these schools enrollments *back* into the original dataframe.

#This is a bit complicated, 
# But instead of *summarizing* the groups in the dataframe
# we want to *mutate* the original dataframe by adding a new column that contains the enrollments of each school
#
enroll %>% 
  #Still form our gorups
  group_by(schools) %>% 
  #Here instead of summarize, we mutate the dataframe enroll
  mutate(n=sum(sizes))
#That looks good

#Let's save that
enroll %>% 
  #Still form our gorups
  group_by(schools) %>% 
  #Here instead of summarize, we mutate the dataframe enroll
  mutate(n=sum(sizes))->enroll
#Check
enroll
