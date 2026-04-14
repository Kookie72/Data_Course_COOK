library(tidyverse)
library(easystats)

messy = read.csv("Utah_Religions_by_County.csv")

view(messy)

# This creates a new, clean dataset, without modifying the original data
clean_Religions = messy %>% 
  pivot_longer(cols = Assemblies.of.God:Orthodox,  # Selects columns to combine
               names_to = "Religion",  # Names column of former column headings
               values_to = "Worshipers") # Names column of values from previous columns

view(clean_Religions)
str(clean_Religions)

#Explore the cleaned data set with a series of figures (I want to see you exploring the data set)
clean_Religions$Religion = as.factor(clean_Religions$Religion)
  
mod1 = glm(data = clean_Religions, 
           formula = Worshipers ~ Pop_2010)
mod2 = glm(data = clean_Religions, 
           formula = Worshipers ~ Religion)
mod3 = glm(data = clean_Religions, 
           formula = Worshipers ~ Pop_2010 + Religion)
mod4 = glm(data = clean_Religions, 
           formula = Worshipers ~ County)
mod5 = glm(data = clean_Religions, 
           formula = Worshipers ~ County + Religion)

compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot() #Finds the model with the best R2 and AIC
summary(mod2) 
report(mod2)

#Address the questions:
  
#  “Does population of a county correlate with the proportion of any specific religious group in that county?”
# No. People tend to join specific religions more than others, regardless of the population of the area.
# There isn't a linear relation. Increasing the population increases the number of people in a religion,
# but there isn't a set percentage of a population that joins specific religions.

#  “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
#Just stick to figures and maybe correlation indices…no need for statistical tests yet
mod6 = glm(data = clean_Religions, 
           formula = Non.Religious ~ Religion)

summary(mod6)
report(mod6)

#No, the proportion of non-religious people does not correlate with any specific religion.
# The proportion of non-religious people is always the same, regardless of what religion is 
#predominant in the county.





