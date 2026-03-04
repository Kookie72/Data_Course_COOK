#Loads required packages
library(tidyverse)
library(ggplot2)
library(gganimate)

#Imports data from .csv file
sharks = read.csv("Data/attacks.csv")
view(sharks)
str(sharks)

#Removes extra rows
my_sharks = sharks %>% 
  filter(!is.na(Year)) %>%  #Removes empty rows from dataset
  filter(Country != "") %>%  #Removes rows without a country listed
  select(-Type, -Time, -Name, -Sex, -Age, -Fatal..Y.N., -Species, -Injury, -Activity, -Investigator.or.Source, -pdf, -href, -href.formula, -Case.Number, -Case.Number.1, -Case.Number.2, -original.order, -X, -X.1) #Removes selected columns
  
write.csv(my_sharks, "Data/my_sharks.csv") #Saves cleaned up dataset

View(my_sharks)
str(my_sharks)

attacks_per_year = my_sharks 
attacks_per_year%>% 
  group_by(Year, Country) %>% 
  summarise(attacks = n(), .groups = "drop") %>%  #.groups = "drop" removes the grouping after summarising so as not to interfere with additional math that may need to be done on data
  ggplot(aes(x = Year,
             y = attacks,
             color = Country)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

head(attacks_per_year)
view(attacks_per_year)

  ggplot(aes(x = Date,
             color = Country)) +
  facet_wrap(~ country) +
  theme_minimal() +
  geom_text(aes(label = country, vjust = -0.5, hjust = 0.1)) + #Labels the dots
  transition_time(year)

