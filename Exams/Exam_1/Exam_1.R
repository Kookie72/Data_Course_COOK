# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)####
covid = read.csv("cleaned_covid_data.csv")
str(covid)

library(tidyverse)

my_covid = covid %>% #changes columns to the correct data type
  mutate(Last_Update = as.Date(Last_Update),
         Confirmed = as.numeric(Confirmed),
         Deaths = as.numeric(Deaths),
         Recovered = as.numeric(Recovered),
         Active = as.numeric(Active))

str(my_covid)
view(my_covid)

# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)####
# Use the tidyverse suite of packages
# Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)
A_states = my_covid %>% 
  filter(grepl("^A", Province_State)) #removes all states that don't start with A

view(A_states)
 
# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)####
# Create a scatterplot
# Add loess curves WITHOUT standard error shading
# Keep scales “free” in each facet
library(ggplot2)  

A_states %>% 
  group_by(Province_State) %>% 
  ggplot(aes(x= Last_Update,
             y = Deaths,
             color = Province_State)) +
  geom_point() +
  facet_wrap(~Province_State, scales = "free") + #scales = "free" allows each graph to be a different scale
  geom_smooth(method = "loess", se = FALSE) #method add loess curve and se = FALSE removes standard error shading

# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)####
# I’m looking for a new data frame with 2 columns:
#   “Province_State”
# “Maximum_Fatality_Ratio”
# Arrange the new data frame in descending order by Maximum_Fatality_Ratio
# This might take a few steps. Be careful about how you deal with missing values!
state_max_fatality_rate = my_covid %>% 
  filter(!is.na(Case_Fatality_Ratio)) %>% #removes rows that contain NA in the Case_Fatality_Ratio column
  group_by(Province_State) %>% #Organizes data by state
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE), #returns the max value from the Case_Fatality_Ratio column
            .groups = "drop") %>%  #Removes grouping
  arrange(desc(Maximum_Fatality_Ratio)) #arrange auto sorts specified column in ascending order, desc changes it to descending order

view(state_max_fatality_rate)

# V. Use that new data frame from task IV to create another plot. (20 pts)####
# X-axis is Province_State
# Y-axis is Maximum_Fatality_Ratio
# bar plot
# x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
# X-axis labels turned to 90 deg to be readable
# Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.
state_max_fatality_rate %>% 
  ggplot(aes(x = factor(Province_State, levels = rev(sort(Province_State))), #Reverse alphabetizes the states on the x-axis
             y = Maximum_Fatality_Ratio,
             fill = Province_State)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State",
       y = "Maximum Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 90))

# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time####
# You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
my_covid %>% 
  group_by(Last_Update) %>% 
  summarise(US_cumulative_deaths = sum(Deaths, na.rm = TRUE)) %>% 
  ggplot(aes(x = Last_Update,
             y = US_cumulative_deaths)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Deaths in US")
