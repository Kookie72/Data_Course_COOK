#Load libraries
library(tidyverse)
library(ggplot2)
library(gganimate)

dat = read.csv("../../Data/BioLog_Plate_Data.csv")

view(dat)

clean_dat = dat %>% 
  mutate(Type = case_when(Sample.ID %in% c("Soil_1", "Soil_2") ~ "Soil",
                                 Sample.ID %in% c("Clear_Creek", "Waste_Water") ~ "Water")) %>% #Creates a column that specifies if sample is from soil or water
  pivot_longer(cols = starts_with("Hr"),
               names_to = "Time",
               values_to = "Hr") %>%  #Convert 3 time colummns to one column
  mutate(Time = parse_number(Time)) %>% #Removes non-numeric characters and converts column to number
#  mutate(Time = str_sub(Time, 4)) %>%  #Removes left 4 characters in entire column
  rename(Absorbance = Hr) 

view(clean_dat)
str(clean_dat)

clean_dat %>% 
  filter(Dilution == 0.1) %>% 
  group_by(Time, Type, Substrate) %>% 
  summarise(mean_value = mean(Absorbance, na.rm = TRUE), #Averages the Absorbance values for soil and water at each time interval
            .groups = "drop") %>% 
  ggplot(aes(x = Time,
             y = mean_value,
             color = Type)) +
  labs(subtitle = "Just dilution 0.1", #Adds note to top of graph
       y = "Absorbance") + 
  geom_line() +
  facet_wrap(~ Substrate) +
  theme_minimal() +

clean_dat %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Time, Sample.ID, Substrate, Dilution) %>% 
  summarise(mean_value = mean(Absorbance, na.rm = TRUE), #Averages the Absorbance values for soil and water at each time interval
            .groups = "drop") %>% 
  ggplot(aes(x = Time,
             y = mean_value,
             color = Sample.ID)) +
  labs(y = "Absorbance",
       title = "Itaconic Acid",
       color = "Sample ID") + 
  geom_line() +
  facet_grid(Substrate ~ Dilution, scales = "free_x") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.y = element_blank()) +
  transition_reveal(Time)






