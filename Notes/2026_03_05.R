install.packages("janitor")
library(janitor)
library(tidyverse)
library(readxl)

messy = read_excel("Data/messy_bp.xlsx", skip = 3)

names(messy)
messy1 = messy %>% 
  select(pat_id, BP...8, HR...9) %>% 
  mutate(Visit = 1) %>% 
  rename(Blood_Pressure = BP...8, Heart_Rate = HR...9) %>% 
  separate(Blood_Pressure, "/", into = c("Systolic", "Diastolic"))

messy2 = messy %>% 
  select(pat_id, BP...10, HR...11) %>% 
  mutate(Visit = 2) %>% 
  rename(Blood_Pressure = BP...10, Heart_Rate = HR...11) %>% 
  separate(Blood_Pressure, "/", into = c("Systolic", "Diastolic"))

messy3 = messy %>% 
  select(pat_id, BP...12, HR...13) %>% 
  mutate(Visit = 3) %>% 
  rename(Blood_Pressure = BP...12, Heart_Rate = HR...13) %>% 
  separate(Blood_Pressure, "/", into = c("Systolic", "Diastolic"))

view(messy)
messy1

clean = messy %>% 
  select(-BP...8, -HR...9, -BP...10, -HR...11, -BP...12, -HR...13)

clean1 = clean %>% 
  left_join(messy1, by = "pat_id")

clean2 = messy2 %>% 
  left_join(clean1, by = "pat_id")

messy2
view(clean2)

messy1 = messy %>% 
  select(-starts_with("HR")) %>% 
  pivot_longer(cols = starts_with("BP"),
               names_to = "Visit",
               values_to = "BP") %>% 
  separate(BP, into = c("Systolic", "Diastolic")) %>% 
  mutate(Visit = case_when(Visit == "BP...8" ~1,
                           Visit == "BP...10" ~2,
                           Visit == "BP...12" ~3)) %>% 
  view()

messy2 = messy %>% 
  select(-starts_with("BP")) %>% 
  pivot_longer(cols = starts_with("HR"),
               names_to = "Visit",
               values_to = "HR") %>%
  mutate(Visit = case_when(Visit == "HR...9" ~1,
                           Visit == "HR...11" ~2,
                           Visit == "HR...13" ~3)) %>% 
  view()

clean_data %>% 
  mutate(DOB = paste("Month of Birth", "Month of Birth", "Month of Birth", sep = "-"))

clean_data %>% 
  clean_names() %>% 
  names()

