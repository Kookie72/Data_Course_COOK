## install palmerpenguins package####
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
## open 'penguins' dataset####
view(penguins)
## save penguins with bill length > 40 to a new object####
new_penguins = penguins %>% 
  filter(bill_length_mm > 40)
view(new_penguins)
## calculate the average of body mass of them#### 
mean(new_penguins$body_mass_g, na.rm = T)
?tidyverse
?mean
new_penguins$body_mass_g %>% 
  mean()

new_penguins = penguins
new_penguins %>% 
  filter(bill_length_mm > 40) %>% 
  select(body_mass_g) %>% 
  mean()

mean(penguins$body_mass_g, na.rm = T)

new_penguins %>% 
  filter(bill_length_mm > 40) %>% 
  pluck("body_mass_g") %>% 
  mean()

## calculate body mass g by sex####
view(new_penguins)
new_penguins %>% 
  filter(sex == "male") %>%
  filter(bill_length_mm > 40) %>% 
  pluck("body_mass_g") %>% 
  mean() %>% 
new_penguins %>% 
  filter(sex == "female") %>% 
  filter(bill_length_mm > 40)
  pluck("body_mass_g") %>% 
  mean()

new_penguins %>% 
  filter(bill_length_mm > 40) %>% 
  group_by(sex, island) %>% 
  summarize(avg_body_mass = mean(body_mass_g),
            no_of_penguins = n(),
            max_mass = max(body_mass_g),
            min_mass = min(body_mass_g)) %>% 
  arrange(desc(max_mass)) %>% #sorts by specified column desc forces largest to smallest
  write_csv("Data/new_penguins.csv")
## Deletes file ####
file.remove("Data/new_penguins")

## find the fattie penguins (body mass > 5000)
## count how many male and females
## return the max body mass of male and female
fat_penguins = penguins
fat_penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>%
  summarize(number = n(),
            max_mass = max(body_mass_g))

## (bonus??) add new column to data to tell if they are fat or not
fat_penguins %>% 
  mutate(fat_status = case_when(body_mass_g > 5000 ~ "FAAAT",
                                body_mass_g < 5000 & body_mass_g > 3000 ~ "Average",
                                body_mass_g < 3000 ~ "Skinny",
                                T ~ "Unknown")) %>%
  view()

## mutate ####
new_penguins %>% 
  mutate(len_times_dpt = bill_length_mm * bill_depth_mm) %>% 
  view()

## combine mutate() and case_when()
penguins %>% 
  mutate(fat_status = case_when(body_mass_g > 5000 ~ "FAAAT",
                                body_mass_g < 5000 & body_mass_g > 3000 ~ "Average",
                                body_mass_g < 3000 ~ "Skinny")) %>% 
  view()

penguins %>% 
  mutate(fat_status = case_when(body_mass_g > 5000 ~ "FAAAT",
                                TRUE ~ "Super Healthy")) %>% 
  view()

## add new column to data to highlight penguins with big bills####








