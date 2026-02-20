#finds the largest number in the dataframe, ignoring anything that isn't numeric
df_iris = iris
max(unlist(df_even[sapply(df_even, is.numeric)]), na.rm = TRUE) 

## using penguin data####
library(palmerpenguins)
library(tidyverse)
## add a new col (fatstat)
## for penguins weight more than 5000g -->
## for penguins weight less than or equal to 5000g and more than 3000g -->
## for penguins weight less than or equal to 3000g -->
penguin_weights = penguins %>% 
  mutate(fatstat = case_when(body_mass_g > 5000 ~ "TOO FAT!!!",
                             body_mass_g <= 5000 & body_mass_g > 3000 ~ "Normal",
                             body_mass_g <= 3000 ~ "TOO SKINNY!!!"))
penguin_weights
view(penguin_weights)

##Charts####
plot()
hist()
barplot()
boxplot()

names(penguin_weights)
plot(penguin_weights$bill_length_mm, penguin_weights$body_mass_g)

#install ggplot2 package
library(ggplot2)
ggplot()
#aes = aesthetic
ggplot(aes(x = bill_length_mm,
           y = body_mass_g),
       data = penguin_weights)+
  geom_path()

penguin_weights %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g,
             colour = sex,
             shape = species)) +
  geom_jitter() +
  theme_light()

penguin_weights %>% 
  ggplot(aes(x = bill_length_mm)) +
  geom_bar(stat = "count")

penguin_weights %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g)) +
  geom_bar(stat = "identity") #identity defaults to y value

max(penguin_weights$body_mass_g, na.rm = T)

## calculate the total weight of Gentoo penguin
view(penguin_weights)
penguin_weights %>%
  ggplot(aes(x = species,
             y = body_mass_g))+
  geom_bar(stat = "identity", position = "dodge") #stat stacks data, dodge does not

penguin_weights %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g)) +
 geom_col()

gentoo_weights = penguin_weights %>% 
  filter(species == "Gentoo") %>% 
  summarise(total_weight_g = sum(body_mass_g, na.rm = TRUE))

# stack####
penguin_weights %>% 
  ggplot(aes(x = species,
             y = body_mass_g,
             colour = sex)) +
  geom_bar(stat = "identity", position = "stack")
#dodge####
penguin_weights %>% 
  ggplot(aes(x = species,
             y = body_mass_g,
             colour = sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)


## plot average body mass of penguins by sex and species####
penguin_weights %>% 
  ggplot(aes(x = species,
             y = cummean(body_mass_g),
             colour = sex)) +
  geom_bar(stat = "identity", position = "dodge")

penguin_weights %>% 
  group_by(species, sex) %>% 
  summarise(avg_mass_g = mean(body_mass_g, na.rm = TRUE)) %>% 
  ggplot(aes(x = species,
             y = avg_mass_g,
             fill = sex)) +
  geom_bar(stat = "identity", position = "dodge")


