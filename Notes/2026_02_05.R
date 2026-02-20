## plot how many penguins observed on each island and their species
## (bonus) how many of them are female, male, etc.

library(palmerpenguins)
library(tidyverse)
library(ggplot2)

# ggplot(aes(x = island,
#            y = species),
#        data = penguins)+ #data is the dataframe being used
#   geom_area()

penguins %>% 
  group_by(island, species, sex) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = island,
             y = count,
             fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ island) + #Separates data by selected category
  scale_fill_brewer(palette = 7)

  ?scale_fill_brewer  

penguins %>% 
  group_by(island, species, sex) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = island,
             y = count,
             fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(sex ~ island) #adds multiple graphs by selected categories

# logical operator
T/F
! # is not

vec = c(1,2,3,NA, 5, NA)
is.na(vec)
!is.na(vec)

#Get fat penguins
penguins %>% 
  filter(body_mass_g > 5000)

penguins$body_mass_g > 5000

penguins %>% 
  filter(!is.na(sex))

## are penguins with bigger flippers heavier?
## any differences between species and sex?
## make a plot to show that. (make sure no NA for sex)
penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(flipper_length_mm, body_mass_g, species, sex) %>%
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ sex) + #adds multiple graphs by selected categories
  scale_fill_brewer(palette = 4)

penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(flipper_length_mm, body_mass_g, species, sex) %>%
    summarise(count = n()) %>% 
    ggplot(aes(x = flipper_length_mm,
               y = body_mass_g,
               color = species,
               shape = sex)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(title = "Relationship between flipper and weight",
         x = "Flipper (mm)",
         y = "Weight (g)",
         color = "Breed") +
    scale_x_continuous(limit = c(170, 235), expand = c(0, 0)) + #Sets upper and lower limits for x
    scale_y_continuous(limits = c(2500, 6500), expand = c(0,0)) +
    stat_ellipse() + #adds ellipse to graph
    ggsave("Data/my_plot.jpg", width = 10, height = 15, dpi = 300) 

  penguins %>% 
    filter(!is.na(sex)) %>% 
    group_by(flipper_length_mm, body_mass_g, species, sex) %>%
    ggplot(aes(x = flipper_length_mm,
               y = body_mass_g,
               color = species,
               shape = sex)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(title = "Relationship between flipper and weight",
         x = "Flipper (mm)",
         y = "Weight (g)",
         color = "Breed") +
    scale_x_continuous(limit = c(170, 235), expand = c(0, 0)) + #Sets upper and lower limits for x
    scale_y_continuous(limits = c(2500, 6500), expand = c(0,0)) +
    stat_ellipse() +  #adds ellipse to graph
    scale_color_manual(values = c("Chinstrap" = "maroon", 
                                  "Gentoo" = "limegreen", 
                                  "Adelie" = "orange")) +
    scale_shape_manual(values = c(1, 12))
  





