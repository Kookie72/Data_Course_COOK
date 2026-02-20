## make a cool plot using penguijn data (make sure no NA)
## manually set color as ugly/annoying as possible
## add title, fix axis labels
## save to local directory

library(palmerpenguins)
library(tidyverse)
library(ggplot2)

penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(flipper_length_mm, body_mass_g, species, sex) %>%
  ggplot(aes(x = flipper_length_mm, ## sets the data used for the axis
             y = body_mass_g,
             color = species,
             shape = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Relationship between flipper and weight", ## adds titles to the graph
       x = "Flipper (mm)",
       y = "Weight (g)",
       color = "Breed") +
  scale_x_continuous(limit = c(170, 235), expand = c(0, 0)) + #Sets upper and lower limits for x
  scale_y_continuous(limits = c(2500, 6500), expand = c(0,0)) +
  scale_color_manual(values = c("Chinstrap" = "black", ## sets colors manually
                                "Adelie" = "grey", 
                                "Gentoo" = "white")) +
  scale_shape_manual(values = c(1, 12)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 20))+
  theme(axis.text = element_text(angle = 45, size = 16)) 
ggsave("Data/my_plot_2026_02_10.jpg", width = 10, height = 15, dpi = 300)

## element_text(angle = , face = , size = )
## hjust = 

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g)) +
  geom_point(aes(color = species)) + ## local color setting
  geom_smooth() ## draws line

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             colour = species)) + ## global color setting
  geom_point(alpha = 0.5, size = 10) +
  geom_smooth()

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             colour = species)) + ## global color setting
  geom_point(color = "black") +
  geom_smooth()

setwd("/Users/Jonia/Desktop/Data_Course_COOK")
read.csv("Data/wide_income_rent.csv")

setwd("/Users/Jonia/Desktop/Data_Course_COOK/Data") ## file path won't work if someone tries running script from a different computer
read.csv("wide_income_rent.csv")

## make a plot to show penguin weight change across 3 years
view(penguins)
str(penguins)
names(penguins)

view(penguins)

penguins %>% 
  ggplot(aes(x = as.factor(year), ## as.factor() will only show years with data, won't add half years
             y = body_mass_g,
             fill = species)) + ## fill colors entire bar, color only outlines bar
  geom_bar(stat = "identity", position = "dodge")

penguins %>% 
  ggplot(aes(x = as.factor(year),
             y = body_mass_g,
             fill = species)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.5) ## geom_jitter shows individual dots, alpha determines transparency


