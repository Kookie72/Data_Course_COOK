library(readxl)
library(tidyverse)
library(ggplot2)

messy = read.csv("Data/Bird_Measurements.csv")

view(messy)

messy %>% 
  ggplot(aes(x = English_name,
             y = Egg_mass,
             colour = English_name)) +
  geom_point() +
  #geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, size = 5), 
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "green"),
        panel.background = element_rect(fill = "limegreen")) +
  labs(x = "English Name", y = "Egg Mass", title = "Bird Egg Comparison")

ggsave("Assignments/Assignment_5/Ugly_Plot.png", width = 12, height = 6, dpi = 300)
  