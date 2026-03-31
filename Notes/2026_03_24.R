## download "height.xlsx" from Teams channel
## make it tidy, make a plot, and explain it

install.packages("skimr")
install.packages("measurements")

##Required packages
library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(readxl)
library(measurements)

messy = read_excel("Data/height.xlsx")

heights = messy %>% 
  pivot_longer(everything(),
               names_to = "Gender") %>% 
  rename(Height = value) %>% 
  separate(Height, into = c("Feet", "Inches"), convert = T) %>% 
  mutate(Total_inches = Feet*12 + Inches)
#  mutate(cm = Total_inches * 2.54) 
#  mutate(cm_convert = conv_unit(Total_inches, from = "in", to = "cm"))

heights %>% 
  ggplot(aes(x = Total_inches,
             fill = Gender)) +
  geom_density(alpha = 0.5)


view(heights)

t.test(Total_inches ~ Gender, data = heights)












