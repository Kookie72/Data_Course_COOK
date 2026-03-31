library(readxl)
library(tidyverse)

path = "Data/messy_bp.xlsx"
dat = read_excel(path, skip = 3)
View(dat)

names(dat)
dat %>% 
  select(-c(HR...9, HR...11, HR...13)) %>% 
  View()
