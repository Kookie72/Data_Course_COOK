library(tidyverse)
table1
table2

## 1 observation each row
## col = 1 variable

## make table2 clean/tidy/good to make a plot
clean_table2 = table2 %>% 
  pivot_wider(names_from = "type", values_from = "count") #changes data in column to be new column headers and removes original column

clean_table2

clean_table3 = table3 %>% 
  separate(rate, "/", into = c("cases", "population"), #separates data from one column to two new columns
           convert = T) %>% #changes data to integer
  mutate(rate = cases/population) %>% #creates column
  select(-rate) #removes column


clean_table3

table4a #cases
table4b #population

clean_table4a = table4a %>% 
  pivot_longer(-country, names_to = "year", values_to = "cases")

clean_table4a

clean_table4b = table4b %>% 
  pivot_longer(-country, names_to = "year", values_to = "population")

clean_table4b

full_join(clean_table4a, clean_table4b) #merges tables, can only merge 2 at a time

full_join(clean_table4a, clean_table4b, by = "country")

table5

clean_table5 = table5 %>% 
  separate(rate, "/", into = c("cases", "population"),
           convert = T) %>% 
  mutate(rate = cases/population,
         year = paste0(century, year)) %>%  #paste0 combines columns without adding a space
  select(-century)

clean_table5

## read "messy_bp.xlsx
install.packages("readxl")
library(readxl)

messy = read_excel("Data/messy_bp.xlsx", skip = 3)

names(messy)
messy %>% 
  select(-c("HR...9", "HR...11", "HR...13"))

## create dataframes for visit 1, visit 2, visit 3, then merge to original dataframe

view(messy)
messy


