#1. Install 'tidyverse' package####
#Install package
install.packages("tidyverse")
#2. load 'tidyverse' package in your environment
#loads package
library(tidyverse)
?tidyverse

## Using tidyverse ####
###1. get the car with cyl equal to 4####
df_mtcars = mtcars
four_cyl = df_mtcars[df_mtcars$cyl == 4, ]
four_cyl

###2. save both mpg > 20 and cyl equal to 4 into a new object####
df_mtcars = mtcars
df_mpg_cyl = df_mtcars[df_mtcars$mpg > 20 & df_mtcars$cyl == 4,]
df_mpg_cyl

mean(df_mpg_cyl)

#%>% shift + command + m
  
new_car = df_mtcars %>% 
  filter(mpg > 22) %>% 
  filter(cyl == 4) %>% 
  filter(wt < 3) %>% 
  filter(hp > 90)
new_car

write.csv(new_car, "Data/new_car.csv")


