## read "/Users/yu-yaliang/Desktop/Data_Course_LASTNAME/Data/DatasaurusDozen.tsv"
## exam and make a good graph

library(tidyverse)
library(ggplot2)

my_dino = read.table("Data/DatasaurusDozen.tsv")
my_dino = my_dino[-1, ]
view(my_dino)
my_dino
str(my_dino)

my_dino = my_dino%>% 
  mutate(V2 = as.numeric(V2)) %>% 
  mutate(V3 = as.numeric(V3))
  
my_dino$V1 = gsub("_", " ", my_dino$V1)

str(my_dino)

my_dino %>% 
  #  group_by(V1) %>% 
  ggplot(aes(x = V2,
             y = V3,
             color = V1)) +
  geom_point() +
  facet_wrap(~ V1, ncol = 3) + # sets the number of columns
  scale_x_continuous(limit = c(-10, 110), expand = c(0, 0)) +
  scale_y_continuous(limit = c(-10, 110), expand = c(0, 0)) +
  labs(color = "Image",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank())


my_dino %>% 
  group_by(V1) %>% 
  summarise(mean_x = mean(V2),
            mean_y = mean(V3),
            sd_x = sd(V2),
            sd_y = sd(V3),
            max_x = max(V2),
            max_y = max(V3))


##GGally
install.packages("GGally")
library(GGally)
ggpairs(my_dino)
ggpairs(penguins)
dim(penguins)

## Animation
install.packages("gganimate")
install.packages("gapminder")
install.packages("gifski")
install.packages("av")
library(gganimate)
library(gapminder)
library(gifski)
library(av)
library(tidyverse)

view(gapminder)

my_map = gapminder
dim(my_map)
names(my_map)
unique(my_map$country)
str(my_map)
range(my_map$year)
unique(my_map$year)

my_map = gapminder
my_map %>% 
  group_by(country) %>% 
  ggplot(aes(x = year,
             y = population,
             color = country)) +
  geom_bar(stat = "identity", position = "dodge")

my_map = gapminder%>% 
  ggplot(aes(x = year,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent) +
#  transition_components(year)
  transition_time(year) +
  labs(title = "Year: {frame_time}")

animate(my_map, width = 1000, height = 1000)
anim_save("Data/Animations/my_animation.gif", animation = my_map) #saves specified animation with specified title
  




