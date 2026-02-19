install.packages("ggmap")
install.packages("leaflet")
install.packages("patchwork")
library(ggmap)

# Plot the map and add simple dots
ggmap(world.map) +
  geom_point(data = )


library(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = -122.4149, lat = 37.7749, #Places pin at specified map location
             popup = "Hello!!!") #Adds tag to pin


penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  summarise(avg_mass = mean(body_mass_g),
            sd_mass = sd(body_mass_g)) %>% 
  ggplot(aes(x = species,
             y = avg_mass,
             fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = avg_mass - sd_mass, ymax = avg_mass + sd_mass),
                position = position_dodge2(width = 0.5, padding = 0.5))

new_peng = penguins %>% 
  mutate(mass_100 = body_mass_g + 100) ## adds column

view(new_peng)

new_peng %>% 
  select(- mass_100) ## removes column

new_peng[, -9] ## removes 9th column
new_peng[, -length(new_peng)] ## removes last column
new_peng[, ncol(new_peng)] ## removes last column

penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             colour = sex)) +
  labs(x = "Bill depth (mm)",
       y = "Body mass (g)",
       color = "Sex") +
  geom_point() +
  facet_wrap(~ species) +
  scale_color_manual(values = c("female" = "darkmagenta",
                                "male" = "green")) +
  geom_point(alpha = 0.7, size = 4) + ##alpha sets transparency and size sets size of each point
  theme_bw() +
  theme(strip.background = element_blank(), ## strip.background removes the background
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold", size = 12)) 

?theme

library(patchwork)
p1
p2
p3

p1 + p2
p1 + p2 + p3
p1/p2
p1/p2/p3

(p1+p2)/p3 +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

## read "/Users/yu-yaliang/Desktop/Data_Course_LASTNAME/Data/DatasaurusDozen.tsv"
## exam and make a good graph
my_dino = read.table("Data/DatasaurusDozen.tsv")
my_dino = my_dino[-1, ]
view(my_dino)
my_dino
str(my_dino)

my_dino = my_dino%>% 
  mutate(V2 = as.numeric(V2)) %>% 
  mutate(V3 = as.numeric(V3))

str(my_dino)

my_dino %>% 
#  group_by(V1) %>% 
  ggplot(aes(x = V2,
             y = V3,
             color = V1)) +
  geom_point() +
  facet_wrap(~ V1, ncol = 3) + 
  scale_x_continuous(limit = c(10, 100), expand = c(0, 0)) +
  scale_y_continuous(limit = c(-10, 110), expand = c(0, 0)) +
  labs(color = "Image")
