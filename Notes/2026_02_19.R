## Create a plot showing how GDP and life expectancy have changed across different countries over the years
## label country
## (bonus) label country of interest

library(gganimate)
library(gapminder)
library(gifski)
library(av)
library(tidyverse)

view(gapminder)

my_map = gapminder

my_map %>% 
  group_by(country) %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = gdpPercap)) +
  facet_wrap(~ continent) +
  labs(title = "GDP Over the Years",
       x = "GDP",
       y = "Life Expectancy") +
  theme_minimal() +
  geom_text(aes(label = country, vjust = -0.5, hjust = 0.1)) + #Labels the dots
  transition_time(year) 

## YuYa's graph
cool_country = c("New Zealand", "Kuwait", "Cambodia", "Rwanda", "United Kingdom", "China", "Norway", "Sri Lanka")

gapminder %>% 
  mutate(cool_country = case_when(country %in% cool_country ~ country))
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point() +
  geom_text(aes(label = country, vjust = -0.5, hjust = 0.1)) +
  theme(legend.position = "none") + #removes legend
  transition_time(year) #transition_component also works if transition_time doesn't
  
# read in "wide_income_rent.csv" and make a plot to show rent in each state
my_rent = read.csv("Data/wide_income_rent.csv")
my_rent = t(my_rent)  
my_rent = as.data.frame(my_rent)  
my_rent = my_rent[-1,]
colnames(my_rent) = c("income", "rent")
my_rent$state = rownames(my_rent)

my_rent %>% 
  ggplot(aes(x = state,
             y = rent)) +
  geom_bar(stat = "identity")

my_rent = read.csv("Data/wide_income_rent.csv")
my_rent %>% 
  pivot_longer(-variable, names_to = "state", values_to = "USD") %>% 
  view()

dat_test = data.frame(
  ID = c(22, 33, 45, 60),
  H = c(145, 155, 160, 132),
  W = c(32, 22, 134, 50)
)

dat_test %>% 
  pivot_longer(cols = c(H, W), names_to = "measure", values_to = "value")

dat_long = dat_test %>% 
  pivot_longer(-ID, names_to = "measure", values_to = "value")

dat_long %>% 
  pivot_wider(names_from = "measure", values_from = "value")

## using pivot to clean "wide_income_rent.csv" and plot rent for each state
my_rent = read.csv("Data/wide_income_rent.csv")
view (my_rent)

new_rent = my_rent %>% 
  pivot_longer(-variable, names_to = "state", values_to = "USD") %>% 
  pivot_wider(names_from = "variable", values_from = USD) %>% 
  ggplot(aes(x = state,
             y = rent)) +
  geom_bar(stat = "identity")


?pivot_longer










