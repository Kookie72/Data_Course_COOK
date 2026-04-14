## build your BEST model to predict cty
library(tidyverse)
library(ggplot2)
library(easystats)
library(MASS)
library(palmerpenguins)

my_mpg = mpg

View(mpg)

mod1 = glm(data = my_mpg,
          formula = cty ~ displ) # 0.638
mod2 = glm(data = my_mpg,
          formula = cty ~ trans) # 0.176
mod3 = glm(data = my_mpg,
          formula = cty ~ drv) # 0.445
mod4 = glm(data = my_mpg,
          formula = cty ~ year) # 0.001
mod5 = glm(data = my_mpg,
          formula = cty ~ model) # 0.802
mod6 = glm(data = my_mpg,
          formula = cty ~ manufacturer) # 0.556

performance(mod5) # Gives R2 value
summary(mod5)
report(mod5)
predict(mod5, my_mpg)

plot(predict(mod5,my_mpg), mod$fitted.values)  

my_mpg %>% 
  ggplot(aes(x = model,
             y = cty,
             color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "glm")

step_mod5 = stepAIC(mod5)
step_mod5$formula # finds best model, based on variables in selected object

mod_maxx = glm(data = my_mpg,
               formula = cty ~ .) # . selects everything
mod_maxx = glm(data = my_mpg,
               formula = cty ~ .^2) # . selects everything .^2 

step_maxx = stepAIC(mod_maxx)

## does penguin mass vary between species?
my_penguins = penguins

view(my_penguins)
str(my_penguins)

mod_penguins = glm(data = my_penguins,
                   formula = body_mass_g ~ species)

summary(mod_penguins)

my_penguins$species = relevel(my_penguins$species,
                              ref = "Gentoo")
mod_pen = glm(data = my_penguins)

## Wheter this is a Gentoo or not
view(my_penguins)

dat_pen2 = my_penguins %>% 
  mutate(gentoo = case_when(species == "Gentoo" ~ TRUE,
                            TRUE ~ FALSE)) %>% # If it isn't True, mark it as False

glm(data = dat_pen2,
    formula = gentoo ~ bill_length_mm + bill_depth_mm,
    family = "Binomial")


