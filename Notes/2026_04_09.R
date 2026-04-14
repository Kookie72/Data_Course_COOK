## using penguins dataset to build a model to predict whether a penguin is Gentoo or not
library(tidyverse)
my_penguins = penguins
my_penguins$species = as.factor(my_penguins$species)

mod1 = glm(species ~ island,
           data = my_penguins)
mod2 = glm(species ~ bill_length_mm,
           data = my_penguins)
mod3 = glm(species ~ bill_depth_mm,
           data = my_penguins)
mod4 = glm(species ~ bill_depth_mm + bill_length_mm,
           data = my_penguins)
mod5 = glm(species ~ flipper_length_mm,
           data = my_penguins)
mod6 = glm(species ~ flipper_length_mm + bill_depth_mm,
           data = my_penguins)
mod7 = glm(species ~ island,
           data = my_penguins)

compare_performance(mod1, mod3, mod5, mod7) %>% plot()


##########
mod_pen = penguins %>% 
  mutate(gentoo = case_when(species == "Gentoo" ~ T,
                            TRUE ~ F)) %>% 
  glm(formula = gentoo ~ bill_length_mm + body_mass_g,
      family = "binomial")

summary(mod_pen)
report(mod_pen)

dat_pen = penguins

#Determines probability of penguin being Gentoo
dat_pen$pred = predict(mod_pen, dat_pen, type = "response")

range(dat_pen$pred, na.rm = T)

dat_pen %>% 
  ggplot(aes(x = bill_length_mm,
             y = pred,
             color = species)) +
  geom_point()

dat_pen %>% 
  mutate(outcome = case_when(pred > 0.75 ~ "Gentoo",
                             pred < 0.25 ~ "Not Gentoo",
                             TRUE ~ "IDK")) %>% 
  dplyr::select(species, outcome) %>% 
  mutate(matches = case_when(species == "Gentoo" & outcome == "Gentoo" ~ TRUE,
                             species != "Gentoo" & outcome == "Not Gentoo",
                             TRUE ~ F)) %>% 
  pluck("matches") %>% 
  sum()/nrow(dat_pen)

table(evaluation$species)
table(evaluation$matches)

#use this data to predict grad school admission
dat_ad = read.csv("~/Desktop/Data_Course_COOK/Data/GradSchool_Admissions.csv")

view(dat_ad)
summary(dat_ad)

mod1 = glm(admit ~ gre,
           data = dat_ad)
mod2 = glm(admit ~ gpa,
           data = dat_ad)
mod3 = glm(admit ~ rank,
           data = dat_ad)
mod4 = glm(admit ~ gre + gpa + rank,
           data = dat_ad)
mod5 = glm(admit ~ gpa + gre * rank,
           data = dat_ad)

compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot

summary(mod4)
report(mod4)

mod_add = glm(data = dat_ad,
              formula = as.logical(admit) ~ (gre + gpa) * rank,
              family = "binomial")
summary(mod_add)

dat_ad$pred = predict(mod_add, dat_ad, type = "response")

dat_ad %>% 
  ggplot(aes(x = gpa,
             y = pred,
             color = factor(rank))) +
  geom_point() +
  geom_smooth()

mpg
install.packages("caret")
library(caret)
#createDataPartition()

id = createDataPartition(mpg$cty, p = 0.8, list = F)

dat_train = mpg[id, ]
dat_test = mpg[-id, ]

mod_mpg_train = glm(data = dat_train,
                    formula = cty ~ displ + cyl + displ:year + model)

dat_test$pred = predict(mod_mpg_train, dat_test)
view(dat_test)

dat_test %>% 
  mutate(error = abs(pred - cty)) %>% 
  pluck("error") %>% 
  summary()




