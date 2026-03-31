##load required packages
library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(readxl)
library(measurements)

mpg
View(mpg)

## does displ effect cty?
## (hint: 1. plot out and examine; 2. statistical test)

my_mpg = mpg

my_mpg %>% 
  ggplot(aes(x = displ,
             y = cty,
             color = cyl)) + #y = dependent variable, x = independent variable
  geom_point() +
  geom_smooth() + #Adds line
  geom_smooth(method = glm) + #Adds trendline
  scale_x_continuous(limits = c(1,7.5), expand = c(0,0)) + #Sets min and max x values
  annotate("text", x = 5, y = 30,
           label = "cty = (-2.63) * displ + 25.99",
           size = 3)

cor.test(my_mpg$displ, my_mpg$cty, method = "spearm") #correlation test between two data
cor.test(my_mpg$displ, my_mpg$cty) #correlation test between two data
shapiro.test(my_mpg$cty)

# how much does displ effect cty
glm(cty ~ displ, data = my_mpg) #y=mx+b, cty=m*displ+b, coefficients: m=displ, b=(Intercept)
mod = glm(cty ~ displ, data = my_mpg) #y=mx+b, cty=m*displ+b, coefficients: m=displ, b=(Intercept)
str(mod)
summary(mod)

mod$coefficients
mod$fitted.values
mod$residuals #The difference between reality and what is expected

install.packages("easystats")
library(easystats)

performance(mod) #includes r2. The closer to 1 it is, the better the fit
report(mod) #creates a report for the model

corrr = cor.test(my_mpg$displ, my_mpg$cty)
report(corrr)

check_model(mod)

# build a better model for "cty"
# prove it's a better model
mod = glm(cty ~ displ, data = my_mpg)
mod2 = glm(cty ~ displ + cyl, data = my_mpg)
mod3 = glm(cty ~ displ + manufacturer, data = my_mpg)
mod4 = glm(cty ~ displ + manufacturer + model, data = my_mpg)
mod5 = glm(cty ~ displ + manufacturer + model + year, data = my_mpg)
mod6 = glm(cty ~ displ + manufacturer + model + year + cyl, data = my_mpg)
mod_max = glm(cty ~ displ + manufacturer + model + year + cyl + trans + drv + fl + class, data = my_mpg)

performance(mod)
performance(mod2)
performance(mod3)
performance(mod4)
performance(mod_max)

check_model(mod2)

compare_performance(mod, mod2) %>% plot()
compare_performance(mod, mod2, mod3, mod4, mod5, mod6, mod_max) %>% plot()

summary(mod3)



