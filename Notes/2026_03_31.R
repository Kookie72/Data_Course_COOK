## build a model to predict cty as a function of displ
## mpg dataset

library(tidyverse)
library(ggplot2)
library(easystats)

my_mpg = mpg

mod = glm(data = my_mpg,
          formula = cty ~ displ)
summary(mod)
report(mod)
performance(mod)
predict(mod, my_mpg)
view(my_mpg)

mod$fitted.values[1]

plot(predict(mod,my_mpg), mod$fitted.values)

predict(mod, data.frame(displ = 1:100))

plot(predict(mod, data.frame(displ = 1:100)))
my_mpg$displ %>% range()

my_mpg$pred = predict(mod, my_mpg)
plot(my_mpg$cty, my_mpg$pred)

mod1 = glm(cty ~ displ,
          data = my_mpg)
mod2 = glm(cty ~ displ + cyl,
          data = my_mpg)
mod3 = glm(cty ~ displ + cyl + displ:cyl,
          data = my_mpg)
mod4 = glm(cty ~ displ*cyl*year,
           data = my_mpg)
mod5 = glm(cty ~ displ + cyl + year + displ:cyl + displ:year + cyl:year,
           data = my_mpg)

summary(mod5)
compare_models(mod1, mod2, mod3, mod4, mod5) %>% plot()
compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot()

my_mpg %>% 
  ggplot(aes(x = displ,
             y = cty,
             color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "glm")

## predict cty using 3 models and compare the results
## (probably just plot out and see each of the predictions)
mod1 = glm(cty ~ displ + cyl + class + trans + drv,
           data = my_mpg)
mod2 = glm(cty ~ displ + year + cyl + trans + class,
           data = my_mpg)
mod3 = glm(cty ~ displ + year + cyl + drv + class,
           data = my_mpg)
mod4 = glm(cty ~ class + cyl + displ,
           data = my_mpg)

compare_models(mod1, mod2, mod3) %>% plot()
compare_performance(mod1, mod2, mod3, mod4) %>% plot()

performance(mod3)

summary(mod3)

predict(mod3, my_mpg)

my_mpg$pred1 = predict(mod1, my_mpg)
my_mpg$pred2 = predict(mod2, my_mpg)
my_mpg$pred3 = predict(mod3, my_mpg)

my_mpg %>% 
  ggplot(aes(x = displ,
             y = pred3,
             color = cyl,
             group = cyl)) +
  geom_point() +
  geom_smooth(method = "glm")

my_mpg %>% 
  pivot_longer(starts_with("pred")) %>% 
  ggplot(aes(x = displ,
             y = cty,
             color = factor(cyl))) +
  geom_point() + 
  geom_point(aes(y = value), color = "black") +
  facet_wrap(~ name)















