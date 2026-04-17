#Script####
##1. loads the “/Data/mushroom_growth.csv” data set####
my_mushrooms = read.csv("../../Data/mushroom_growth.csv")

##2. creates several plots exploring relationships between the response and predictors####
view(my_mushrooms)
str(my_mushrooms)

ggplot(my_mushrooms, aes(x = Light, y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()
ggplot(my_mushrooms, aes(x = Nitrogen, y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()
ggplot(my_mushrooms, aes(x = Humidity, y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()
ggplot(my_mushrooms, aes(x = Temperature, y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()
ggplot(my_mushrooms, aes(x = Species, y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ## `geom_smooth()` using formula 'y ~ x'
  theme_minimal()

##3. defines at least 4 models that explain the dependent variable “GrowthRate”####
mod1 = lm(GrowthRate ~ Light, data = my_mushrooms)
mod2 = lm(GrowthRate ~ Nitrogen, data = my_mushrooms)
mod3 = lm(GrowthRate ~ Humidity, data = my_mushrooms)
mod4 = lm(GrowthRate ~ Temperature, data = my_mushrooms)
mod5 = lm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature, data = my_mushrooms)
mod6 = lm(GrowthRate ~ Light*Nitrogen + Humidity*Temperature, data = my_mushrooms)

##4. calculates the mean sq. error of each model####
compare_models(mod1, mod2, mod3, mod4, mod5, mod6) %>%  plot()
compare_performance(mod1, mod2, mod3, mod4, mod5, mod6) %>% plot() # mod6 looks like the best fit

# mean(mod1$residuals^2)
# mean(mod2$residuals^2)
# mean(mod3$residuals^2)
# mean(mod4$residuals^2)
# mean(mod5$residuals^2)
# mean(mod6$residuals^2)

##5. selects the best model you tried####
#mod6 is the best fit

##6. adds predictions based on new hypothetical values for the independent variables used in your model####
# Create the hypothetical grid
hypothetical_data = my_mushrooms %>%
  data_grid(
    Light = seq(0, 20, by = 5),        
    Nitrogen = seq(0, 45, by = 5),     
    Humidity = c("Low", "High"),       
    Temperature = 20                   
  ) %>%
  # Add the predictions from mod6
  add_predictions(mod6, var = "pred")

##7. plots these predictions alongside the real data####
ggplot() +
  # Plots the REAL data
  # geom_line(data = my_mushrooms,
  #           aes(x = Light, y = GrowthRate, color = as.factor(Nitrogen)),
  #           size = 1) +
  geom_point(data = my_mushrooms,
             aes(x = Light, y = GrowthRate, color = as.factor(Nitrogen))) +
  # Plots the HYPOTHETICAL predictions as lines
  geom_line(data = hypothetical_data, 
            aes(x = Light, y = pred, color = as.factor(Nitrogen)), 
            size = 1) +
  # Separate by Humidity
  facet_wrap(~Humidity, scale = "free") +
  theme_minimal() +
  labs(x = "Light Level",
       y = "Growth Rate",
       color = "Nitrogen")

# Responses####
##1. Are any of your predicted response values from your best model scientifically meaningless? Explain.####
# No. Some of my predicted responses have a growth rate near zero, but none of the have a negative growth rate.
# And none of them have a growth rate that is higher than what was observed in real life.

##2. In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.####
# Yes. The real life data had growth rate that was not linear. Here is a resource that shows how to deal with this: https://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/

##3. Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)####
lin_relation = read.csv("../../Data/non_linear_relationship.csv")
view(lin_relation)
str(lin_relation)

lin_mod = lm(predictor ~ response, data = lin_relation) %>% plot()

