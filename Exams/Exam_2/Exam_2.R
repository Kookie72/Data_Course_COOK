library(tidyverse)
library(ggplot2)
library(easystats)

##1. Read in the unicef data ####
messy = read.csv("unicef-u5mr.csv")

view(messy)

##2. Get it into tidy format####
clean_Mortality_Rate = messy %>% 
  pivot_longer(cols = U5MR.1950:U5MR.2015, # Selects columns U5MR.1950 to U5MR.2015
               names_to = "Year",
               values_to = "U5MR") %>% 
  mutate(Year = parse_number(str_remove(Year, "U5MR\\."))) #Removes U5MR and converts column to number

view(clean_Mortality_Rate)
str(clean_Mortality_Rate)

##3. Plot each country’s U5MR over time####
# Creates graphs that compares countries in each continent
clean_Mortality_Rate %>% 
  filter(!is.na(U5MR)) %>%  # Ignores data if it is N/A in the U5MR column
  ggplot(aes(x = Year,
             y = U5MR,
             group = CountryName,
             colour = CountryName)) +
  theme(legend.position = "none") +
  geom_line() +
  facet_wrap(~ Continent)

##4. Save this plot as LASTNAME_Plot_1.png####
ggsave("COOK_Plot_1.png")
  
# Creates a graph the compares continents
U5MR_continent = clean_Mortality_Rate %>% 
  group_by(Continent, Year) %>% 
  mutate(Mean_U5MR = mean(U5MR, na.rm = TRUE))

view(U5MR_continent)
str(U5MR_continent)

##5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year####
U5MR_continent %>% 
  filter(!is.na(U5MR)) %>%  # Ignores data if it is N/A in the U5MR column
  ggplot(aes(x = Year,
             y = Mean_U5MR,
             group = Continent,
             colour = Continent)) +
  geom_line() 

##6. Save that plot as LASTNAME_Plot_2.png####
ggsave("COOK_Plot_2.png")

##7. Create three models of U5MR####
# Models for this data
mod1 = glm(U5MR ~ Year, 
           data = U5MR_continent)

mod2 = glm(U5MR ~ Year + Continent, 
           data = U5MR_continent)

mod3 = glm(U5MR ~ Year * Continent, # The * species the interaction between the two.
           data = U5MR_continent)

##8. Compare the three models with respect to their performance####
# Compares the 3 models based on their performance
compare_performance(mod1, mod2, mod3) %>% plot() # mod3 is the best one, with respect to R2 and AIC

# Compares the graphs of the predictions for all 3 models
U5MR_pred = U5MR_continent

U5MR_pred$mod1 = predict(mod1, U5MR_continent)
U5MR_pred$mod2 = predict(mod2, U5MR_continent)
U5MR_pred$mod3 = predict(mod3, U5MR_continent)

U5MR_pred = U5MR_pred %>% 
  pivot_longer(cols = mod1:mod3,
               names_to = "Models",
               values_to = "Predicted_U5MR")

view(U5MR_pred)

##9. Plot the 3 models’ predictions####
ggplot(U5MR_pred, 
       aes(x = Year,
           y = Predicted_U5MR,
           color = Continent)) +
  geom_line() +
  facet_wrap(~ Models) +
  labs(y = "Predicted U5MR",
       title = "Model predictions")

##10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020.####
Pred_Ecuador = predict(mod3, newdata = data.frame(Year = 2020, Continent = "Americas"))
print(Pred_Ecuador)

#Better model?
mod4 = glm(U5MR ~ Year:Continent + Year:Region, # The * species the interaction between the two.
           data = U5MR_continent)
Pred_Ecuador = predict(mod4, newdata = data.frame(Year = 2020, Continent = "Americas", Region = "South America"))
print(Pred_Ecuador)



