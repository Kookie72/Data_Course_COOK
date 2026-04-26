#Task 1####
library(tidyverse)
library(ggplot2)

Faculty_Salaries = read.csv("FacultySalaries_1995.csv")
View(Faculty_Salaries)
str(Faculty_Salaries)

my_salaries = Faculty_Salaries %>% 
  rename(Full_Professor_Salary = AvgFullProfSalary,
         Associate_Professor_Salary = AvgAssocProfSalary,
         Assistant_Professor_Salary = AvgAssistProfSalary,
         All_Professors_Salary = AvgProfSalaryAll,
         Full_Professor_Comp = AvgFullProfComp,
         Associate_Professor_Comp = AvgAssocProfComp,
         Assistant_Professor_Comp = AvgAssistProfComp,
         All_Professors_Comp = AvgProfCompAll,
         Full_Professor_Number = NumFullProfs,
         Associate_Professor_Number = NumAssocProfs,
         Assistant_Professor_Number = NumAssistProfs,
         Instructor_Number = NumInstructors,
         All_Professors_Number = NumFacultyAll) %>% 
  pivot_longer(cols = contains("_"),
                names_to = c("Job_Title", ".value"),
                names_sep = "_(?=[^_]+$)") # Splits at the LAST underscore

view(my_salaries) 
str(my_salaries)

# Define the labels
rank_labels = c("Assistant_Professor" = "Assist", 
                 "Associate_Professor" = "Assoc", 
                 "Full_Professor" = "Full")

my_salaries %>%
  filter(Job_Title %in% c("Assistant_Professor", "Associate_Professor", "Full_Professor"),
         Tier != "VIIB",
         !is.na(Salary)) %>% 
  # Reorder the Job_Title so the x-axis makes sense logically
  mutate(Job_Title = factor(Job_Title, levels = names(rank_labels))) %>%
  ggplot(aes(x = Job_Title, y = Salary, fill = Job_Title)) +
  geom_boxplot() +
  facet_wrap(~ Tier) +
  theme_minimal() +
  labs(x = "Rank",
    y = "Salary",
    fill = "Rank") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + # angle = 90 makes the labels vertical, vjust/hjust alignment ensures they line up correctly under the ticks
  scale_x_discrete(labels = c("Assistant_Professor" = "Assist",
                              "Associate_Professor" = "Assoc",
                              "Full_Professor" = "Full"),
    drop = TRUE) +
  scale_fill_discrete(labels = rank_labels)

#Task 2####
salaries_clean = my_salaries %>%
  filter(Job_Title %in% c("Assistant_Professor", "Associate_Professor", "Full_Professor"),
    Tier != "VIIB",
    !is.na(Salary))

# Build ANOVA model
# Formula: Response ~ Predictor1 + Predictor2 + Predictor3
salary_model = aov(Salary ~ State + Tier + Job_Title, data = salaries_clean)

# Display the summary output
summary(salary_model)

#Task 3####
juniper = read.csv("Juniper_Oils.csv")

view(juniper)

my_juniper = juniper %>% 
  pivot_longer(cols = c("alpha.pinene":"thujopsenal"),
               names_to = "Chemical",
               values_to = "Concentration")

view(my_juniper)
str(my_juniper)

#Task 4####
my_juniper %>% 
  ggplot(aes(x = YearsSinceBurn,
             y = Concentration)) +
  facet_wrap(~ Chemical, scales = "free") +
  geom_smooth(color = "blue", se = TRUE, method = "loess") +
  theme_minimal()

#Task 5####
library(broom)

chemical_model = glm(Concentration ~ Chemical * YearsSinceBurn, data = my_juniper) #interaction (*) between Chemical and YearsSinceBurn

my_chemicals = tidy(chemical_model) %>%
  filter(p.value < 0.05)# %>%

print(my_chemicals)  
  
  

  