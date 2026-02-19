# 1. Read "wingspan_vs_mass.csv" using both relative path and absolute path #
## Relative path ##
read.csv(file = "Data/wingspan_vs_mass.csv")
## Absolute path ##
read.csv(file = "/Users/jonia/Desktop/Data_Course_COOK/Data/wingspan_vs_mass.csv")
# 2. How many rows and columns in the file?
rp = read.csv(file = "Data/wingspan_vs_mass.csv")
##Number of rows
nrow(rp)
ncol(rp)
#creates a variable with multiple objects
fruit = c("apple", "peach", "strawberry")
num = c(1, 2, 3)
log = c(TRUE, FALSE)
vec = c("1", 2, T)
color = c("red", "purple", "yellow", "green", "orange", "pink", "white", "brown", "black", "cyan", "magenta", "blue", "violet")
as.factor(color)
#checks if variable is numeric
is.numeric(fruit)
is.numeric(num)
is.numeric(log)
#checks if variable is character
is.character(vec)
is.character(num)
is.character(log)
#Adds value to every object in variable but does not update value of variable
num + 100
#Matrix (two dim, same type of data)
matrix(c(1:6), nrow = 3, ncol = 2)
matrix(c(1:6), nrow = 3, ncol = 2, byrow = TRUE)
#Data Frame (two dim, different type of data, same length)
str(rp)
rp$mass + 100
rp$mass * 100
## Creates new column with specified values
mass_plus_2000 = rp$mass + 2000
#Array (multiple dim, same type)
array(c(1:12), dim = c(2, 2, 3))
#List (multi dim, different type, different length)
list(mass_plus_2000)
list(rp, number = c(1:6), fruit = "strawberry")
#Function (store a function)
my_function = function(x, y){
  out = x + y
  print(out)
}
my_function(1,2)
#Display specific variable in object
fruit[1]
fruit[2]
fruit[3]
#For loop
for (i in fruit) {
  print(i)
}
#While loop
i = 1
while (i <6) {
  print(i)
  i = i + 1
}

#1. create a data frame of favorite fruits
df_fruit = data.frame(name = c("straberry", "raspberry", "orange", "grape", "banana"))
#2. add calories to the data frame
df_fruit$calories = df_fruit$calories + 100
#list column names
names(df_fruit)
#3. write a loop to print out name of fruit and their calories
df_fruit = data.frame(name = c("straberry", "raspberry", "orange", "grape", "banana"),
                      calories = c(500, 300, 400, 100, 200))
for (i in 1:nrow(df_fruit)) {
  print(c("Fruit:", df_fruit$name[i], "Calories:",df_fruit$calories[i]))
  #print(df_fruit$calories[i])
}
#3b. write a loop that only prints out specified column
df_fruit = data.frame(name = c("straberry", "raspberry", "orange", "grape", "banana"),
                      calories = c(500, 300, 400, 100, 200))
df_fruit$calories[2]
for (i in 1:nrow(df_fruit)) {
  #print(i)
  out = df_fruit$calories[i]
  print(out)
}

