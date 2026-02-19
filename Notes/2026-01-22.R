mtcars

#1. Save mtcars dataset to a new object####
df_mtcars = mtcars

#Displays specified rows and columns
df_mtcars[1:2,]
df_mtcars[,1:2]
df_mtcars[1:2, 1:2]

#1.5 examine the data structure amd type####
View(df_mtcars)

#2. Save cars with mpg > 20 to a new obj called "good_cars"####
good_cars = df_mtcars[df_mtcars$mpg > 20, ]
dim(good_cars)
max(good_cars)
min(good_cars)
View(good_cars)
write.csv(good_cars, file = "Data/good_cars.csv")

#Get detailed information on specified command
?write.csv

for (i in 1:nrow(df_mtcars)) {
  if (df_mtcars[i,1] > 20) {
   #Still trying to figure this one out 
  }
}

##1. get the car with cyl equal to 4####
df_mtcars = mtcars
four_cyl = df_mtcars[df_mtcars$cyl == 4, ]
four_cyl

##2. save both mpg > 20 and cyl equal to 4 into a new object####
df_mtcars = mtcars
df_mpg_cyl = df_mtcars[df_mtcars$mpg > 20 & df_mtcars$cyl == 4,]
df_mpg_cyl

##3. what are the data type of each cols?####
## convert all cols to character
str(df_mpg_cyl)
chr_car = df_mpg_cyl
for (i in 1:ncol(chr_car)) {
  chr_car[, i] = as.character(chr_car[, i])
}
str(chr_car)

## alternative method
?apply()
chr_car = df_mpg_cyl
new_car = apply(chr_car, 2, as.character)
str(new_car)
new_car
