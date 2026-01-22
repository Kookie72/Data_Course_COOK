#4. Returns a list of all .csv files in specified folder
list.files(path = "Data", pattern = ".csv")
#Save a variable that contains a list of all .csv files in specified folder
csv_files = list.files(path = "Data", pattern = ".csv")
#5. Returns to total number of files in variable
length(csv_files)
#6. Creates a variable that contains the contents of specified file
df = read.csv("Data/wingspan_vs_mass.csv")
#7. Returns the first 5 lines of variable
head(df, n = 5)
#8. Find all files in specified directory and subdirectories that start with b
list.files(path = "Data", pattern = "^b", recursive = TRUE)
#9. Displays first line of each file in folder and subfolders that start with b
b_files = list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)
for (x in b_files) {
  print(readLines(x, n = 1))
}
#10. Displays first line of each file in folder and subfolders that end with .csv
read_csv_files = list.files(path = "Data", pattern = ".csv", recursive = TRUE, full.names = TRUE)
for (x in read_csv_files) {
  print(readLines(x, n = 1))
}