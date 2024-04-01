#3-1
vector1 <- 1:9
vector2 <- 10:18

my_array <- array(c(vector1, vector2), dim = c(3, 3, 2))

cat("Second row of the second matrix:\n", my_array[2, , 2], "\n")

cat("Element in the 3rd row and 3rd column of the 1st matrix:", my_array[3, 3, 1], "\n")

#3-2
array1 <- matrix(1:9, nrow = 3)
array2 <- matrix(10:18, nrow = 3)
array3 <- matrix(19:27, nrow = 3)

combined_array <- rbind(array1, array2, array3)
print(combined_array)

#3-3
array1 =  array(1:30, dim=c(3,4,2))
print(array1)

a <- array(seq(from = 50, length.out = 15, by = 2), c(5, 3))
print("Content of the array:")
print("5×3 array of sequence of even integers greater than 50:")
print(a)

#3-4
exam_data <- data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
)
subset_exam_data <- exam_data[c(3, 5), c(1, 3)]
print(subset_exam_data)
Country <- c("USA", "USA", "USA", "USA", "UK", "USA", "USA", "India", "USA", "USA")
exam_data$country <- Country
print(exam_data)

#3-5
new_exam_data <- rbind(exam_data, data.frame(name = c('Robert', 'Sophia'), score = c(10.5, 9), attempts = c(1, 3),
                                             qualify = c('yes', 'no'), country=c("USA","UK")))
print(new_exam_data)

result1<-data.frame(exam_data$name,exam_data$score) 
print(result1)

write.csv(exam_data, "exam_data_sorted.csv")

file_info <- read.csv("exam_data_sorted.csv")
print(file_info)

#3-6
data <- airquality
if(is.data.frame(data)) {
  print("It is a dataframe.")
}
ordered_data <- data[order(data$Ozone, data$Solar.R), ]

ordered_data <- ordered_data[, !(names(ordered_data) %in% c("Solar.R", "Wind"))]

print(ordered_data)

#3-7
data(women)

height_factor <- cut(women$height, breaks = 5)

print(height_factor)

#3-8
set.seed(123)

random_letters <- sample(LETTERS, size = 20, replace = TRUE)

random_factor <- factor(random_letters)

five_levels <- levels(sample(random_factor, 5))

print(five_levels)

#3-9
# Load the Iris dataset
data(iris)

# (i) Find dimension, Structure, Summary statistics, Standard Deviation of all features.
# Dimension
cat("Dimension of the dataset:\n")
print(dim(iris))

# Structure
cat("\nStructure of the dataset:\n")
str(iris)

# Summary statistics
cat("\nSummary statistics of the dataset:\n")
print(summary(iris))

# Standard Deviation of all features
cat("\nStandard Deviation of all features:\n")
print(apply(iris[, 1:4], 2, sd))

# (ii) Find mean and standard deviation of features grouped by three species of Iris flowers
cat("\nMean and standard deviation of features grouped by species:\n")
aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean)
aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=sd)

# (iii) Find quantile value of sepal width and length
cat("\nQuantile value of sepal width and length:\n")
print(quantile(iris$Sepal.Width))
print(quantile(iris$Sepal.Length))

# (iv) Create new data frame named iris1 with a new column named Sepal.Length.Cate that categorizes “Sepal.Length” by quantile
iris1 <- iris
iris1$Sepal.Length.Cate <- cut(iris1$Sepal.Length, breaks = quantile(iris1$Sepal.Length))
cat("\nNew data frame iris1 with Sepal.Length.Cate column:\n")
print(head(iris1))

# (v) Average value of numerical variables by two categorical variables: Species and Sepal.Length.Cate
cat("\nAverage value of numerical variables by Species and Sepal.Length.Cate:\n")
aggregate(iris1[,1:4], by=list(Species=iris1$Species, Sepal.Length.Cate=iris1$Sepal.Length.Cate), FUN=mean)

# (vi) Average mean value of numerical variables by Species and Sepal.Length.Cate
cat("\nAverage mean value of numerical variables by Species and Sepal.Length.Cate:\n")
aggregate(iris1[,1:4], by=list(Species=iris1$Species, Sepal.Length.Cate=iris1$Sepal.Length.Cate), 
          FUN=function(x) mean(x, na.rm=TRUE))

# (vii) Create Pivot Table based on Species and Sepal.Length.Cate
# Load the iris dataset
data(iris)

# Create a new variable with categorized sepal length
iris$Sepal.Length.Cate <- cut(iris$Sepal.Length, 
          breaks = quantile(iris$Sepal.Length), 
          labels = c("< 5.1", "5.1 - 5.8", "5.8 - 6.4", "> 6.4"), 
          include.lowest = TRUE)

# Create a pivot table
pivot_table <- table(iris$Species, iris$Sepal.Length.Cate)

# Print the pivot table
print(pivot_table)


#analytical-1
# Given vector
nums <- c(10, 20, 30, 40, 50, 60)

# Print maximum and minimum values
cat("Maximum value:", max(nums), "\n")
cat("Minimum value:", min(nums), "\n")

# Task (a): Create a sequence of numbers from 20 to 50
seq_20_to_50 <- seq(20, 50)

# Find mean of numbers from 20 to 60
mean_20_to_60 <- mean(seq(20, 60))

# Find sum of numbers from 51 to 91
sum_51_to_91 <- sum(seq(51, 91))

cat("\nMean of numbers from 20 to 60:", mean_20_to_60, "\n")
cat("Sum of numbers from 51 to 91:", sum_51_to_91, "\n")

##analytical-2
#Define the function f(x)
f <- function(x) {
  if (x < 0.5) {
    return(x)
  } else if (x >= 0.5 && x <= 1) {
    return(1 - x)
  } else {
    return(0)
  }
}

# Test the function
cat("\nTest cases for f(x):\n")
cat("f(0.3) =", f(0.3), "\n")
cat("f(0.7) =", f(0.7), "\n")
cat("f(1.5) =", f(1.5), "\n")

#b
# Define a function to find sum and mean of a given vector
sum_and_mean <- function(vector) {
  return(c(sum = sum(vector), mean = mean(vector)))
}

# Test the function
cat("\nTest case for sum_and_mean function:\n")
cat("For vector c(1, 2, 3, 4, 5):\n")
print(sum_and_mean(c(1, 2, 3, 4, 5)))

#analytical-3
# Create vectors for Height and GPA
Height <- c(66, 62, 63, 70, 74)
GPA <- c(3.80, 3.78, 3.88, 3.72, 3.69)

# Create a data frame
student_data <- data.frame(Height, GPA)

# Display the data frame
print(student_data)

cat("MEAN: ",mean(Height),"\n")
cat("MEAN: ",mean(GPA),"\n")

#analytical-4
# Create vectors a and b
a <- c(10, 20, 10, 10, 40, 50, 20, 30)
b <- c(10, 30, 10, 20, 0, 50, 30, 30)

# Create a data frame
df <- data.frame(a, b)

# Display duplicated elements
cat("Duplicated elements:\n")
print(df[duplicated(df), ])

# Display unique rows
cat("Unique rows:\n")
print(unique(df))

# Find the mean of "b" with respect to "a"
mean_b <- aggregate(df$b, by = list(df$a), FUN = mean)

# Print the result
print(mean_b)
