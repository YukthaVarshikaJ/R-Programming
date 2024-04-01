#2-1
string <- "hello world"
unique_chars <- unique(strsplit(string, "")[[1]])
cat("Unique characters in the string:", unique_chars, "\n")

vector <- c(1, 2, 3, 3, 4, 5, 5)
unique_numbers <- unique(vector)
cat("Unique numbers in the vector:", unique_numbers, "\n")

#2-2
a <- c(1, 2, 3)
b <- c(4, 5, 6)
c <- c(7, 8, 9)
matrix <- cbind(a, b, c)
print(matrix)

#2-3
set.seed(123)  
random_numbers <- rnorm(100)
occurrences <- table(random_numbers)
print(occurrences)

#2-4
data <- read.csv("C://Users//Yuktha Varshika//Documents//R Programming csv files//stu.csv")
print(data)

#2-5
numeric_vector <- c(1, 2, 3)
character_vector <- c("a", "b", "c")
logical_vector <- c(TRUE, FALSE, TRUE)

cat("Numeric vector:", numeric_vector, "\n")
cat("Type of numeric vector:", class(numeric_vector), "\n")
cat("Character vector:", character_vector, "\n")
cat("Type of character vector:", class(character_vector), "\n")
cat("Logical vector:", logical_vector, "\n")
cat("Type of logical vector:", class(logical_vector), "\n")

#2-6
matrix_5x4_rows <- matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
rownames(matrix_5x4_rows) <- c("Row1", "Row2", "Row3", "Row4", "Row5")
colnames(matrix_5x4_rows) <- c("Col1", "Col2", "Col3", "Col4")
print(matrix_5x4_rows)

matrix_3x3_rows <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
rownames(matrix_3x3_rows) <- c("Row1", "Row2", "Row3")
colnames(matrix_3x3_rows) <- c("Col1", "Col2", "Col3")
print(matrix_3x3_rows)

matrix_2x2_columns <- matrix(1:4, nrow = 2, ncol = 2, byrow = FALSE)
rownames(matrix_2x2_columns) <- c("Row1", "Row2")
colnames(matrix_2x2_columns) <- c("Col1", "Col2")
print(matrix_2x2_columns)

#2-7
a =  array(1:24,
  dim = c(4, 3, 2),
  dimnames = list(
    c("Col1", "Col2", "Col3", "Col4"),
    c("Row1", "Row2", "Row3"),
    c("Part1", "Part2"))
)
print(a)

#2-8
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
array_data <- array(c(vector1, vector2), dim = c(3, 3, 2))
print(array_data)

#2-9
numeric_vector <- c(1, 2, 3)
character_matrix <- matrix(c("a", "b", "c", "d"), nrow = 2)
custom_function <- function(x) {return(x^2)}
my_list <- list(numeric_vector, character_matrix, custom_function)
print(my_list)

#2-10
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
     xlab = "X-axis", ylab = "Y-axis", main = "Empty Plot with Axes Limits")


#analytical-1
city_A <- c(22, 25, 21, 23, 24, 22, 20)
city_B <- c(18, 20, 19, 21, 20, 19, 18)
city_C <- c(30, 32, 31, 33, 34, 32, 31)

avg_temp_A <- mean(city_A)
avg_temp_B <- mean(city_B)
avg_temp_C <- mean(city_C)

cat("Average temperature for City A:", avg_temp_A, "°C\n")
cat("Average temperature for City B:", avg_temp_B, "°C\n")
cat("Average temperature for City C:", avg_temp_C, "°C\n")

highest_avg_temp <- max(avg_temp_A, avg_temp_B, avg_temp_C)
cat("City with the highest average temperature:", 
    ifelse(highest_avg_temp == avg_temp_A, "City A", 
           ifelse(highest_avg_temp == avg_temp_B, "City B", "City C")), 
    "with a temperature of", highest_avg_temp, "°C\n")

var_temp_A <- var(city_A)
var_temp_B <- var(city_B)
var_temp_C <- var(city_C)

cat("Variance in temperature for City A:", var_temp_A, "\n")
cat("Variance in temperature for City B:", var_temp_B, "\n")
cat("Variance in temperature for City C:", var_temp_C, "\n")

max_temp_day_A <- which.max(city_A)
max_temp_day_B <- which.max(city_B)
max_temp_day_C <- which.max(city_C)

cat("Day with the maximum temperature in City A: Day", max_temp_day_A, "with a temperature of", city_A[max_temp_day_A], "°C\n")
cat("Day with the maximum temperature in City B: Day", max_temp_day_B, "with a temperature of", city_B[max_temp_day_B], "°C\n")
cat("Day with the maximum temperature in City C: Day", max_temp_day_C, "with a temperature of", city_C[max_temp_day_C], "°C\n")

#analytical-2
FinancialTransaction <- function(amount, date, description) 
  {
  transaction <- list(
    amount = amount,
    date = as.Date(date, format = "%d/%m/%Y"),
    description = description
  )
  class(transaction) <- "FinancialTransaction"
  return(transaction)
}
print.FinancialTransaction <- function(x) {
  cat("Amount:", x$amount, "\n")
  cat("Date:", format(x$date, "%d/%m/%Y"), "\n")
  cat("Description:", x$description, "\n")
}
transaction1 <- FinancialTransaction(amount = 500000, date = "12/07/2000", description = "computers")
transaction2 <- FinancialTransaction(amount = 200000, date = "13/08/2000", description = "Software")
transaction3 <- FinancialTransaction(amount = 100000, date = "14/09/2000", description = "Home Product")

print(transaction1)
print(transaction2)
print(transaction3)

#analytical3
# Load the required library
library(ggplot2)

# Create a sample dataset 'traffic_data'
traffic_data <- data.frame(
  day = 1:30,
  accidents = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
                15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
                25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
)

# Fit a Poisson regression model
poisson_model <- glm(accidents ~ day, data = traffic_data, family = "poisson")

# Summarize the model's output
summary(poisson_model)

# Interpret the coefficient for 'day'
# The coefficient for 'day' represents the change in the log count of accidents for a one-unit increase in 'day'.
# Since Poisson regression models the log of the mean response as a linear combination of the predictors,
# exponentiating the coefficient gives the ratio of the predicted mean number of accidents for each additional day.
# For example, if the coefficient for 'day' is 0.1, it means that for each additional day, the predicted mean number
# of accidents increases by a factor of exp(0.1) = 1.105.

# Predict the number of accidents on the 31st day
new_data <- data.frame(day = 31)
predicted_accidents <- predict(poisson_model, newdata = new_data, type = "response")

# Report the predicted count
cat("Predicted number of accidents on the 31st day:", round(predicted_accidents))



