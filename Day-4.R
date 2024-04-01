#4-1
# Load necessary libraries
library(datasets)
library(caTools) # for sample.split
library(nnet)    # for logistic regression
library(caret)   # for confusion matrix

# Load iris dataset
data("iris")

# Randomly sample the iris dataset
set.seed(123)  # for reproducibility
split <- sample.split(iris$Species, SplitRatio = 0.8)
train_data <- subset(iris, split == TRUE)
test_data <- subset(iris, split == FALSE)

# Convert Species column to factors with the same levels
test_data$Species <- factor(test_data$Species, levels = levels(train_data$Species))

# Logistic regression model
log_model <- multinom(Species ~ Petal.Width + Petal.Length, data = train_data)

# Predict probabilities using test data
test_probs <- predict(log_model, newdata = test_data, type = "probs")

# Predicted species
predicted_species <- colnames(test_probs)[apply(test_probs, 1, which.max)]
predicted_species <- factor(predicted_species, levels = levels(train_data$Species))

# Confusion matrix
confusion_matrix <- confusionMatrix(predicted_species, test_data$Species)
confusion_matrix




# (i) Compute mean, median, mode
values <- c(90, 50, 70, 80, 70, 60, 20, 30, 80, 90, 20)
mean_val <- mean(values)
median_val <- median(values)
mode_val <- as.numeric(names(table(values)[table(values) == max(table(values))]))

# Display results
print(paste("Mean:", mean_val))
print(paste("Median:", median_val))
print(paste("Mode:", mode_val))

# (ii) Find 2nd highest and 3rd lowest values
sorted_values <- sort(unique(values), decreasing = TRUE)
second_highest <- sorted_values[2]
third_lowest <- sorted_values[length(sorted_values) - 2]

# Display results
print(paste("2nd highest value:", second_highest))
print(paste("3rd lowest value:", third_lowest))

#4-2
# Load the airquality dataset
data("airquality")

# i. Compute the mean temperature (without using built-in function)
mean_temp <- sum(airquality$Temp) / length(airquality$Temp)
print(paste("Mean Temperature:", mean_temp))

# ii. Extract the first five rows from airquality
first_five <- head(airquality, 5)
print(first_five)

# iii. Extract all columns from airquality except Temp and Wind
subset_data <- airquality[, !(names(airquality) %in% c("Temp", "Wind"))]
print(subset_data)

# iv. Which was the coldest day during the period?
coldest_day <- airquality[which.min(airquality$Temp), ]
print(paste("Coldest Day:", coldest_day$Day))

# (i) Summary statistics of air quality dataset
summary_stats <- summary(airquality)
print(summary_stats)

# (ii) Melt airquality dataset and display as long-format data
library(reshape2)
melted_data <- melt(airquality)
print(melted_data)

# (iii) Melt airquality data and specify month and day to be "ID variables"
melted_data_id <- melt(airquality, id.vars = c("Month", "Day"))
print(melted_data_id)

# (iv) Cast the molten airquality dataset with respect to month and date features
casted_data <- dcast(melted_data_id, Month + Day ~ variable)
print(casted_data)

# (v) Compute the average of Ozone, Solar.R, Wind, and temperature per month
average_monthly_data <- aggregate(. ~ Month, data = airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], 
                                  FUN = mean, na.rm = TRUE)
print(average_monthly_data)

#4-3
# Load necessary library
library(ggplot2)

# (i) Find and handle missing values
missing_values <- colMeans(is.na(airquality))
print("Missing Values:")
print(missing_values)

# Drop missing values if less than 10% else replace with mean
for (col in names(airquality)) {
  if (missing_values[col] < 0.1) {
    airquality <- na.omit(airquality)
  } else {
    airquality[[col]][is.na(airquality[[col]])] <- mean(airquality[[col]], na.rm = TRUE)
  }
} 

# (ii) Apply linear regression on Ozone and Solar.R
lm_model <- lm(Ozone ~ Solar.R, data = airquality)
print(lm_model)
# (iii) Plot scatter plot and add regression line
ggplot(airquality, aes(x = Solar.R, y = Ozone)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatter plot of Ozone vs Solar.R with Regression Line")

#4-4
# Load the ChickWeight dataset
data("ChickWeight")
library(dplyr)

# (i) Order the data frame by "weight" grouped by "diet" and extract the last 6 records
ordered_data <- ChickWeight %>%
  group_by(Diet) %>%
  arrange(weight) %>%
  slice_tail(n = 6)

print(ordered_data)

# (ii) a. Perform melting function based on "Chick", "Time", "Diet" features as ID variables
library(reshape2)
melted_data <- melt(ChickWeight, id.vars = c("Chick", "Time", "Diet"))
print("Melted Data:")
print(head(melted_data))

# (b) Perform cast function to display the mean value of weight grouped by Diet
library(dplyr)

mean_weight <- melted_data %>%
  group_by(Diet, variable) %>%
  summarise(mean_weight = mean(value))

print("Mean Value of Weight Grouped by Diet:")
print(mean_weight)


# c. Perform cast function to display the mode of weight grouped by Diet
library(dplyr)
mode_weight <- melted_data %>%
  group_by(Diet, value) %>%
  summarise(count = n()) %>%
  slice(which.max(count)) %>%
  select(-count)
print("Mode of Weight Grouped by Diet:")
print(mode_weight)

#4-5
#  a.Load necessary library
library(ggplot2)

# Create Box plot
ggplot(ChickWeight, aes(x = factor(Diet), y = weight)) +
  geom_boxplot() +
  labs(title = "Box plot of Weight Grouped by Diet")

# b. Filter data for Diet-1 category
diet_1_data <- subset(ChickWeight, Diet == 1)

# Create Histogram
ggplot(diet_1_data, aes(x = weight)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Weight for Diet-1")

# c.Create Scatter plot
ggplot(ChickWeight, aes(x = Time, y = weight, color = factor(Diet))) +
  geom_point() +
  labs(title = "Scatter plot of Weight vs Time Grouped by Diet")


#analytical-1
# a.Given data frame
exam_data <- data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no')
)

# Sort by name and score columns
sorted_data <- exam_data[order(exam_data$name, exam_data$score), ]
print(sorted_data)

# b.Calculate average score based on "qualify"
average_score <- aggregate(score ~ qualify, data = exam_data, FUN = "mean")
print(average_score)


#analytical-2
# a.Given data
data <- c(25, 15, 23, 40, 27, 25, 23, 25, 20)

# Calculate Karl-Pearson’s coefficient of skewness
skewness <- (mean(data) - median(data)) / sd(data)
print(skewness)

# b.Given data
X <- c(68, 64, 75, 50, 64, 80, 75, 40, 55, 64)
Y <- c(62, 58, 68, 45, 81, 60, 68, 48, 58, 70)

# Compute correlation coefficient
correlation_coefficient <- cor(X, Y)
print(correlation_coefficient)


