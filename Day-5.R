#5-1
# a.Create Box plot for weight grouped by Diet
library(ggplot2)
ggplot(ChickWeight, aes(x = factor(Diet), y = weight)) +
  geom_boxplot() +
  labs(title = "Box plot of Weight Grouped by Diet")

# b. data for Diet-1 category
diet_1_data <- subset(ChickWeight, Diet == 1)

# Create Histogram for weight for Diet-1
ggplot(diet_1_data, aes(x = weight)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Weight for Diet-1")

# c.Create Scatter plot for weight vs Time grouped by Diet
ggplot(ChickWeight, aes(x = Time, y = weight, color = factor(Diet))) +
  geom_point() +
  labs(title = "Scatter plot of Weight vs Time Grouped by Diet")

#5-2
# a.Fit the multiple regression model
model <- lm(weight ~ Time + Diet, data = ChickWeight)

# Summarize the model
summary(model)

# b.Predict weight for Time = 10 and Diet = 1
new_data <- data.frame(Time = 10, Diet = 1)
predicted_weight <- predict(model, newdata = new_data)
print(predicted_weight)

# c.Calculate the model's residuals
residuals <- resid(model)
print(residuals)

#5-3
# Load the Titanic dataset
data(Titanic)

# Convert the dataset to a data frame for easier manipulation
titanic_df <- as.data.frame(Titanic)

# Load necessary library
library(ggplot2)

# a. Bar chart to show details of "Survived" based on passenger Class
ggplot(titanic_df, aes(x = Class, fill = factor(Survived))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Survival Status based on Passenger Class",
       x = "Passenger Class",
       y = "Count",
       fill = "Survived")

# b. Modify the above plot based on gender of people who survived
ggplot(titanic_df, aes(x = Class, fill = factor(Survived), color = Sex)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Survival Status based on Passenger Class and Gender",
       x = "Passenger Class",
       y = "Count",
       fill = "Survived",
       color = "Gender")

unique_ages <- unique(titanic_df$Age)
print(unique_ages)
summary(df$Age)
titanic_df$Age<-as.numeric(as.character(titanic_df$Age))
titanic_df<-titanic_df[!is.na(titanic_df$Age),]
# c. Histogram plot to show distribution of feature "Age"
ggplot(titanic_df, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Distribution of Age on Titanic",
       x = "Age",
       y = "Frequency")
