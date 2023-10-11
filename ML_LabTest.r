install.packages('ISLR')

library(ISLR)
library(ggplot2)

# Install the caTools package (for data splitting)
install.packages('caTools')
library(caTools)

install.packages('ISLR2')

# Load the dataset
data("Hitters")

a<- data("Hitters")
head(a,6)

# For this example, let's consider only a subset of columns for the predictors and response
# Adjust columns as per your requirement
dataset <- Auto[, c("CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks", "League", "Division", "PutOuts", "Assists", "Errors", "NewLeague",'Salary')]


# Check for missing values and handle them if necessary
# For this example, we'll remove rows with missing values
dataset <- na.omit(dataset)

# Split the dataset into training and testing sets
set.seed(123)  # Set seed for reproducibility
split <- sample.split(dataset$acceleration, SplitRatio = 0.8)
train_data <- dataset[split, ]
test_data <- dataset[!split, ]

# Create the Linear Regression model
model <- lm(acceleration ~ cylinders + horsepower + weight + displacement, data = train_data)

# Fit the model
model_fit <- summary(model)

# Print summary of the model
print(model_fit)


# Plot the model graph (decision boundaries)
# Note: Linear regression doesn't have decision boundaries in the same way as classifiers
# You can plot the actual vs. predicted values

# Predict on the test set
predictions <- predict(model, newdata = test_data)


# Display prediction results on the test set
# For this example, let's print the predicted values alongside the actual values
result_df <- data.frame(Actual = test_data$acceleration, Predicted = predictions)
print(head(result_df))

# Additional evaluation metrics can be computed here, such as RMSE, MAE, etc.

# Plot actual vs. predicted values
ggplot(result_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted values")



