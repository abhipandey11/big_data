# Install and load necessary packages
install.packages(c("caret", "ggplot2", "dplyr", "readr"))
library(caret)
library(ggplot2)
library(dplyr)
library(readr)

# Load the Students Score dataset
file_path <- file.choose()  # Open file dialog to choose the file
data <- read.csv(file_path)  # Read CSV file
data <- as.data.frame(data)  # Ensure it's a DataFrame

# View structure & summary
print(colnames(data))  # See existing column names
str(data)
summary(data)

# Rename columns if necessary
colnames(data) <- c("Hours", "Scores")

# Split Data into Training & Testing Sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$Scores, p = 0.8, list = FALSE)  
trainData <- data[trainIndex, ]  # 80% Training data
testData  <- data[-trainIndex, ]  # 20% Testing data

# Train a Linear Regression Model
model <- train(Scores ~ Hours, 
               data = trainData, 
               method = "lm", 
               trControl = trainControl(method = "cv", number = 10))  # 10-fold cross-validation

# Print model summary
print(summary(model$finalModel))

# Make Predictions & Evaluate Model
predictions <- predict(model, testData)

# Compute RMSE & R-squared using caret's built-in function
metrics <- postResample(predictions, testData$Scores)
rmse <- metrics["RMSE"]
rsq <- metrics["Rsquared"]

# Print evaluation metrics
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", rsq, "\n")

# Visualize Predictions vs Actual Values
ggplot(testData, aes(x = Scores, y = predictions)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "Predicted vs Actual Scores",
       x = "Actual Scores",
       y = "Predicted Scores") +
  theme_minimal() +
  xlim(min(testData$Scores) - 5, max(testData$Scores) + 5) +
  ylim(min(predictions) - 5, max(predictions) + 5)
