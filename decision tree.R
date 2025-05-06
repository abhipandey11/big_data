# Load necessary libraries
install.packages(c("rpart", "rpart.plot", "caret", "dplyr"))

library(rpart)
library(rpart.plot)
library(caret)     # For accuracy calculation and confusion matrix
library(dplyr)

# Load the dataset
file_path <- file.choose()  # Open file dialog
data <- read.csv(file_path)  # Read CSV file
data <- as.data.frame(data)  # Ensure it's a DataFrame

# Select relevant columns for classification
data <- data %>%
  select(Survived, Pclass, Age, SibSp, Parch, Fare)

# Handle missing values by replacing them with the median of each column
data <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Convert 'Survived' to a factor for classification
data$Survived <- as.factor(data$Survived)

# Split the data into training (70%) and testing (30%)
set.seed(42)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# --- Step 1: Build the Decision Tree Model ---
decision_tree <- rpart(Survived ~ Pclass + Age + SibSp + Parch + Fare, 
                       data = train_data, 
                       method = "class")

# --- Step 2: Visualize the Decision Tree ---
rpart.plot(decision_tree, main = "Decision Tree for Titanic Data", type = 3, extra = 102)

# --- Step 3: Model Prediction ---
predictions <- predict(decision_tree, test_data, type = "class")

# --- Step 4: Model Evaluation ---
conf_mat <- confusionMatrix(predictions, test_data$Survived)
print(conf_mat)

# Display accuracy
cat("Model Accuracy: ", round(conf_mat$overall["Accuracy"] * 100, 2), "%\n")
