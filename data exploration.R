# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)

# Assignment 1
# Step 1: Load iris dataset
df <- iris

# Step 2: Convert Data Types (no dates in iris, skip date parsing)
# Convert Species to factor (already is, but good practice)
df$Species <- as.factor(df$Species)

# Summary of dataset
summary(df)

# Step 3: Exploratory Data Analysis (EDA)
# Histogram of Sepal.Length
ggplot(df, aes(x = Sepal.Length)) +
  geom_histogram(fill = "steelblue", bins = 20, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Sepal Length", x = "Sepal Length", y = "Frequency")

# Bar chart of Species counts
ggplot(df, aes(x = Species, fill = Species)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Count by Species", x = "Species", y = "Count")

# Step 4: Handle Missing Values (iris has none, but demonstration included)
# Check and fill NAs
df[is.na(df)] <- 0  # or use median if numeric
# Example: Impute Sepal.Width if it had NA
# df$Sepal.Width[is.na(df$Sepal.Width)] <- median(df$Sepal.Width, na.rm = TRUE)

# Step 5: Remove Duplicates
df <- df[!duplicated(df), ]

# Step 6: Standardization & Normalization
num_cols <- names(df)[sapply(df, is.numeric)]

# Standardization (Z-score)
df[num_cols] <- scale(df[num_cols])

# Min-Max Normalization (optional — if needed after scaling)
normalize_minmax <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
df[num_cols] <- as.data.frame(lapply(df[num_cols], normalize_minmax))

# Step 7: Encode Categorical Variables
# Use dummyVars to convert Species into dummy variables
df_encoded <- dummyVars(" ~ .", data = df) %>%
  predict(df) %>%
  as.data.frame()

# Step 8: Save the Clean Dataset
write.csv(df_encoded, "cleaned_iris_dataset.csv", row.names = FALSE)
print("Clean dataset saved as 'cleaned_iris_dataset.csv'")
