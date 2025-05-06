# Load necessary libraries
install.packages(c("ggplot2", "dplyr", "factoextra"))
library(ggplot2)
library(dplyr)
library(factoextra)  # For Elbow Method visualization

# Load the dataset
file_path <- file.choose()  # Open file dialog
data <- read.csv(file_path)  # Read CSV file
data <- as.data.frame(data)  # Ensure it's a DataFrame

# Select relevant numerical columns for clustering
numeric_columns <- c("Pclass", "Age", "SibSp", "Parch", "Fare")
data_numeric <- data[, numeric_columns]

# Handle missing values by replacing them with the median of each column
data_numeric <- data_numeric %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# --- Step 1: Find the Optimal Number of Clusters using the Elbow Method ---
set.seed(42)
wcss <- sapply(1:10, function(k) {
  kmeans(data_numeric, centers = k, nstart = 20)$tot.withinss
})

# Plot the Elbow Curve
plot(1:10, wcss, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (k)", ylab = "WCSS (Within-Cluster Sum of Squares)",
     main = "Elbow Method for Optimal k")

# --- Step 2: Perform K-Means Clustering with Optimal k (Adjust if Needed) ---
optimal_k <- 3  # Adjust manually after checking the Elbow plot
kmeans_result <- kmeans(data_numeric, centers = optimal_k, nstart = 20)

# Print clustering results
print(kmeans_result)

# Add the cluster results to the original dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# --- Step 3: Visualize Clusters (Using Age vs. Fare) ---
ggplot(data, aes(x = Age, y = Fare, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-Means Clustering on Tested Dataset", 
       x = "Age", 
       y = "Fare") +
  theme_minimal()
