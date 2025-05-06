# Install required packages if not installed
install.packages("recommenderlab")
install.packages("ggplot2")

# Load libraries
library(recommenderlab)
library(ggplot2)

# Load MovieLens dataset
data("MovieLense")

# Train-Test Split (80%-20%)
set.seed(123)
trainIndex <- sample(1:nrow(MovieLense), size = 0.8 * nrow(MovieLense))
train_data <- MovieLense[trainIndex]
test_data <- MovieLense[-trainIndex]

# Train a User-Based Collaborative Filtering (UBCF) Model
ubcf_model <- Recommender(train_data, method = "UBCF", param = list(nn = 50))

# Select a specific user for visualization
user_id <- sample(rownames(test_data), 1)  # Randomly select a user from test data
print(paste("Selected User ID:", user_id))

# Get top-N recommendations (e.g., top 10 movies)
top_n <- 10
user_pred <- predict(ubcf_model, test_data[user_id], n = top_n)

# Convert recommendations to a list
rec_list <- as(user_pred, "list")[[1]]

# Convert to data frame for visualization
recommendations_df <- data.frame(
  Movie = rec_list,
  Rank = seq(1, length(rec_list))
)

# Plot: Recommended Movies vs. Rank
ggplot(recommendations_df, aes(x = reorder(Movie, -Rank), y = Rank)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +  # Flip the bar chart for better readability
  labs(title = paste("Top", top_n, "Recommended Movies for User", user_id),
       x = "Movie",
       y = "Ranking (Lower is Better)") +
  theme_minimal()
