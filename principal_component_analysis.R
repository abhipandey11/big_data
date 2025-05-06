# Install required packages if not already installed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")

# Load the libraries into the R session
library(ggplot2)
library(dplyr)
library(plotly)

# Load the iris dataset
data(iris)

# View the first few rows of the dataset
head(iris)

# Check the structure of the dataset
str(iris)

# Normalize the numerical columns (all columns except the 'Species' column)
iris_normalized <- iris[, 1:4] %>%
  scale()  # Standardizing the data (scaling)

# Apply PCA to the normalized data
pca_result <- prcomp(iris_normalized, center = TRUE, scale. = TRUE)

# Print the summary of the PCA results
summary(pca_result)

# View the principal components
pca_result$rotation

# View the variance explained by each principal component
pca_result$sdev^2 / sum(pca_result$sdev^2)

# Create a data frame with the PCA results and add the species information
pca_df <- data.frame(pca_result$x, Species = iris$Species)

# Plot the first two principal components (PC1 vs PC2)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "PCA - First Two Principal Components", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Create a 3D scatter plot of the first three principal components
pca_3d <- plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Species, 
                  type = "scatter3d", mode = "markers") %>%
  layout(title = "PCA - First Three Principal Components",
         scene = list(xaxis = list(title = "Principal Component 1"),
                      yaxis = list(title = "Principal Component 2"),
                      zaxis = list(title = "Principal Component 3")))
pca_3d

# Plot the explained variance (scree plot)
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)  # Proportion of variance
ggplot(data.frame(PC = 1:length(explained_variance), Variance = explained_variance), 
       aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Scree Plot", x = "Principal Components", y = "Proportion of Variance Explained") +
  theme_minimal()

