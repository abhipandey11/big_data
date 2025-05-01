# Load necessary libraries
install.packages(c("tm", "syuzhet", "tidytext", "ggplot2", "wordcloud"))
library(tm)
library(syuzhet)
library(tidytext)
library(ggplot2)
library(wordcloud)

# --- Step 1: Load and Import WhatsApp Data ---
file_path <- file.choose()  # Select the exported .txt file
chat_data <- readLines(file_path, encoding = "UTF-8")

# Remove timestamps, usernames, and convert to lowercase
clean_text <- gsub("^\\[.*\\] ", "", chat_data)  # Remove timestamps
clean_text <- gsub(".*: ", "", clean_text)        # Remove usernames
clean_text <- tolower(clean_text)                 # Convert to lowercase

# --- Step 2: Text Cleaning and Preprocessing ---
# Create a text corpus
corpus <- Corpus(VectorSource(clean_text))

# Clean text: Remove punctuation, numbers, stopwords, etc.
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Convert corpus to data frame
text_df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

# --- Step 3: Sentiment Analysis ---
sentiments <- get_nrc_sentiment(text_df$text)
sentiment_data <- cbind(text_df, sentiments)

# Summarizing sentiment counts
sentiment_summary <- colSums(sentiment_data[, 2:11])

# --- Step 4: Sentiment Visualization ---
# Bar Chart for Sentiment Distribution
barplot(sentiment_summary, col = rainbow(10), las = 2,
        main = "Sentiment Analysis of WhatsApp Chat",
        ylab = "Count")

# --- Step 5: Word Cloud for Text Visualization ---
# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
term_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(term_matrix), decreasing = TRUE)

# Generate Word Cloud
set.seed(42)
wordcloud(words = names(word_freq), 
          freq = word_freq, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# --- Step 6: Advanced Visualization with ggplot2 ---
tidy_sentiments <- data.frame(sentiment = names(sentiment_summary), 
                              count = as.numeric(sentiment_summary))

ggplot(tidy_sentiments, aes(x = reorder(sentiment, -count), y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sentiment Analysis Visualization", 
       x = "Sentiment", 
       y = "Count") +
  coord_flip()

cat("Analysis Complete! 🚀")
