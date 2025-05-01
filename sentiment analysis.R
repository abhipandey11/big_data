library(tm)
library(wordcloud)
library(syuzhet)
library(tidytext)
library(ggplot2)
library(dplyr)



chat<- c(
  "[12/04/2024, 10:01] Alice: I’m so excited for the trip!",
  "[12/04/2024, 10:02] Bob: Same here! Can’t wait 😍",
  "[12/04/2024, 10:03] Alice: Just hope the weather stays good.",
  "[12/04/2024, 10:04] Bob: Yeah, the rain would ruin everything 😞",
  "[12/04/2024, 10:05] Alice: Don’t worry, I checked — it’ll be sunny ☀️",
  "[12/04/2024, 10:06] Bob: Awesome! This is going to be great!"
)

chat<-gsub("^\\[.*\\]","",chat)
chat<-gsub(".*:","",chat)
chat<-tolower(chat)

corpus<-Corpus(VectorSource(chat))

corpus<-corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords("en"))%>%
  tm_map(stripWhitespace)

text_df<-data.frame(text=sapply(corpus,as.character),stringsAsFactors = FALSE)

sentiments<-get_nrc_sentiment(text_df$text)
sentiment_data<-cbind(text_df,sentiments)

summary_sentiment<-colSums(sentiment_data[,2:11])
barplot(summary_sentiment,col=rainbow(10),las=2,ylab="count")


tdm<-TermDocumentMatrix(corpus)
tm<-as.matrix(tdm)
word_freq<-sort(rowSums(tm),decreasing = TRUE)

wordcloud(words=names(word_freq),
          freq= word_freq,
          max.words = 100,
          random.order = FALSE,
          color=brewer.pal(8,"Dark2"))
