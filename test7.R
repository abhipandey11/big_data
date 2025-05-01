library(tm)
library(tidytext)
library(syuzhet)
library(wordcloud)
library(caret)
library(ggplot2)
library(dplyr)

chats<-c("[10/04/2024,20:31] Alice: I am so excited!!",
         "[10/04/2024, 24:32] Bob:Me too/I am so happy.I hope the rain does not ruin it",
          " [10/04/2024,20:33] Alice: ahhh. so sad!!",
         "[10/04/2024,20:31] Bob: Dont lose ope.be positive it would not rain today")
chats<-gsub("^\\[.*\\]","",chats)
chats<-gsub(".*:","",chats)
chats<-tolower(chats)

corpus<-Corpus(VectorSource(chats))

corpus<-corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords("en"))%>%
  tm_map(stripWhitespace)

textdf<-data.frame(text=sapply(corpus,as.character),stringsAsFactors = FALSE)

sentiments<-get_nrc_sentiment(textdf$text)
sentiment_data<-cbind(textdf,sentiments)
sentiment_summary<-colSums(sentiment_data[,2:11])

barplot(sentiment_summary,col=rainbow(10),las=2,ylab="count")

tdm<-TermDocumentMatrix(textdf)
tm<-as.matrix(tdm)
word_freq<-sort(rowSums(tm),decreasing=TRUE)


wordcloud(words =names(word_freq),
          freq = word_freq,
          max.words = 100,
          random.order = FALSE,
          col=brewer.pal(8,"Dark2"))
