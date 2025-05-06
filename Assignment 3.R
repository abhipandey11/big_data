library(ggplot2)
library(dplyr)
library(factoextra)


data<- iris[,1:4]
set.seed(123)
wcss<-sapply(1:10,function(k){
  kmeans(data,centers=k,nstart=20)$tot.withinss
})

plot(1:10,wcss,col="blue",pch=19,type="b",xlab="number of cluster",ylab="wcss",main="ELBOW Method")

optimal_cluster<-3
kresult<-kmeans(data,centers=3,nstart=20)
data$Cluster <- as.factor(kresult$cluster)
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) + 
  geom_point(size = 3, alpha = 0.7) + 
  labs(title = "K-Means Clustering on Tested Dataset", 
       x = "Sepal Length", 
       y = "Sepal Width") + 
  theme_minimal() 
