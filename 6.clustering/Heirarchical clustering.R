title: "Clustering for the crime data"

  
library(readr)
crime_data<-read.csv("file:///E:/assignments data/clustering/crime_data.csv")
View(crime_data)
attach(crime_data)
summary(crime_data)

str(crime_data)

crime_data1<-crime_data[, 2:5]
View(crime_data1)

#Normalizing the data
normalized_data<-scale(crime_data1)
normalized_data


#distance matrix
d <- dist(normalized_data, method="euclidean")

#Heirarchical clustering using complete linkage method
fit<-hclust(d, method="complete")

#dendrogram
plot(fit)
plot(fit, hang=-1)

rect.hclust(fit,plot(fit, hang=-1), k=4, border="green")

#using Cutree
groups<-cutree(fit, k=4)
membership<-as.matrix(groups)

final<-data.frame(crime_data, membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
final1

aggregate(crime_data[,-1],by=list(final$membership),mean)

# Group 2 have the higher rate of crime


