title: Perform clustering (Both hierarchical and K means clustering) for the airlines

library(readxl)
Airlines_data<-read_xlsx("E:\\assignments data\\clustering\\EastWestAirlines.xlsx", sheet="data")
View(Airlines_data)
attach(Airlines_data)
Airlines_data$Award<-Airlines_data$`Award?`
Airlines_data1<-Airlines_data[, c(2:11,13)]
colnames(Airlines_data1)
summary(Airlines_data1)
str(Airlines_data1)
#Normalizing data
norm_airline_data1<-scale(Airlines_data1)
#distance matrix
dist_airline_data1<-dist(norm_airline_data1, method="euclidean")
str(dist_airline_data1)
#heirarchical clustering
fit<-hclust(dist_airline_data1, method="complete")
fit
#Dendrogram
plot(fit, hang=-1)
rect.hclust(fit,plot(fit, hang=-1), k=5, border="red")
#cut tree
group_airline<-cutree(fit, k=5)
membership<-as.matrix(group_airline)
final<-data.frame(Airlines_data, membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
aggregate(final1[,-2],by=list(final$membership),mean)

----------------------------------------------------------------------------------------------------------------------------
#k means clustering
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
data<-Airlines_data1
#normlizing data
data<-scale(data)
#choosing optimum number of clusters
wss<-(nrow(data)-1)*sum(apply(data,2,var))
for(i in 2:15){
  wss[i]<-sum(kmeans(data, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="number of clusters", ylab="within group sum of squares")
#kmeans clustering
km<-kmeans(data, 5)
str(km)
km$centers
km$size
km$withinss
km$tot.withinss
km$totss
autoplot(km, data, frame=TRUE)
final1<-data.frame(norm_airline_data1,km$cluster )
colnames(final1)
aggregate(final1[, 1:12], by=list(final1$km.cluster), FUN = mean)


