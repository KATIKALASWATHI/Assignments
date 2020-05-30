---
  title: "using decision trees to prepare a model on fraud data"
---
 
library(readr)
library(caret)
data<-read.csv("file:///E:/assignments data/descition tree/Fraud_check.csv")
attach(data)
str(data)
hist(Taxable.Income)
sum(is.na(data))
data$taxble_income<-ifelse (Taxable.Income<30000, "Risky", "Good")
data$taxble_income<-as.factor(data$taxble_income)
fraud_data<-data[-3]
head(fraud_data)
intraininglocal<-createDataPartition(fraud_data$taxble_income, p=0.80, list=F)
train<-fraud_data[intraininglocal,]
test<-fraud_data[-intraininglocal,]
### decision tree
library(C50)
model<-C5.0(train$taxble_income~., data=train)
summary(model)
pred<-predict.C5.0(model, test)
a<-table(test$taxble_income, pred)
a
confusionMatrix(test$taxble_income, pred)
plot(model)

##### boosting#####
model1<-C5.0(train$taxble_income~., data=train, trails=40)
summary(model1)
pred1<-predict.C5.0(model1, test[-6])
confusionMatrix(test$taxble_income, pred1)
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(fraud_data$taxble_income, p=0.90, list=F)
  train1<-fraud_data[inTraininglocal,]
  test1<-fraud_data[-inTraininglocal,]
  
  fit<-C5.0(train1$taxble_income~.,data=train1)
  pred2<-predict.C5.0(fit, test1[-6])
  a<-table(test1$taxble_income,pred2)
  
  acc<-c(acc,sum(diag(a))/sum(a))
}

summary(acc)
acc
-----------------------------------------------------------------------------------
---
title: "decision tree can be built with target variable Sale"
---
library(readr)
data<-read.csv("file:///E:/assignments data/descition tree/Company_Data.csv")
View(data)
library(caret)
library(C50)
str(data)
head(data)
attach(data)
hist( Sales)
pairs(data)
highsales1<-ifelse(Sales<10, "No", "Yes")
data1<-data.frame(data, highsales1)
inTraininglocal<-createDataPartition(highsales1, p=0.75, list=F )
traindata<-data1[inTraininglocal, ]
testdata<-data1[-inTraininglocal, ]
names(traindata)
library(party)
optree=ctree(highsales1 ~ CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, data=traindata)
summary(optree)
plot(optree)
pred<-predict(optree, testdata)
head(pred)
a<-table(testdata$highsales1, pred)
a
confusionMatrix(testdata$highsales1, pred)
## accuracy=0.79 for better accuracies using boosting and bagging 
##boosting 
model<-C5.0(highsales1~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, data=traindata, tdrails=40)
pred_values<-predict.C5.0(model, testdata[-12])
a<-table(testdata$highsales1, pred_values)
a
acc<-sum(diag(a)/sum(a))
acc

## bagging
acc<-c()
for(i in 1:100){
  print(i)
  intraininglocal1<-createDataPartition(data1$highsales1, p=0.80, list=F)
  train<-data1[intraininglocal1,]
  test<-data1[-intraininglocal1,]
  model<-C5.0(train$highsales1~ CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, data=train)
  pred<-predict.C5.0(model, test[-12])
  b<-table(test$highsales1, pred)
  acc<-c(acc, sum(diag(b)/sum(b)))
}

summary(acc)
acc
------------------------------------------------------------------------
---
title: "Decision tree for the irish data"

library(party)
library(caret)
data(iris)
head(iris)
str(iris)
pairs(iris)
sum(is.na(iris))
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(iris[i], main=colnames(iris)[i])
}

## data partiotioning
intraininglocal<-createDataPartition(iris$Species, p=0.75, list=F)
train<-iris[intraininglocal,]
dim(train)
test<-iris[-intraininglocal,]
dim(test)
## model bulding
model<-ctree(train$Species~., data=train)
summary(model)
pred<-predict(model, train)
a=table(train$Species, pred)
a
##Accuracy
mean(pred==train$Species)*100
## finding predicted values and confution matrix using test data
pred_val<-predict(model, newdata=test)
confusionMatrix(test$Species, pred_val)
plot(model)




