#_________________Prediction Model for Predicting Price_____________
library(readr)
#loading data
data<-read.csv("E:\\assignments data\\ToyotaCorolla.csv")
#dataset details
View(data)
corralla<-data[,c("Price", "Age_08_04", "KM", "HP", "cc", "Doors","Gears", "Quarterly_Tax", "Weight")]
dim(corralla)
colnames(corralla)
class(corralla)
sum(is.null(corralla))#checking null values
sum(duplicated(corralla))#checking duplicates
unique(corralla)
attach(corralla)

##EDA
library(psych)
describe(corralla)
pairs(corralla)#checking collinearity
cor(corralla)
library(corpcor)
cor2pcor(cor(corralla))

##model generation
model<-lm(Price~., data=corralla)
summary(model)
# p values of cc and Doors are >0.05
model1<-lm(Price ~cc+Doors)
summary(model1)
library(car)
vif(model)
#vif <10 so there is no multicolliearity
influenceIndexPlot(model)
influencePlot(model)
#for better R-squared values removing the influence factors
#fitted multiple linear regression model
model2<-lm(Price ~ ., corralla[-c(81,222,961),])
summary(model2)
# here P values are <0.05 and
#multiple R-squared value is 0.8852
#Adjusted R-squared value is 0.8845
predicted_values<-predict(model2)
predicted_values
avPlots(model2)
par(mfrow=c(2,2))
plot(model2)

---------------------------------------------------------------------------------
---  -------------------------------------------------------------------------------------
#_________________________Predict Salse of the Computer__________
#loading data
library(readr)
data<-read.csv("E:\\assignments data\\Computer_Data (1).csv")
#dataset details and EDA
colnames(data)
computerData<-data[, c(2:11)]
View(computerData)
attach(computerData)
dim(computerData)
sum(is.null(computerData))
library(plyr)
computerData$cd<-revalue(computerData$cd, c("yes"="1", "no"="2"))
computerData$multi<-revalue(computerData$multi, c("yes"="1", "no"="2"))
computerData$premium<-revalue(computerData$premium, c("yes", "no"="2"))
summary(computerData)
head(computerData)
# checking for multicollinearity
plot(computerData) 
computerData$cd<-as.numeric(computerData$cd)
computerData$multi<-as.numeric(computerData$multi)
computerData$premium<-as.numeric(computerData$premium)
cor(computerData)
# pairwise correlation between predictor variables are low
# so there is no multicollinearity

library(corpcor)
cor2pcor(cor(computerData)) 
# finding influence factors
model<-lm(price ~., data=computerData)
summary(model)
library(car)
influenceIndexPlot(model)
influencePlot(model)  
model1<-lm(price ~., data=computerData[-c(1441,1701,3784,4478),])
summary(model1)
vif(model1)
# vif<10 so there is no multicollinearity
avPlots(model1)
# fitting of multiple linear regression model
model2<-lm(price ~ .-computerData$cd-computerData$multi, data=computerData[-c(1441,1701,3784,4478)])
summary(model2)
predicted_values<-predict(model2)
head(predicted_values)
# here p values <0.05
#multiple R-squared value is 0.7756
#Adjusted R-squared value is 0.7752
par(mfrow=c(2,2))
plot(model2)

------------------------------------------------------------------------------------
----  -------------------------------------------------------------------
#________________________Prediction Model for Profit of Startup_50 Data______________
  
#loading data
startup_50<-read.csv("E:\\assignments data\\50_Startups.csv")
#dataset details and EDA
View(startup_50)
attach(startup_50)
colnames(startup_50)
head(startup_50)
dim(startup_50)
sum(is.null(startup_50))
unique(startup_50)
library(plyr)
startup_50$State<-revalue(startup_50$State, c("New York"="1", "California"="2", "Florida"="3"))
startup_50$State<-as.numeric(startup_50$State)
class(startup_50$State)
# finding collinearity
pairs(startup_50)
# correlation matrix
cor(startup_50)
# partial correlation
library(corpcor)
cor2pcor(cor(startup_50))

model<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State)
summary(model)
# here Administation and Marketing.spend has greater than 0.05 
model.adm<-lm(Profit~Administration+Marketing.Spend)
summary(model.adm)
library(car)
vif(model)
# vif<10 there is no multicollinearity
influenceIndexPlot(model)
influencePlot(model)
# for better multiple R-square vakues removing influence variable 
model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State, data=startup_50[-c(46,47,49,50)])
summary(model1)
avPlots(model)
#fitted multiple linear regression model
model2<-lm(Profit ~.-Administration, data=startup_50[-c(46,47,49,50)])
summary(model2)
#here multiple R-squared value is 0.9505 and Adjusted R-squared value is 0.9472
#p value of marketing.spend is almost equal to 0.05
predicted_val<-predict(model2)
predicted_val

par(mfrow=c(2,2))
plot(model2)