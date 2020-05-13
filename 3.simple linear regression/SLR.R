#-------------------------------------------prediction model for Churn_out_rate-----------------------------------

library(readr)
library(ggplot2)
data<-read.csv("E:\\assignments\\3.simple linear regression\\emp_data.csv")
data

attach(data)
SH<-Salary_hike
COR<-Churn_out_rate

# Exploratory data analysis
# structure of data
str(data)

# Descriptive statistics
library(psych)
describe(data)
class(data)
dim(data)

#correlation
cor(SH, COR)
plot(SH, COR) ##correlation plot

par(mfrow=c(1,2))
boxplot(SH, main="salary_hike", col="skyblue")
boxplot(COR, main="churn_out_rate", col="skyblue")
hist(COR)

# the data is right skewed

#Regression analysis
reg<-lm(COR ~ SH)
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")

rmse1<-sqrt(mean(reg$residuals^2))
rmse1

# multiple R-squared value is 0.8312
# for better R-squared value using transformation
# Logarthmic transformation
reg_log<-lm(COR ~ log(SH))
summary(reg_log)
confint(reg_log, level=0.95)
predict(reg_log, interval="predict")
rmse2<-sqrt(mean(reg_log$residuals^2))
rmse2
# p value<0.05 and multiple R-squared value is 0.8486

#Exponential transformation
reg_exp<-lm(log(COR) ~ SH)
confint(reg_exp, level=0.95)
exp(predict(reg_exp, interval="predict"))
rmse3<-sqrt(mean(reg_exp$residuals^2))
rmse3
#p values <0.05 and multiple R-squared value is 0.8735

#polynomial of second degree transformation
reg_poly<-lm(COR ~ SH+I(SH^2)) 
summary(reg_poly)
confint(reg_poly, level=0.95)
predict(reg_poly, interval="predict")
rmse4<-sqrt(mean(reg_poly$residuals^2))
rmse4
#p values>0.05 and multiple R-squared value is 0.9737

#polynomial of three degree transformation
reg_poly1<-lm(COR ~ SH+I(SH^2)+I(SH^3))
summary(reg_poly1)
##p values<0.05 and multiple R-square valuesis 0.9893
#Adjusted R-squared value is 0.984
#polynomial of three degree gives the best R-squared values
confint(reg_poly1, level=0.95)
#predicted values
predict(reg_poly1, interval="predict")
#residuals
residuals<-reg_poly1$residuals
residuals
#residual mean square error
rmse<-sqrt(mean(residuals^2))
rmse
predicted_values<-predict(reg_poly1)
predicted_values
ggplot(data=data, aes(x=SH+I(SH^2)+I(SH^3), y=COR))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=SH+I(SH^2)+I(SH^3), y=predicted_values))
par(mfrow=c(2,2))
plot(reg_poly1)

#-----------------------------------------Predict delivery time using sorting time----------------------------------------------------
data<-read_csv("E:\\assignments\\3.simple linear regression\\delivery_time.csv")
View(data)
attach(data)
DT<-`Delivery Time`
ST<-`Sorting Time`
# Exploratory data analysis
head(data)
str(data)
dim(data)
summary(data)
library(psych)
describe(data)
cor(ST,DT)

#scatter diagram
plot(ST,DT)

par(mfrow=c(1,2))
boxplot(DT, main="Delivary Time", col="skyblue")
boxplot(ST, main="Sarting Timie", col="skyblue")
# from above boxplots there is no outliers
hist(DT)
#right skewed
# if |r|<0.85 , this is moderately correlated

# Regression analysis
reg<-lm(DT ~ ST)
summary(reg)
pred_val<-predict(reg)
pred_val
rmse<-sqrt(mean(reg$residuals^2))
rmse
confint(reg, level=0.95)
predict(reg, interval="predict")
library(ggplot2)
ggplot(data=data, aes(x=ST, y=DT))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=ST, y=pred_val))
# pval<0.05 and multiple R-square is 0.6823, for better multiple R-square value uing transformations

#logarthmic Transformation
reg_log<-lm(DT ~ log(ST))
summary(reg_log)
pred_val1<-predict(reg_log)
pred_val1
rmse<-sqrt(mean(reg_log$residuals^2))
rmse
confint(reg_log, level=0.95)
predict(reg_log, interval="predict")
ggplot(data=data, aes(x=log(ST), y=DT))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=log(ST),y=pred_val1))
# multiple R-squared value is 0.6794

#Exponentinal Transformation
reg_exp<-lm(log(DT) ~ ST)
summary(reg_exp)
pred_val2<-predict(reg_exp)
pred_val2
rmse<-sqrt(mean(reg_exp$residuals^2))
rmse
confint(reg_exp, level=0.95)
exp(predict(reg_exp, interval="predict"))
ggplot(data=data, aes(x=ST, y=log(DT)))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=ST, y=pred_val2))
# p value<0.05 and multiple R-squared value is 0.7109

#polynomial of second degree tronsformation
reg_poly<-lm(DT ~ ST+I(ST^2), data=data)
summary(reg_poly)
pred_val3<-predict(reg_poly)
pred_val3
confint(reg_poly, level=0.95)
predict(reg_poly, interval="predict")
ggplot(data=data, aes(x=ST+I(ST^2), y=DT))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=ST+I(ST^2), y=pred_val3))
# multiple R-squared value is 0.6934

#polynomial of Three degree transformation
reg_poly1<-lm(DT ~ ST+I(ST^2)+I(ST^3))
summary(reg_poly1)
pred_val4<-predict(reg_poly1)
pred_val4
confint(reg_poly1, level=0.95 )
predict(reg_poly1, interval="predict")
rmse<-sqrt(mean(reg_poly1$residuals^2))
rmse
ggplot(data=data, aes(x=ST+I(ST^2)+I(ST^3), y=DT))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=ST+I(ST^2)+I(ST^3), y=pred_val4))
# multiple R-squared value is 0.7034

#Exponential model gives the best multiple R-squared value is 0.7109 and adjusted r-squared value is 0.6957 and p values<0.05
predicted_dt_values<-exp(predict(reg_exp))
predicted_dt_values
par(mfrow=c(2,2))
plot(reg_exp)

#--------------------------------prediction model for Salary_hike------------------------------------------------
library(readr)
data<-read.csv("E:\\assignments\\3.simple linear regression\\Salary_Data.csv")
View(data)
attach(data)
YE<-YearsExperience
SH<-Salary
# Exploratory data analysis
# structure of data
str(data)
head(data)

# Descriptive statistics
library(psych)
describe(data)
summary(data)
cor(YE,SH)#heighly positively correlated
plot(YE,SH)
par(mfrow=c(1,2))
boxplot(YE, main=" YearsExperience ", col="skyblue")
boxplot(SH, main="Salary ", col="skyblue")
hist(SH)

#Regression Analysis
reg<-lm(SH ~ YE)
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")

#p values<0.05 and multiple R-squared value is 0.957 
#logarthmic transformation
reg_log<-lm(SH ~ log(YE))
summary(reg_log)
confint(reg_log, level=0.95)
predict(reg_log, interval="predict")
#p values <0.05 and multiple r-squared value is 0.8539

#Exponential Transformation
reg_exp<-lm(log(SH) ~ YE)
summary(reg_exp)
confint(reg_exp, level=0.95)
predict(reg_exp, interval="predict")
#p values<0.05 and multiple R-squared value is 0.932

#polynomial of second degree transformation
reg_poly<-lm(SH ~ YE+I(YE^2))
summary(reg_poly)
confint(reg_poly, level=0.95)
predict(reg_poly,interval="predict")
#multiple R squared valued is 0.957

#polynomial of three degree transformation
reg_poly1<-lm(SH ~ YE+I(YE^2)+I(YE^3))
summary(reg_poly1)
confint(reg_poly1, level=0.95)
predict(reg_poly1, interval="predict")
#multiple R square value is 0.9636
#Adjusted R squared value is 0.9594
#polynomial of three degree id best for R-squared values
predicted_values<-predict(reg_poly1)
predicted_values
library(ggplot2)
ggplot(data=data, aes(x=YE + I(YE^2) + I(YE^3), y=SH))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=YE + I(YE^2) + I(YE^3), y=predicted_values))

par(mfrow=c(2,2))
plot(reg_poly1)

#---------------------------------------------predict weight gained using calories consumed------------------------------------------
data<-read.csv("E:\\assignments\\3.simple linear regression\\calories_consumed.csv")
data
attach(data)
WG<-Weight.gained..grams.
CCON<-Calories.Consumed
str(data)

# Descriptive statistics
library(psych)
describe(data)
class(data)
dim(data)
#correlation
cor(CCON, WG)
#scatter diagream
plot(CCON, WG)

par(mfrow=c(1,2))
boxplot(CCON, main="Calories.Consumed", col="skyblue")
boxplot(WG, main="Weight.gained..grams", col="skyblue")
# from above boxplots there is no outliers
hist(WG)# the data is right skewed

#Regression analysis
reg<-lm(WG ~CCON )
summary(reg)
pred_val<-predict(reg)
pred_val
rmse<-sqrt(mean((pred_val-WG)^2))
rmse
confint(reg, level=0.95)
predict(reg, interval="predict")
library(ggplot2)
ggplot(data=data, aes(x=CCON, y=WG))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=CCON, y=pred_val))
# p vlaue<0.05 and multiple R-squared value is 0.8968
# for multiple R-squared value uding trasformations

#logarthmic Transformation
reg_log<-lm(WG ~ log(CCON))
summary(reg_log)
pred_val1<-predict(reg_log)
pred_val1
confint(reg_log, level=0.95)
predict(reg_log, interval="predict")
rmse1<-sqrt(mean(reg_log$residuals^2))
rmse1
ggplot(data=data, aes(x=log(CCON), y=WG))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=log(CCON), y=pred_val1))
# p value <0.05 and multiple R_squared value is 0.8077
# Exponential Transformation
reg_exp<-lm(log(WG) ~ CCON)
summary(reg_exp)
pred_val2<-predict(reg_exp)
pred_val2
confint(reg_exp, level=0.95)
predict(reg_exp, interval="predict")
ggplot(data=data, aes(x=CCON, y=log(WG)))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=CCON, y=pred_val2))
#p value<0.05 and multiple R-squared value is 0.8776

# polynomial of second degree transformation
reg_poly<-lm(WG ~ CCON+I(CCON^2))
summary(reg_poly)
pred_val3<-predict(reg_poly)
pred_val3
confint(reg_poly, level=0.95)
predict(reg_poly, interval="predict")
ggplot(data=data, aes(x=CCON+I(CCON^2), y=WG))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=CCON+I(CCON^2), y=pred_val3))
#multiple R-squared value is 0.9521

#polynomial of three degree transformation
reg_poly1<-lm(WG ~ CCON+I(CCON^2)+I(CCON^3))
summary(reg_poly1)
pred_val4<-predict(reg_poly1)
pred_val4
confint(reg_poly1, level=0.95)
predict(reg_poly1, interval="predict")
rmse<-sqrt(mean(reg_poly1$residuals^2))
rmse
ggplot(data=data, aes(x=CCON + I(CCON^2) + I(CCON^3), y=WG))+geom_point(color="blue")+geom_line(color="red", data=data, aes(x=CCON + I(CCON^2) + I(CCON^3), y=pred_val4))
# p values<0.05, multiple R-squared value is 0.9811 and adjusted R-squared value is 0.9755

#polynomial of Three degree gives the best R-squared values
predicted_wg_val<-predict(reg_poly1)
predicted_wg_val
par(mfrow=c(2,2))
plot(reg_poly1)


