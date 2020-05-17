 title: "Hypothesis testing"

  

library(BSDA)
library(nortest)
library(readr)

###### test for significance difference in the  diameter of the cutlet between two units
cutlet<-read.csv("E:\\assignments data\\hypothesis testing\\Cutlets.csv")
View(cutlet)
attach(cutlet)
head(cutlet)
str(cutlet)
## normality test
shapiro.test(Unit.A)
shapiro.test(Unit.B)

## p values of unitA and unit B are >0.05, so data is in normal format
## variance test
var.test(Unit.A,Unit.B)
## p>0.05 , unit A and Unit B have equal variances
## two sample t test for equal variances
t.test(Unit.A, Unit.B,mu=0, alternative="two.sided",conf.level=0.95, var.equal=TRUE)
## p>0.05, accept Ho
##there is no significant difference in the diameter of the cutlet between two units

---------------------------------------------------------------------------------------------------------------------------------
###test for finding difference in average TAT among the different laboratories 
labTAT<-read.csv("file:///E:/assignments data/hypothesis testing/LabTAT.csv")
View(labTAT)
attach(labTAT)
str(labTAT)
head(labTAT)
## normality  test
data<-stack(labTAT)
View(data)
shapiro.test(data$values)

## all the variables have p>0.05, so data is normally distributed
## variance test
library(car)
leveneTest(data$values~data$ind, data=labTAT)
### p>0.05, variances are equal
## Anova test
anova_results<-aov(data$values~ind, data=data)
summary(anova_results)
### p<0.05, accept Ha
## there is difference in average TAT among the different laboratories


----------------------------------------------------------------------------------------------------------------------------------
###test for finding if male-female buyer rations are similar across regions
BuyerRatio<-read.csv("E:\\assignments data\\hypothesis testing\\BuyerRatio.csv")
View(BuyerRatio)
str(BuyerRatio)

stacked_data<-stack(BuyerRatio)
View(stacked_data)
table(stacked_data$ind, stacked_data$values)
###chisquare test
chisq.test(table(stacked_data$ind, stacked_data$values))
### p>0.05, accept Ho

------------------------------------------------------------------------------------------------------------------------------
##male-female buyer rations are similar across regions
cof<-read.csv("E:\\assignments data\\hypothesis testing\\Costomer+OrderForm.csv")
View(cof)
attach(cof)
stacked_data<-stack(lapply(cof, as.character))
View(stacked_data)
table(stacked_data$ind, stacked_data$values)
## chisquare test
chisq.test(table(stacked_data$ind, stacked_data$values))
### p>0.05, acceptHo
##4 centers haved equal efective % 


-------------------------------------------------------------------------------------------------------------------------------
###test on finding male versus females walking in to the store differ based on day of the week
data<-read.csv("E:\\assignments data\\hypothesis testing\\Faltoons.csv")
View(data)
table(data)
prop.test(table(data), alternative = "two.sided", conf.level = 0.95, correct = TRUE)

### two sample proportion test
prop.test(table(data), alternative = "two.sided", conf.level = 0.95, correct = TRUE)
### p>0.05, acceptHo
##there is no difference male versus females walking in to the store based on day of the week
```

