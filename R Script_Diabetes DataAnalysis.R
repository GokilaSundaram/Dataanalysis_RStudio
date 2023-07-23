setwd("C:\\Term2")

diabetes <- read.csv("diabetes.csv", head=TRUE)
##############################################
#explore the given dataset
dim(diabetes) # Number of rows and columns
str (diabetes) # Variable data types
View(diabetes) # View the data Set
attach(diabetes)
detach(diabetes)

######################################################
#clean data#

library(dplyr)
library(tidyverse)
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)

###Replace zero with NaN
diabetes$Glucose[diabetes$Glucose==0] <- NaN
diabetes$BloodPressure[diabetes$BloodPressure==0] <- NaN
diabetes$BMI[diabetes$BMI==0] <- NaN
diabetes$Insulin[diabetes$Insulin==0] <- NaN
diabetes$SkinThickness[diabetes$SkinThickness==0] <- NaN
diabetes$Age[diabetes$Age==0] <- NaN

#look for duplicates#
Duplicates<- diabetes[duplicated(diabetes)]
View(Duplicates) # there are no duplicates
##Missing values in the dataset
missing_pattern<- md.pattern(diabetes,rotate.names = TRUE)
View(missing_pattern)

##visualize distributions of variables##

summary(diabetes)
install.packages("psych")
library(psych)
describe(diabetes)
library(lattice)

par(mfrow = c(2,4))

##qq Plot
qqnorm(Glucose,main="QQ Plot_Glucose") #linear line shows data is normally distributed
qqline(Glucose,col="red")
qqnorm(BloodPressure,main="QQ Plot_BloodPressure")
qqline(BloodPressure,col="red") # normal distribution with outliers
qqnorm(SkinThickness,main="QQ Plot_SkinThickness") # Not  a normal distribution
qqline(SkinThickness,col="red")
qqnorm(Age,main="QQ Plot_Age") # Not  a normal distribution wit Outliers
qqline(Age,col="red")
qqnorm(Insulin,main="QQ Plot_Insulin")# not a normal distribution
qqline(Insulin,col="red") 
qqnorm(BMI,main="QQ Plot_BMI") # normal distribution
qqline(BMI,col="red")
qqnorm(DiabetesPedigreeFunction,main="QQ Plot_DiabetesPedigreeFunction") # not a normal distribution
qqline(DiabetesPedigreeFunction,col="red")
qqnorm(Pregnancies,main="QQ Plot_Pregnancies") 
qqline(Pregnancies,col="red")

## Boxplot
boxplot(Glucose,main="Boxplot_Glucose",ylab="Glucose(mg/dL)") # normal distribution
boxplot(BloodPressure,main="Boxplot_BloodPressure",ylab="BP(mmHg)") # Normal with extremal outliers
boxplot(SkinThickness,main="Boxplot_SkinThickness",ylab="SkinThickness(mm)") # not a normal distribution
boxplot(DiabetesPedigreeFunction,main="Boxplot_DiabetesPedigreeFunction",ylab="DiabetesPedigreeFunction")# not normal distribution
boxplot(BMI,main="Boxplot_BMI",ylab="BMI")
boxplot(Age,main="Boxplot_Age",ylab="Age(years)")#not normal 
boxplot(Insulin,main="Boxplot_Insulin",ylab="Insulin")# not normal
boxplot(Pregnancies,main="Boxplot_Pregnancies",ylab="number of Pregnancies")

#Remove na values to draw bell curve in Histogram.
diabetes2<- na.omit(diabetes)
attach(diabetes2)

hist(Pregnancies, main="Histogram of Pregnancies", xlab="Number of Pregnancies", col="orange", probability = TRUE)
lines(density(Pregnancies))
lines(density(diabetes2$Glucose))
hist(diabetes2$Glucose, main="Histogram of Glucose", xlab="Glucose (mg/dL)", col="pink", probability = TRUE)
hist(BloodPressure, main="Histogram of BloodPressure", xlab="BloodPressure(mmHg)", col="turquoise", probability = TRUE)
lines(density(BloodPressure))
hist(Age, main="Histogram of Age", xlab="Age(years)", col="tan", probability = TRUE)
lines(density(Age))
hist(SkinThickness, main="Histogram of SkinThickness", xlab="SkinThickness(mm)", col="thistle", probability = TRUE)
lines(density(SkinThickness))
hist(BMI, main="Histogram of BMI", xlab="Body Mass Index", col="slategray", probability = TRUE)
lines(density(BMI))
hist(DiabetesPedigreeFunction, main="Histogram of DiabetesPedigreeFunction", xlab="DiabetesPedigreeFunction", col="sienna3", probability = TRUE)
lines(density(DiabetesPedigreeFunction))
hist(Insulin, main="Histogram of Insulin", xlab="Insulin Concentration", col="salmon", probability = TRUE)
lines(density(Insulin))

# Run ks test to chck the normality of the data.

ks.test(Glucose, "pnorm",mean=mean(Glucose,na.rm=TRUE), sd=sd(Glucose,na.rm=TRUE))
ks.test(SkinThickness, "pnorm",mean=mean(SkinThickness,na.rm=TRUE), sd=sd(SkinThickness,na.rm=TRUE)) 
ks.test(BloodPressure, "pnorm",mean=mean(BloodPressure,na.rm=TRUE), sd=sd(BloodPressure,na.rm=TRUE)) 
ks.test(Age, "pnorm",mean=mean(Age,na.rm=TRUE), sd=sd(Age,na.rm=TRUE)) 
ks.test(Insulin, "pnorm",mean=mean(Insulin,na.rm=TRUE), sd=sd(Insulin,na.rm=TRUE)) 
ks.test(DiabetesPedigreeFunction, "pnorm",mean=mean(DiabetesPedigreeFunction,na.rm=TRUE), sd=sd(DiabetesPedigreeFunction,na.rm=TRUE)) 
ks.test(BMI, "pnorm",mean=mean(BMI,na.rm=TRUE), sd=sd(BMI,na.rm=TRUE)) 
ks.test(Pregnancies, "pnorm",mean=mean(Pregnancies,na.rm=TRUE), sd=sd(Pregnancies,na.rm=TRUE)) 


##################################################################################################
###Question d: differences in central tendencies
###Sample Groups 1:  with Missing Values
true_diabetes<- subset(diabetes, diabetes$Outcome == 1)
false_diabetes<- subset(diabetes,diabetes$Outcome == 0)
View(true_diabetes)
View(false_diabetes)

## Sample Groups 2: with median Imputation
####Replace missing values with median
ImputedData<- data.frame(diabetes)
View(ImputedData)

ImputedData$Glucose[which(is.na(ImputedData$Glucose))] <- median(ImputedData$Glucose,na.rm=TRUE)
ImputedData$BloodPressure[which(is.na(ImputedData$BloodPressure))] <- median(ImputedData$BloodPressure,na.rm=TRUE)
ImputedData$SkinThickness[which(is.na(ImputedData$SkinThickness))]<- median(ImputedData$SkinThickness,na.rm=TRUE)
ImputedData$BMI[which(is.na(ImputedData$BMI))]<- median(ImputedData$BMI,na.rm=TRUE)
ImputedData$Insulin[which(is.na(ImputedData$Insulin))] <- median(ImputedData$Insulin,na.rm=TRUE)
ImputedData$Age[which(is.na(ImputedData$Age))]<- median(ImputedData$Age,na.rm=TRUE)

attach(ImputedData)
View(diabetes)
summary(ImputedData)
##########
ImputedData_true<- subset(ImputedData, ImputedData$Outcome==1)
ImputedData_false<- subset(ImputedData, ImputedData$Outcome==0)

##########################################################
##### Mann Whitney U Test on the sample groups###########
wilcox.test(true_diabetes$Glucose, false_diabetes$Glucose,paired=FALSE)## p-value < 2.2e-16
wilcox.test(ImputedData_true$Glucose, ImputedData_false$Glucose,paired=FALSE)##p-value < 2.2e-16


#One sided test
wilcox.test(false_diabetes$Glucose,true_diabetes$Glucose,alternative = "greater",paired=FALSE)# 
wilcox.test(false_diabetes$Glucose,true_diabetes$Glucose, alternative = "less",paired=FALSE) ##p-value < 2.2e-16
#################################################
# Blood Pressure

wilcox.test(true_diabetes$BloodPressure,false_diabetes$BloodPressure,paired=FALSE) ##p-value = 1.629e-06
wilcox.test(ImputedData_true$BloodPressure, ImputedData_false$BloodPressure,paired=FALSE) ## p-value = 2.234e-06

#One sided test
wilcox.test(false_diabetes$BloodPressure,true_diabetes$BloodPressure,alternative = "greater",paired=FALSE)# 
wilcox.test(false_diabetes$BloodPressure,true_diabetes$BloodPressure, alternative = "less",paired=FALSE) ##p-value = 8.143e-07

##########################################################################

#Man WHitney test for Skinthickness
wilcox.test(true_diabetes$SkinThickness,false_diabetes$SkinThickness,paired=FALSE) ##p-value = 6.961e-10
wilcox.test(ImputedData_true$SkinThickness, ImputedData_false$SkinThickness,paired=FALSE) ## p-value =  2.32e-09
#One sided test
wilcox.test(false_diabetes$SkinThickness,true_diabetes$SkinThickness,alternative = "greater",paired=FALSE)# 
wilcox.test(false_diabetes$SkinThickness,true_diabetes$SkinThickness, alternative = "less",paired=FALSE) ##p-value = 3.481e-10

 ########################################################
#Mann Whitney U test_BMI
t.test(true_diabetes$BMI,false_diabetes$BMI,paired=FALSE) #p-value < 2.2e-16
wilcox.test(true_diabetes$BMI,false_diabetes$BMI,paired=FALSE) #  p-value < 2.2e-16
wilcox.test(ImputedData_true$BMI, ImputedData_false$BMI,paired=FALSE) # p-value < 2.2e-16

wilcox.test(false_diabetes$BMI,true_diabetes$BMI,paired=FALSE, alternative = "less") #  p-value < 2.2e-16

######################################################################################
#Mann Whitney U test_Age
wilcox.test(true_diabetes$Age,false_diabetes$Age,paired=FALSE) # p-value < 2.2e-16.
wilcox.test(ImputedData_true$Age, ImputedData_false$Age,paired=FALSE) # p-value < 2.2e-16

wilcox.test(false_diabetes$Age,true_diabetes$Age,paired=FALSE, alternative = "less") # p-value < 2.2e-16
#######################################################################################
#Mann Whitney U test DiabetesPedigreeFunction
wilcox.test(ImputedData_true$DiabetesPedigreeFunction, ImputedData_false$DiabetesPedigreeFunction,paired=FALSE)# p-value = 1.197e-06
wilcox.test(true_diabetes$DiabetesPedigreeFunction,false_diabetes$DiabetesPedigreeFunction,paired=FALSE) # p-value = 1.197e-06
wilcox.test(false_diabetes$DiabetesPedigreeFunction,true_diabetes$DiabetesPedigreeFunction,paired=FALSE, alternative = "less") # p-value = 5.983e-07
wilcox.test(false_diabetes$DiabetesPedigreeFunction,true_diabetes$DiabetesPedigreeFunction,paired=FALSE, alternative = "greater")

#################################################
#Mann Whitney U test Insulin
wilcox.test(ImputedData_true$Insulin, ImputedData_false$Insulin,paired=FALSE)#p-value = 2.961e-14
wilcox.test(true_diabetes$Insulin,false_diabetes$Insulin,paired=FALSE)# p-value = 7.477e-14
wilcox.test(true_diabetes$Insulin,false_diabetes$Insulin,paired=FALSE,alternative = "less")
wilcox.test(true_diabetes$Insulin,false_diabetes$Insulin,paired=FALSE,alternative = "greater")##p-value = 3.738e-14
View(true_diabetes)
####################################################################
#Mann Whitney U test Pregnancies
wilcox.test(ImputedData_true$Pregnancies, ImputedData_false$Pregnancies,paired=FALSE) #p-value = 3.745e-08
wilcox.test(true_diabetes$Pregnancies,false_diabetes$Pregnancies,paired=FALSE)# p-value = 3.745e-08
wilcox.test(true_diabetes$Pregnancies,false_diabetes$Pregnancies,paired=FALSE,alternative = "less")# 
wilcox.test(true_diabetes$Pregnancies,false_diabetes$Pregnancies,paired=FALSE,alternative = "greater")##p-value = 1.873e-08


#######################################################
##To conduct Cohen's d test in R studio# 
install.packages("effsize")
library(effsize)

#Cohens  test, which measures the which measures the standardised difference between the means of two distributions.
cohen.d(true_diabetes$Insulin,false_diabetes$Insulin,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE)#estimate:g estimate: 0.7456093
cohen.d(true_diabetes$Glucose,false_diabetes$Glucose,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE)#estimate 1.277175 (large)
cohen.d(true_diabetes$Age,false_diabetes$Age,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE)#g estimate: 0.5032209 (medium)
cohen.d(true_diabetes$Pregnancies,false_diabetes$Pregnancies,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)#g estimate: 0.5190721(medium)
cohen.d(true_diabetes$SkinThickness,false_diabetes$SkinThickness,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE) ## 0.5741309 (medium)
cohen.d(true_diabetes$BloodPressure,false_diabetes$BloodPressure,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE)##0.3650561 (small)
cohen.d(true_diabetes$BMI,false_diabetes$BMI,pooled=FALSE,paired=FALSE,hedges.correction = TRUE,na.rm=TRUE)## 0.6923878 (medium)
cohen.d(true_diabetes$DiabetesPedigreeFunction,false_diabetes$DiabetesPedigreeFunction,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)##g estimate: 0.403389 (small)


#cohen s test with imputed sample  groups
cohen.d(ImputedData_true$Insulin,ImputedData_false$Insulin,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)# estimate 0.4952934 (small)
cohen.d(ImputedData_true$Glucose,ImputedData_false$Glucose,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)#estimate 1.271605 (large)
cohen.d(ImputedData_true$SkinThickness,ImputedData_false$SkinThickness,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)##0.46261 (small)
cohen.d(ImputedData_true$BloodPressure,ImputedData_false$BloodPressure,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)##0.3519895 (small) (small)
cohen.d(ImputedData_true$BMI,ImputedData_false$BMI,pooled=FALSE,paired=FALSE,hedges.correction = TRUE)##0.6908807 (medium)
##Visualize differences in central tendency
library(lattice)
par(mfrow = c(1,1))
boxplot(true_diabetes$Insulin,false_diabetes$Insulin,main="Median Diff in Insulin",ylab="Insulin")
boxplot(true_diabetes$Glucose,false_diabetes$Glucose,main="Median Diff in Glucose",ylab="Glucose")
boxplot(true_diabetes$Pregnancies,false_diabetes$Pregnancies,main="Median Diff in Pregnancies",ylab="Pregnancies")
boxplot(true_diabetes$Age,false_diabetes$Age,main="Median Diff in Age",ylab="Age")
boxplot(true_diabetes$SkinThickness,false_diabetes$SkinThickness,main="Median Diff in SkinThickness",ylab="SkinThickness")
boxplot(true_diabetes$BMI,false_diabetes$BMI,main="Median Diff in BMI",ylab="BMI")
boxplot(true_diabetes$BloodPressure,false_diabetes$BloodPressure,main="Median Diff in BloodPressure",ylab="BloodPressure")
boxplot(true_diabetes$DiabetesPedigreeFunction,false_diabetes$DiabetesPedigreeFunction,main="Median Diff in DiabetesPedigreeFunction",ylab="DiabetesPedigreeFunction")


####################################################################


#question e
#correlation between variables quick view####
##Complete cases omit all missing observations

View(diabetes)
completecases <- na.omit(diabetes)
attach(completecases)
completecases_true<-subset(completecases, completecases$Outcome == 1)
completecases_false<-subset(completecases, completecases$Outcome == 0)

####Cor test on Complete cases##########
plot(completecases_new) 
completecases_new<- completecases[,c(-9)] # exclude Outcome variable
corvalues<- cor(completecases_new,method = "spearman")
pairs.panels(corvalues)

corrplot(corvalues,method="number")

###Check confidence interval of correlation test

install.packages("confintr")
library(confintr)
ci_cor(completecases[2:3], method="spearman",type = "bootstrap")

###correlation with median imputed data####
plot(ImputedData_new) 
ImputedData_new<- ImputedData[,c(-9)] # exclude Outcome variable
corvalues_imp<- cor(ImputedData_new,method = "spearman")
pairs.panels(corvalues)

corrplot(corvalues_imp,method="number")
####################################################################
### Regression Model to find the variable which influence diabetes outcome####
##Approach 1: Dataset with NaN values
##linear regression model##
summary(lm(Outcome ~ . , data= diabetes)) #### against all variables

summary(lm(Outcome ~ Glucose , data= diabetes))
summary(lm(Outcome ~ BMI , data= diabetes))
summary(lm(Outcome ~ DiabetesPedigreeFunction , data= diabetes))
summary(lm(Outcome ~ Pregnancies , data= diabetes))
summary(lm(Outcome ~ Age , data= diabetes))
summary(lm(Outcome ~ SkinThickness , data= diabetes))
summary(lm(Outcome ~ Insulin , data= diabetes))
summary(lm(Outcome ~ BloodPressure , data= diabetes))

View(diabetes)

###Model 2: Logistic Regression Model with NaN Values
summary(glm(Outcome ~ . , data= diabetes)) #### against all variables

summary(glm(Outcome ~ Glucose , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ BMI , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ DiabetesPedigreeFunction , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ Pregnancies , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ Age , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ Insulin , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ SkinThickness , family = binomial(link = "logit"), data = diabetes))
summary(glm(Outcome ~ BloodPressure , family = binomial(link = "logit"), data = diabetes))
###############################################
##Approach 2: Dataset with Imputed values
install.packages("mice")
library(mice)
methods(mice)
####Imputation using Mice####
regressionimpute <- mice(diabetes, m = 1, method = "norm.predict")
regressionimputed_diabetes<-complete(regressionimpute)
View(regressionimputed_diabetes)

cor(regressionimputed_diabetes)
##Regression model analysis with imputed dataset
model<- summary(glm(regressionimputed_diabetes$Outcome ~ . , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Glucose , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Pregnancies , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$BMI, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Age, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Insulin, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$SkinThickness, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$BloodPressure, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(glm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$BMI, family = binomial(link = "logit"), data = regressionimputed_diabetes))



summary(lm(regressionimputed_diabetes$Outcome ~ . , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Glucose , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Pregnancies , family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$BMI, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$DiabetesPedigreeFunction, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$Insulin, family = binomial(link = "logit"), data = regressionimputed_diabetes))
summary(lm(regressionimputed_diabetes$Outcome ~ regressionimputed_diabetes$BloodPressure, family = binomial(link = "logit"), data = regressionimputed_diabetes))
##Linear Regression plot for all predictor variables
attach(regressionimputed_diabetes)
par(mfrow = c(2,4))
 plot(Outcome ~ Pregnancies, data = regressionimputed_diabetes, main = "Outcome by Pregnancies",
      xlab = "Pregnancies", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ Pregnancies, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for Glucose
 plot(Outcome ~ Glucose, data = regressionimputed_diabetes, main = "Outcome by Glucose",
      xlab = "Glucose", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ Glucose, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for BMI
 plot(Outcome ~ BMI, data = regressionimputed_diabetes, main = "Outcome by BMI",
      xlab = "BMI", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ BMI, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for Diabetespedigreefunction
 plot(Outcome ~ DiabetesPedigreeFunction, data = regressionimputed_diabetes, main = "Outcome by DiabetesPedigreeFunction",
      xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ DiabetesPedigreeFunction, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for Age
 plot(Outcome ~ Age, data = regressionimputed_diabetes, main = "Outcome by Age",
      xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ Age, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for SkinThickness
 plot(Outcome ~ SkinThickness, data = regressionimputed_diabetes, main = "Outcome by SkinThickness",
      xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ SkinThickness, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for Bloodpressure
 plot(Outcome ~ BloodPressure, data = regressionimputed_diabetes, main = "Outcome by Bloodpressure",
      xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ BloodPressure, data = regressionimputed_diabetes, col = "red"))
 
 ##Linear Regression plot for Bloodpressure
 plot(Outcome ~ Insulin, data = regressionimputed_diabetes, main = "Outcome by Insulin",
      xlab = "Insulin", ylab = "Probability of Outcome")
 abline(lm(Outcome ~ Insulin, data = regressionimputed_diabetes, col = "red"))
 
 
 
 ################################################################
 attach(regressionimputed_diabetes)
##Logistic Regression Model plots for all predictor variables
plot(Outcome ~ Glucose, data = regressionimputed_diabetes, main = "Outcome by Glucose",
     xlab = "Glucose", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ Glucose , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(Glucose,probability),col="red",lwd=2)

plot(Outcome ~ BMI, data = regressionimputed_diabetes, main = "Outcome by BMI",
     xlab = "BMI", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ BMI , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(BMI,probability),col="red",lwd=2)

plot(Outcome ~ DiabetesPedigreeFunction, data = regressionimputed_diabetes, main = "Outcome by DiabetesPedigreeFunction",
     xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ DiabetesPedigreeFunction , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(DiabetesPedigreeFunction,probability),col="red",lwd=2)

plot(Outcome ~ Pregnancies, data = regressionimputed_diabetes, main = "Outcome by Pregnancies",
     xlab = "Pregnancies", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ Pregnancies , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(Pregnancies,probability),col="red",lwd=2)

plot(Outcome ~ Age, data = regressionimputed_diabetes, main = "Outcome by Age",
     xlab = "Age", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ Age , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(Age,probability),col="red",lwd=2)

plot(Outcome ~ Insulin, data = regressionimputed_diabetes, main = "Outcome by Insulin",
     xlab = "Insulin", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ Insulin , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(Insulin,probability),col="red",lwd=2)

plot(Outcome ~ BloodPressure, data = regressionimputed_diabetes, main = "Outcome by BloodPressure",
     xlab = "BloodPressure", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ BloodPressure , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(BloodPressure,probability),col="red",lwd=2)

plot(Outcome ~ DiabetesPedigreeFunction, data = regressionimputed_diabetes, main = "Outcome by DiabetesPedigreeFunction",
     xlab = "DiabetesPedigreeFunction", ylab = "Probability of Outcome", ylim=c(0,1),las=1)
model2 <- glm(Outcome ~ DiabetesPedigreeFunction , family = binomial(link = "logit"), data = regressionimputed_diabetes)
probability<- predict.glm(model2, type=c("response"),data = regressionimputed_diabetes)
lines(smooth.spline(DiabetesPedigreeFunction,probability),col="red",lwd=2)

##################################################################

###g) Logistic Regression Model to predict missing values in Glucose
# Create 2 groups, one with Missing values in Glucose and other one without Missing Values###
Missingdata_Glucose <- subset(diabetes, is.na(Glucose)) #5 Rows
Completedata_Glucose<- subset(diabetes, !is.na(Glucose)) #763 rows

View(Missingdata_Glucose)
View(Completedata_Glucose)

# Use lm function to predict the glucose values
plot( Age ~ Glucose, data = diabetes, main = "Glucose by Age",
      xlab = "Age", ylab = "Glucose")
abline(lm(Age ~ Glucose, data = diabetes))

predictglucose <- lm(Glucose ~ Age, data=Completedata_Glucose )

summary(predictglucose)
##Confidence interval on predictions
confint(predictglucose,level=0.70)

## store predicted values
Predictedvalue<- predict(predictglucose, newdata=as.data.frame(Missingdata_Glucose))

##Replace missing values with predicted values
Missingdata_Glucose$Glucose <- round(Predictedvalue)

#Both DF can be merged as follows
ImputedGlucose<- merge(Missingdata_Glucose,Completedata_Glucose, all=TRUE)
View(ImputedGlucose)
## Accuracy of results 
mean(Predictedvalue)  ##118.4499
mean(diabetes$Glucose,na.rm=TRUE) ## mean of original dataset 121.6868
mean(ImputedGlucose$Glucose)##mean of imputed dataset 121.6654



###############################################


