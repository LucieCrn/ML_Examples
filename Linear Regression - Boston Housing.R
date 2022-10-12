------------------------------------------------
# Linear regression - Boston Housing 
------------------------------------------------
  

#----------------
# 1 - Import data 
#----------------

install.packages("ISLR2")
library("ISLR2")

?Boston
data = Boston

# Y = medv


#-------------------
# 2 - Data structure
#-------------------
str(data)
#We can see that most of the variables are quantitative. 

# Qualitative variables should be of type factor
data$chas = as.factor(data$chas)
str(data) #To check if it has been sucessfully transform. 


#------------------------------------------
# 3 - Check for outliers and missing values
#------------------------------------------
summary(data)
#No missing values

hist(data$medv)
boxplot(data$medv)
#Heavy tail on the right hand side of the distribution. 

data$logmedv = log(data$medv)
boxplot(data$logmedv)
#Replace a right hand side by a left hand side. 
hist(data$logmedv)
#A little bit better. 
#The logvariable seems closer to a normal distribution.






#-------------------------
# Simple linear regression
#-------------------------

?Boston

#------------------------------
#Link between logmedv and lstat
#------------------------------



#Graph
attach(data) #To avoid reference to dataset (dollar). 
plot(logmedv~lstat)

#Model estimation
lm.fit1 = lm(logmedv~lstat)
summary(lm.fit1)
#Estimate = beta_1 coeeficient. 

#Interpretation : 
#The pvalue is very small (Pr(>|t|), less than 5%). 
#So we can conclude that the coefficient beta_1 is significantly different from 0 and we can interpret its value.
#beta_1 = -0,04 <0 so an increase by 1 unit of lstat impacts negatively logmedv by a decrease of 0,04. 
#R2 = 0,6481 and the adjusted R2 = 0,6474 -> there are very close. This model explains 65% of the log price variations. 

names(lm.fit1) #All the results provided by the function lm. 

plot(lstat, logmedv, pch="+")
abline(lm.fit1, col="red")


#------------------
# Residual analysis
#------------------

#Normality of residuals 
#Qqplot 
qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1))
#Most of th epoints fit on the line which indicates that the residuals distribution is close to a Normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(lm.fit1))
#pvalue = 2.297e-08 <<< 5% so we reject H0. 
#The distribition cannot be considered as normal. 
#There must be potential outliers...
hist(residuals(lm.fit1))


#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(lm.fit1), residuals(lm.fit1))
plot(predict(lm.fit1), rstudent(lm.fit1)) #Divided the residuals by their standard deviation. 







#---------------------------------
#Evaluate the quality of the model
#---------------------------------


#--------------------------
#Fit on the training sample 
#--------------------------
#R2, adjusted R2. 


#--------------
#Validation set
#--------------

#Sample the dataset 
set.seed(1)
row.number = sample(1:nrow(data), 0.8*nrow(data)) #Je prends 80% des valeurs que j'ai choisi de manière aléatoire 
#(entre 1 et la max de valeurs qu'il y a dans l'échantillon). 
train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

#Estimate the linear fit on the training set 
lm.fit0.8 = lm(logmedv~lstat, data=train)
summary(lm.fit0.8)

#Compute RMSE, MAPE
pred0.8 = predict(lm.fit0.8, newdata=test) #data = test to predict
err0.8 = pred0.8-test$logmedv
rmse = sqrt(mean(err0.8^2))    #root mean squared error
mape = mean(abs(err0.8/test$logmedv))
c(RMSE=rmse, mape=mape, R2= summary(lm.fit0.8)$r.squared) #to print the 3 parameters
#R2 allows to evaluate the accuracy of the fit on the training set. 
#RMSE and Mape related to the test test. 
#RMSE is used not in absolute value to compare the accuracy with an other model (with a smallest RMSE) to see wich one is better. 
#MAPE is the relative variation btw the oberved values and the calculated ones (the smallest the better). 

plot(test$logmedv, pred0.8) #plot of predicted values against test values. 


#-------------
#LOOCV method
#-------------
#To evaluate the predictive power of the model. 
#The purpose -> instead of biliding a training set and a test set. 
#We consider that the test sample is the whole sample minus one value as a training. 
#We do that with all the values and give an average of the errors. 

#Load packages 
installed.packages("boot")
library("boot")
glm.fit = glm(logmedv~lstat, data=data) #generalize the linear model, to do again the modelization but with cross-validation.  
#With the GLM function to the LOOCV method but it will be the same model as calcultaed before. 
cv.err = cv.glm(data, glm.fit)
cv.err$delta[1] #to print the cross-validation statistics. 
#The best model is the one with the smallest CV statistics. 

View(data)




#--------------------------------------------
#Application of the predictive accuracy tools
#--------------------------------------------
#2 models : logmedv vs age or lstat. 

#Accuracy on the sample with R2 and adjusted R2. 
lm.fit2 = lm(logmedv~age, data=data)
summary(lm.fit2)
#The fit on the sample is not very good. 
#R2 = 20% (against 65% with the previous model). 

#Validation set method
lm.fit2 = lm(logmedv~age, data=train)
pred2 = predict(lm.fit2, newdata=test)
err2 = pred2 - test$logmedv
rmse = sqrt(mean(err2^2))    #root mean squared error
mape = mean(abs(err2/test$logmedv))
c(RMSE=rmse, mape=mape, R2=summary(lm.fit2)$r.squared) #To print the 3 parameters 

#Results for the second model 
#RMSE > the previous one -> the predictive power with age as predictor is worse. 
#MAPE > the previous one. 

#CV method
glm.fit2 = glm(logmedv~age, data=data)
cv.err2 = cv.glm(data, glm.fit2)
cv.err2$delta[1]

# CV > the previous one : the predictive power of this second model is worse. 




#----------------------------
#Multiple regression setting
#----------------------------

# Comments on the dataset : 
# - there are a lot of predictors. 
# - there is a qualitative variable, read as factor (it means that we can directly include it in the model). 
# - particular attention to multicolinearity. 
# - stepwise variable selection. 

#lm.fit = lm(logmedv~lstat+age, data=data)
lm.fit = lm(logmedv~.-medv, data=data) #The point means that all the remaining variables will be used as predictors. 
#Be careful to remove the variable we want to explain in the predictors list. 
summary(lm.fit)
#contrasts(chas) -> give details on the recoding of the qualitative variables used. 

#Multiple R-squared:  0.9548,	Adjusted R-squared:  0.9536 -> we have a very good model, who fit very good with the data. 
#Predictors : by looking at the pvalue (Pr(>|t|)), not all are significant (need to remove them). 
#The non-significant predictors are : age and indus. 
#First, we remove the one we the largest pvalue (which is the most unsignificant). 
#We do that step by step -> removing one value at each step. 



#Multicolinearity
----
#= too strong link btw predictors. 
#multicolinearity introduces unstability in the model estimation and issues in the interpretation of midel coefficients. 

#Correlation 2 by 2
#cor(data[c(1:3, 5:13)]) or cor(data[-c(4)]) -> to remove the chas1 value which is at column 4 (the one with the highest pvalue). 
#pairs(data[c(1:3, 5:13)])



#Variance inflation factor (VIF)
#----
install.packages("car")
library("car")
vif(lm.fit)
#The larger the VIF is, the more correlated the variables are. 
#VIF provides the link btw the variable we are considering and all the others. 
#Higher than 10 it's too strongly correlated and we need to remove one. 
#Here we can hesitate with "tax" (very close to 10). 
#The most highly correlated predictor is tax,  but the VIF is still less than 10. 
#We can continue with all predictors and apply the stepwise selection. 



#Variables stepwise selection 
#----
#We remove first the least significant variable (with the largest pvalue)
lm.fit1 = update(lm.fit,~.-age)
summary(lm.fit1)
#There are still non-significant varaibles : indus, age, dis, zn and ptratio. 
#Alternative way = equivalent formulation : 
# lm.fit1 = update(lm.fit, ~.-chas)

#We remove indus which is now the most unsignificant.
lm.fit2 = update(lm.fit1, ~.-indus)
summary(lm.fit2)
#All the predictors are finally significant.




#-------------------
#Interaction terms
#-------------------

#Possible interact btw age and lstat. 
#lstat:age includes only the interaction term. 
#lstat*age includes lstat, age and the interaction term. 

summary(lm(logmedv~lstat*age, data=data)) #3 predictors : lstat, age and lstat*age. 



