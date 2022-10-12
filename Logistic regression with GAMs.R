###############################
# Logistic regression with GAMs
###############################

install.packages("ISLR2")

library("ISLR2")
data = Wage
View(data)

#################
# Data structure
#################

attach(data)

par(mfrow=c(1,1))
hist(wage)
median(wage)

# Create 0/1 variables 
highwage = factor(ifelse(wage>105,1,0))
data.lr = data.frame(data, highwage)
View(data.lr)
str(data.lr)
summary(data.lr)


# Descriptive statistics
plot(highwage, age)
table(highwage, education)

# Use of restricted sample 
data.res = data.lr[(education!="1. < HS Grad"),]

################################
# Classical logistic regression
################################

#------------------
# Model estimation
#------------------

mod.1 = glm(highwage~age+education+year, family=binomial, data=data)
summary(mod.1)


#----------------
# Model accuracy
#----------------

# Training set and test set 
set.seed(1)
row.number = sample(1:nrow(data.lr), 0.8*nrow(data.lr))
train = data.lr[row.number,]
test=data.lr[-row.number,]
dim(train)
dim(test)

# Model evaluation on the training set 
mod.train = glm(highwage~age+education+year, family=binomial, data=train)

# Prediction on the test set 
pred.hw = predict(mod.train, newdata=test, type="response")
head(pred.hw)

# Prediction accuracy 
pred.hw = ifelse(pred.hw>0.5,1,0) # -> si la prédisction est > à 0.5 alors valeur =1, sinon 0. 
# Replace probabilitues by 0/1 coding

misclass.err = mean(pred.hw != test$highwage)
misclass.err 
print(paste("Prediction accuracy =", 1-misclass.err))

Pred.hw = factor(pred.hw, levels=c(0,1), labels=c("Predicted low wage", "Predicted high wage"))
test$highwage = factor(test$highwage, levels=c(0,1), labels=c("Low wage", "High wage"))
View(test)

table(pred.hw, test$highwage)
prop.table(table(pred.hw, test$highwage), 2) #2 pour sommer les pourcentages sur high et low en colonnes et non en lignes.  
# Almost 80% of the low wages have been corrected predicted, 
# and 65% of th ehigh wages have been correctly predicted. 

# This average level of accuracy for the high wage category might come from the choice of the breakpoint median (wage). 
# Another option would me to select a highervalue in order to create a more homogeneous sub-sample. 



##########################
# GAM logistic regression
##########################

library("splines") #to use bs or ns functions
library("gam") #to use gam.plot or gam function
View(data.res)

# Natural splines
mod.gam <- glm(highwage~ns(age,knots=c(30,45,60))+ns(year,df=5)+education, family=binomial,data=data.res)
par(mfrow=c(1,1))
plot.Gam(mod.gam, se=TRUE, col="red")

# Smoothing splines
mod.gam2 <- gam(highwage~s(age,4)+s(year, 5)+education, family=binomial, data=data.res)
par(mfrow=c(1,1))
plot.Gam(mod.gam2, se=TRUE, col="red")

#--------------------------------
# Prediction accuracy for mod.gam
#--------------------------------
mod.gamt <- glm(highwage~ns(age,knots=c(30,45,60))+ns(year,df=5)+education, family=binomial,data=train)


# Prediction on the test set
pred.hw <- predict(mod.gamt, newdata=test, type='response')
head(pred.hw)

# Prediction accuracy
pred.hw <- ifelse(pred.hw>0.5,1,0)
# Replace probabilities by 0/1 coding
head(pred.hw)
View(test)

test$highwage <- ifelse(test$highwage=="High wage",1,0)

View(test)
misclass.err <- mean(pred.hw != test$highwage)
# != stands for not equal to
misclass.err
print(paste('Prediction accuracy =', 1-misclass.err))

pred.hw <- factor(pred.hw, levels=c(0,1), labels=c("Predicted low wage", "Predicted high wage"))
test$highwage <- factor(test$highwage, levels=c(0,1), labels=c("Low wage","High wage" ))
View(test)

table(pred.hw, test$highwage)
prop.table(table(pred.hw, test$highwage),2)
