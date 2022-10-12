#############################################
#### Regression : Hostels in Japan dataset 
#############################################


#----------------------------------------
# 1 - Import and clean the data 
#----------------------------------------

# import data
regression_data <- read.csv2('BDD-Hostels-Japon.csv', dec = ".", stringsAsFactors = TRUE)

# check for data structure 
str(regression_data)

# change the strucutre
regression_data$hostel.name <- as.character(regression_data$hostel.name)
class(regression_data$hostel.name)

# outliers

## for qualitative variables

table(regression_data$City)
prop.table(table(regression_data$City))

barplot(table(regression_data$City))

table(regression_data$rating.band)
# the category "rating" doesn't have any signification and it's concern only 8 observations so we will remove it 

regression_data2 <- regression_data[!(regression_data$rating.band=="Rating"),]


table(regression_data2$rating.band)

#order the categories 
regression_data2$rating.band <- factor(regression_data2$rating.band, order=T, levels = c("Fabulous", "Superb", "Very Good", "Good"))
table(regression_data2$rating.band)

prop.table(table(regression_data2$rating.band))
barplot(table(regression_data2$rating.band))

## for quantitative variables
#Histograms
hist(regression_data2$price.from, main = "Price") 
hist(regression_data2$Distance..km.from.city.centre., main = "Distance from the center")
hist(regression_data2$summary.score, main = "Summary score")
hist(regression_data2$atmosphere, main = "Atmosphere")
hist(regression_data2$cleanliness, main = "Cleanliness")
hist(regression_data2$facilities, main = "Facilities")
hist(regression_data2$location.y, main = "Location")
hist(regression_data2$security, main = "Security")
hist(regression_data2$staff, main = "Staff")
hist(regression_data2$valueformoney, main = "Value for money")

#Boxplots
boxplot(regression_data2$price.from, main = "Price") 
boxplot(regression_data2$Distance..km.from.city.centre., main = "Distance from the center")
boxplot(regression_data2$summary.score, main = "Summary score")
boxplot(regression_data2$atmosphere, main = "Atmosphere")
boxplot(regression_data2$cleanliness, main = "Cleanliness")
boxplot(regression_data2$facilities, main = "Facilities")
boxplot(regression_data2$location.y, main = "Location")
boxplot(regression_data2$security, main = "Security")
boxplot(regression_data2$staff, main = "Staff")
boxplot(regression_data2$valueformoney, main = "Value for money")

#we will remove the outliers for the Price

regression_data2 <- regression_data2[!(regression_data$price.from >= 100000),]

# check for missing values
#remove the NA value for longitude and latitude

summary(regression_data2)
sum(is.na(regression_data2))

library(mice)
md.pattern(regression_data2)

regression_data2.complete <- na.omit(regression_data2)

# Some explorative analysis
## hotel distribution map according to the rating level

install.packages("dplyr")
install.packages("leaflet")

library(dplyr)
library(leaflet)

pal <- colorFactor(c("steelblue","lightgoldenrod","firebrick1","springgreen","palegoldenrod"),domain = c("Superb","Fabulous","Very Good","Good","Rating"))

pop <- paste("Name:",regression_data2.complete$hostel.name,"<br/>",
             "Rating:",regression_data2.complete$rating.band,"<br/>",
             "Distance:",regression_data2.complete$Distance,"<br/>",
             "Price:",regression_data2.complete$price.from,"<br/>")

regression_data2.complete %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(color = pal(regression_data2.complete$rating.band),fillOpacity = 0.8,stroke = FALSE,lng = regression_data2.complete$lon,lat = regression_data2.complete$lat,popup = pop) %>%
  addLegend("bottomright",pal = pal,values = regression_data2.complete$rating.band,opacity = 1,title = "RatingLevel")

#we remove the Ark Hostel observation as it's located in Singapore
regression_data2.complete <- regression_data2.complete[!(regression_data2.complete$hostel.name=="Ark Hostel"), ]

# we will now keep only the useful variables to do regression models
Regression_clean.dataset <- regression_data2.complete[,-1]
Regression_clean.dataset <- Regression_clean.dataset[,-1]
Regression_clean.dataset <- Regression_clean.dataset[,-5]


#Scatter plot matrix - correlation between variables. 

#install.packages('datarium')
#install.packages('lattice')

library(datarium)
library(lattice)

splom(~Regression_clean.dataset[c(1:11)], groups=NULL,data=Regression_clean.dataset,axis.line.tck=0,axis.text.alpha=0)
cor(Regression_clean.dataset[,c(2:11)]) 
pairs(Regression_clean.dataset[,c(2:11)])


# If the score is bigger than 0,8, it means that the variables are strongly correlated. 
# We see with this scatter plot that the summary score is correlated with criterias as facilities and cleanliness
# It's normal as this score is based on these criterias
# Strong correlation between summary score and facilities and between summary score and value for money
# we also see not evident correlation between the city and the price and between the summary score and the price
# We choose to remove cleaniless which is the less correlated with the summary score. 


Regression_clean.dataset <- Regression_clean.dataset[,-6] # We remove cleanliness. 



#----------------------------------------
# 2 - Model estimation and stepwise selection 
#----------------------------------------

attach(Regression_clean.dataset) # To avoid writting "Regression_clean.dataset$...". 

# The dataset is divided in two: a training set and a test set
Regression_training<-Regression_clean.dataset[1:(dim(Regression_clean.dataset)[1]-60),]
Regression_test<-Regression_clean.dataset[(dim(Regression_clean.dataset)[1]-60+1):dim(Regression_clean.dataset)[1],]

lm.fit = lm(summary.score~., data=Regression_training) # First estimation of the regression model with 12 explicative variables.  
summary(lm.fit)
# Multiple R-squared:  0.9827,	Adjusted R-squared:  0.9818  ->  very good model, who fits very good with the data. 
#Predictors : by looking at the pvalue, not all are significant because their pvalue > 5% (need to remove them). 
#The non-significant predictors are : city, distance, lon and lat. 


#Variables stepwise selection 
#----

# Firstly, we remove the least significant variable (with the largest pvalue) -> lon. 
lm.fit1 = update(lm.fit,~.-lon)
summary(lm.fit1)
# There are still non-significant varaibles : city, distance and lat. 


# 2 - we remove distance which is now the most unsignificant.
lm.fit2 = update(lm.fit1, ~.-Distance..km.from.city.centre.)
summary(lm.fit2)
# There are still non-significant varaibles : city and lat. 

# 3 - we remove city which is now the most unsignificant.
lm.fit3 = update(lm.fit2, ~.-City)
summary(lm.fit3)
# There is still non-significant varaibles : lat. 

# 4 - we remove lat which is now the most unsignificant.
lm.fit4 = update(lm.fit3, ~.-lat)
summary(lm.fit4)
# All the variables are now significant. 


#----------------------------------------
# 3 - Model validation
#----------------------------------------

#Confidence Intervals on the Parameters
confint(lm.fit4, level = .95)

# Evaluating the Residuals: centered on zero with a constant variance
with(lm.fit4, {plot(fitted.values,residuals,ylim=c(-40,40)) 
  points(c(min(fitted.values),max(fitted.values)), c(0,0), type="l")})

# Evaluating the Normality Assumption of residuals
residuals <- lm.fit4$residuals
hist(residuals, main="")

# Evaluating the Normality Assumption of residuals
qqnorm(residuals, ylab="Residuals", main="Are the residuals normality assumption ?")
qqline(residuals)

# Verification de l'homoscedasticité et de la non-corrélation des résidus
plot(residuals)
plot(residuals, Regression_training$facilities)
plot(residuals, Regression_training$valueformoney)
plot(residuals, Regression_training$staff)
plot(residuals, Regression_training$atmosphere)
plot(residuals, Regression_training$location.y)
plot(residuals, Regression_training$security)
plot(residuals, Regression_training$price.from)

#Estimation de la puissance de prédiction du modèle
cor(lm.fit4$fitted.values, Regression_training$summary.score)

#Représentation des valeurs prédites et des valeurs actuelles
plot(Regression_training$summary.score, lm.fit4$fitted.values)


#Classification Rate: how well the model does in predicting the dependent variable on out-of-sample observations?
library("caret")

## Loading required package: ggplot2

varImp(lm.fit4)

score.predict <- predict(lm.fit4, newdata = Regression_test, type = "response")
head(score.predict)
summary(score.predict)

# Probability of right prediction
Rate_error <- mean(score.predict != Regression_test$summary.score)

# Probability of score higher than 8/10
score.predict <- ifelse(score.predict>8.8,1,0)
table(score.predict)

# Analysis of the model
score.predict <- factor(score.predict, levels = c(0,1), labels = c("predicted low score", "predicted high score"))
Regression_test$summary.score <- ifelse(Regression_test$summary.score>8.8,1,0)
Regression_test$summary.score <- factor(Regression_test$summary.score, levels = c(0,1), labels = c("low score", "high score"))
prop.table(table(score.predict, Regression_test$summary.score),2)

# The results show 95% of the predicted observations are true negatives and about 91% are true positives; 
# Type II error is 9%: in those cases, the models predicts hostels will have a high score but have a bad one.
# Type I error is 2%: in those cases, the models predicts hostels will have a bad score but have a great score.

