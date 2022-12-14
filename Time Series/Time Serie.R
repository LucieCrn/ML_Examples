############################
# Toulouse airport traffic
############################


#-------------
# Import data
#-------------

Tlsetraf = read.csv2("TlseAirport2019.csv")
View(Tlsetraf)

traf = ts(Tlsetraf[,3], start=c(1982,1), frequency=12)
# Prendre la troisième colonne du tableau Tlsetraf. 

ltraf=0


#-------------------
# Stationarity issue
#-------------------

# 3 plots : data, acf and pacf. 

# Raw data : 
plot.ts(traf)
# Several patterns of non stationarity : 
# increasing variance, non constant trend, potential seasonal effect. 


# Let's look at the ACF : 
acf(ts(traf, frequency=1), main="Autocorrelogram main series")
# Patterns of non stationarity : 
# there is a persistence of significant coefficients and no fast decay to 0.


# PACF : 
pacf(ts(traf, frequency=1), main="Partial autocorrelogram main series")



# The main series is non stationary, so we will transform it into a stationary one.


#--------
# 1st transformation = log transformation (possible because series is positive). 
# This transformation = to remove for increasing variance effect. 
  
ltraf = log(traf)

# Raw data : 
plot.ts(ltraf)
# No more increasing variance effect, 
# still : non constant trend, potential seasonal effect. 


# ACF : 
acf(ts(ltraf, frequency=1), main="Autocorrelogram log(series)")
# Same patterns of non stationarity : 
# persistence of significant coefficients and no fast decay to 0.


# PACF : 
pacf(ts(ltraf, frequency=1), main="Partial autocorrelogram log(series)")


# The log(series) os still non stationary. 



#--------
# 2nd transformation : 1st order differnce to remove the trend. 
  
dltraf = diff(ltraf, 1)

# Raw data : 
plot.ts(dltraf)
# No more increasing variance and no more trend. 
# Still potential seasonal effects. 

# ACF : 
acf(ts(dltraf, frequency=1), main="Autocorrelogram 1st differnce of log(series)")
# We can visualize the seasonality. 
# Persistence of significant coefficients with seasonality = 12. 


# PACF : 
pacf(ts(dltraf, frequency=1), main="Partial autocorrelogram 1st differnce of log(series)")
# Strong significant coefficients around lag 12. 
# The pattern is repeated on order 12. 


# The series is non stationary. 
# We need to apply a 3rd transformation to remove the seasonal effects. 


#--------
# 3rd transformation : difference of order 12 to remove seasonality. 

dltraf_12 = diff(dltraf, 12)

# Raw data : 
plot.ts(dltraf_12)
# There is no more the regularity of the oscilation = no more seasonal effects. 
# There is a potential outliers, we observe some perturbations of the trafic before 1990.  


# ACF : 
acf(ts(dltraf_12, frequency=1), main="Autocorrelogram 1st differnce of log(series) without seasonality")
# Significant coefficents at lags 1, 12, 13. 


# PACF : 
pacf(ts(dltraf_12, frequency=1), main="Partial autocorrelogram 1st differnce of log(series) without seasonality")
# Significant coefficients at lags 1, 11, 12, 24, 25. 


# We can try and fit a multiplicative SARIMA on log(traffic)



#----------------------------------
# Identification of orders p and q
#----------------------------------

# d=D=1, s=12. 
# p=q=1. 
# P=Q=1 as a starting point. 
# Then try P=Q=2 to identify effects at lag 24. 




#-----------------------------------------------------------------------
# Estimation of multiplicative SARIMA (1,1,1)(1,1,1) with seasonality 12
#-----------------------------------------------------------------------
#multiplicative SARIMA (P,D,Q)(p,d,q)

mod1 = arima(ltraf, c(1,1,1), seasonal=list(order=c(1,1,1), period=12), method="ML")
mod1
# AIC = -1528.81

# Plot of the fitted value : 
install.packages("TSA")
library("TSA")
fit1 = fitted(mod1)
plot.ts(cbind(ltraf, fit1), plot.type="single", col=c('black', 'red'))
# Already not really bad. 




#---------------------
# Validation of mod1
#--------------------- 

#----------------
# Significance of coefficients : pvalue of Student test on each coeff. 

# Student_test : to check for the significance if the coefficients. 
# HO : coeff=0 against H1: coeff≠0. 
# pvalue<5%, we accept H1. 

mod1$coef # Value of coefficients
mod1$var.coef #Variance of coefficients
tstat = mod1$coef/sqrt(diag(mod1$var.coef)) # Test statistic of Student test. 
pvalue = 2*(1-pnorm(abs(tstat))) # Pvalue of the test. 
# La probabilité correspondant à la valeur absolue de tstat. 
tstat
pvalue 
# The pvalue of AR1 and SAR are larger than 5%. 
# So we could remove these coefficients from the model. 


#----------------
# Residuals analysis : assumption od Gaussian white noise

res1 = mod1$residuals 
plot(res1)
# We can identify one extreme value : the residuals are less than -0.2. 

# Autocorrelation of the residuals : 
acf(ts(res1, frequency=1))
# One significant coefficient at lag 6. 
# Et le fait que ce n'est pas le premier coefficient = c'est peut-être pas très sérieux. 


# Box-Pierce test ou Ljung-Bix test :
# H0 : no autocorrelation against H1 : autocorrelation. 

Box.test(res1, lag=20, type=c("Ljung-Box"))
# H0 = all correlations up to lag 20 are 0. 
# H1 = at least one is different from 0. 
# pvalue = 16% > 5%, we accept H0 = no autocorrelation. 
# Not mandatory to add extra coefficients because no correlation already in the residuals. 


# Normal distribition assumption
res_norm = res1/sqrt(mod1$sigma2) # Normalized residuals
summary(res_norm)

# If the residuals follow a Normal distribution, the values of res_norm
# sould lie in btw -2 and 2, with 95% of chance. 

plot(res_norm)
abline(a=2, b=0, col="red")
abline(a=2, b=0, col="red")

out1 = which(res_norm < -4) # Identify the value with the following condition : inférieur à -4. 
# Identification numberber of the outlier (le pic qui descend en-dessous de -4 sur le graph). 
out1 # The outlier corresponds to observation n°50. 

install.packages("zoo")
library("zoo")
index(res_norm)[out1] # To have the date of the outlier. 
# The outlier occurs at date 1986.083 (1 month = 1/12)
traf[out1] # Check at the value of the outlier. 
traf[(out1-2):(out1+2)] # Values around the outlier. 
traf[(out1-14):(out1-10)] # To see values one year before the outlier. 
# Pour améliorer artificiellement le modèle, on peut changer à la main l'outlier
# en faisant un peu comme l'année d'avant (on voit que 144000 et 145000, 
# donc là on peut imaginer 157000 et 155000). 


# Qqplot 
# Check if the points are close to the line. 
qqnorm(res1)
qqline(res1)

# Shapiro test 
shapiro.test(res1)
# pvalue < 5% so we reject the normality assumption. 


#----------------
#Constant variable assumption
sq.res = (res1)^2
acf(ts(sq.res, frequency=1))
# 2 highly significant coeffs : lag 1 and lag 12. 
# There is an issue of non constant variance. 


# TSA package to apply McLeod Li
Htest = McLeod.Li.test(mod1, plot = FALSE)
Htest
# The analysis if the 1st pvalue is enough. 
# pvalue for the first autocorrelation = 1.773821e-05 < 5% so we reject teh constant variance assumption. 





#----------------
# Prediction 
#----------------

# Check of the quality of the fit we respect to confidence bounds. 
#--------

cb80 = mod1$sigma2^0.5*qnorm(0.9) # Pour avoir l'intervalle de confinace à 80%. 
cb80
# mod1$sigma2^0.5 = standard deviation of residuals. 
# qnorm(0.9) comme ça il y a 90% des valeurs en-dessous, 
# et en fait la prof veut un intervalle de confiance à 80% (donc 20% des valeurs exclues).
# Intervalle de confiance = milieu de la courbe de Gauss. 
plot(cbind(ltraf, fit1-cb80, fit1+cb80), plot.type='single', lty=c(1,2,2), xlim=c(2000, 2010))

# Proportion of points in the confidence interval
indi = (ltraf-(fit1-cb80))>0 & (fit1+cb80-ltraf)>0
prop = 100*sum(indi)/length(indi) # Calculer la probabilité que le truc d'avant soit vrai. 
prop # = 85% 
# 85% du temps les valeurs sont dans l'intervalle de confiance. 

# If prop>80%, then the fit is considered good. 




#-------------------------
# Validation set approach 
#-------------------------
# L'idée c'est de partager l'échantillon en deux : training part et test part. 
# Where to split ? Always important to discuss. 
# Une possibilité : 80% / 20% pour avoi un bon modèle. 
# Autre : faire deux sous échantillons de la même taille pour avoir assez de données
# afin de faire le modèle mais aussi de bien pouvoir vérifier. 
# Une idée peut aussi de faire les deux pour être sûre de faire les choses bien. 
# Ici on va choisir de faire 50-50. 

data.train = window(ltraf, strat=c(1982, 1), end=c(2005, 1))
# 277 observations for data.train. 
data.test = window(ltraf, start=c(2005, 2), end=c(2019, 7))
# 174 observations for data.test. 

mod1.train = arima(data.train, c(1,1,1), seasonal=list(order=c(1,1,1), period=12), method="ML")
pred1.test = predict(mod1.train, n.ahead=174)

install.packages("forecast")
library("forecast")
accuracy(pred1.test$pred, data.test)
#                  ME      RMSE        MAE        MPE      MAPE      ACF1 Theil's U
# Test set -0.0428181 0.0872974 0.06712551 -0.3253468 0.5054401 0.6566793 0.9709756
# The best model has the lowest values for the accuracy parameters. 

# Plot comparing ibserved values and prdiction of the traffic 
ts.plot(traf, xlim=c(2015,2020))
lines(2.718^(pred1.test$pred), col="red")
lines(2.718^(pred1.test$pred-1.96*pred1.test$se), col=4, lty=2)
lines(2.718^(pred1.test$pred+1.96*pred1.test$se), col=4, lty=2)
# 2.718 = fct expopnentielle, on la prend parce qu'on avait le log avant et 
# on veut revenir aux valeurs sans le log. 





#--------------------------
# Estimation of a 2nd model
#--------------------------

# 1st idea : remove the non significant coeff AR1 and SAR1. 

# Without SAR1  = seasonal autoregressive coefficient : 
mod2 = arima(ltraf, c(1,1,1), seasonal=list(order=c(0,1,1), period=12), method="ML")
# first c=(p,d,q) and after list it's c(P,D,Q), so we change to have P=O. 
# Pourquoi on change P par 0 ? Pour ne plus avoir de seasonalité. 
# Pour le modèle 1, on a commencé par transformé la série en utilisant d=D=1 et s=12. 
# On a obtenu qqch de stationaire avec p=q=1 et P=Q=1. 
mod2
# AIC mod1 = -1528.81
# AIC mod2 = -1531.1 -> mod2 is a little bit better since the smaller the better. 


#---------------------
# Validation of mod2
#--------------------- 

#----------------
# Significance of coefficients : pvalue of Student test on each coeff. 

# Student_test : to check for the significance if the coefficients. 
# HO : coeff=0 against H1: coeff≠0. 
# pvalue<5%, we accept H1. 

mod2$coef # Value of coefficients
mod2$var.coef #Variance of coefficients
tstat = mod2$coef/sqrt(diag(mod2$var.coef)) # Test statistic of Student test. 
pvalue = 2*(1-pnorm(abs(tstat))) # Pvalue of the test. 
# La probabilité correspondant à la valeur absolue de tstat. 
tstat
pvalue 
#          ar1          ma1         sma1 
#  2.949679e-01 2.052116e-10 0.000000e+00 

# The pvalue of AR1 coeff (= 0.29) is still > 5%, so the coeff is still 
# non significant (we accept H0). 


# Check for the residuals autocorrelation for mod2
res2 = mod2$residuals
acf(ts(res2, frequency=1))
# Still no significant coefficients before lag 6. 
# Removing SAR1 hasn't worsended the residuals ACF. 





#--------------------------
# Estimation of a 3rd model
#--------------------------

# Let's try without AR1 coefficent. 
mod3 = arima(ltraf, c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method="ML")
# To remove AR1, I should put p=0. 
# p is the autoregressive part (I want to remove AR1). 
mod3
# AIC mod1 = -1528.81
# AIC mod2 = -1531.1 -> mod2 is a little bit better since the smaller the better. 
# AIC mod3 = -1532.08 -> mod3 is a little bit better. 

#---------------------
# Validation of mod3
#--------------------- 

#----------------
# Significance of coefficients : pvalue of Student test on each coeff. 

# Student_test : to check for the significance if the coefficients. 
# HO : coeff=0 against H1: coeff≠0. 
# if pvalue<5%, we accept H1. 

mod3$coef # Value of coefficients
mod3$var.coef #Variance of coefficients
tstat = mod3$coef/sqrt(diag(mod3$var.coef)) # Test statistic of Student test. 
pvalue = 2*(1-pnorm(abs(tstat))) # Pvalue of the test. 
# La probabilité correspondant à la valeur absolue de tstat. 
tstat
pvalue 
# Both <MA1 and SMA1 are highly significant.


# Check for the residuals autocorrelation for mod3
res3 = mod3$residuals
acf(ts(res3, frequency=1))


# Check for residuals normality 

res_norm3 = res3/sqrt(mod3$sigma2) #normalized residuals 
summary(res_norm3)
# If the residuals follow a Normal distribition of the values of res_norm
# should lie btw -2 and 2,  with 95% of chance.


plot(res_norm3)
abline(a=2, b=0, col="red")
abline(a=-2, b=0, col="red")

# Check for residual constant variance
sq.res3 = (res3)^2
acf(ts(sq.res3, frequency=1))
# 2 highly significant coeffs : lag 1 and lag 12. 
# The issue of non constant variance remains. 


#------------------------------------------------------
# Check of the quality of the fit wrt confidence bounds 
#------------------------------------------------------

library("TSA")
fit3 = fitted(mod3)
fit3

cb80 = mod3$sigma2^0.5*qnorm(0.9) #Confidence bound of security 80%
plot(cbind(ltraf, fit3-cb80, fit3+cb80), plot.type='single', lty=c(1,2,2), xlim=c(2000, 2010))

# Proportion of points in the confidence interval
indi = (ltraf-(fit3-cb80))>0 & (fit3+cb80-ltraf)>0
prop = 100*sum(indi)/length(indi) # Calculer la probabilité que le truc d'avant soit vrai. 
prop # = 85,36% 
# No big improvment with mod1. 



#----------------------------------------------------
# Let's add a dummy variable to correct for the bad fit
# at date February 1986
#----------------------------------------------------
# Adding an external variable x = fitting a SARIMAX model 


out3 = which(res_norm3 < -4) # Idnetifier les outliers
out3 

library("zoo")
index(res_norm3)[out3] # Date de l'outlier


# Create a dummy variable at date February 1986

Tlsetraf$dum1 = 0
Tlsetraf$dum1[out3] = 1







mod4 = arima(ltraf, c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method="ML", xreg=Tlsetraf$dum1)
mod4

# AIC_mod4 = -1605.75
# Big improvment wrt the previous models 

res4 = mod4$residuals 
par(mfrow=c(2,1))
plot(res3)
plot(res4)
par(mfrow=c(1,1))



# Remark in order to do predictions using model 4 

pred4 = predict(mod4, n.ahead=12, newxreg=0)
ts.plot(traf, 2.718^pred4$pred, log="y", lty=c(1,3))



