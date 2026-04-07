#===============================
#Assignment 8: Spatial Data II
#AQMS 2 - Francisco Villamil
#April 6th 2026
#================================

library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)
data(world)

#=============
#2.1 Spatial Lag model
#=============

#a) Report estimated rho and p-value
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(slm_fit)

#As shown in the model, the rho value is near 0 (-0.0043) and NOT statistically
#significant, as its p-value is 0.805. This suggests that there is no spatial
#dependence between life expectancy and GDP per capita. The coefficient for log_gdp,
#on the other hand, is 5.55 and statistically significant.

#b) The interpretation of rho is given above. However, for added specificity, we 
#can say that had rho been positive and statistically significant, this would
#mean that we can expect at least a portion of how much life expectancy is in a country
#to be a result of the life expectancy of neighboring countries.

#c) Given that the SLM model assumes propagation of the outcome variable
#across neighboring observations, the log_gdp coefficient is not representative
#on GDP's effect on a given country i. Instead, it is built into the model in a way
#which assumes spatial dependence, meaning that X affects neighboring countries too. 

#================================
#2.2 Direct and Indirect Effects
#=================================

#a) 
set.seed(123)
impacts_slm = impacts(slm_fit, listw = listw, R = 500)
summary(impacts_slm)

#The direct effect is 5.548, the indirect effect is -0.024, and the total effect
#is 5.525. In comparison to the coefficients obtained in the OLS and the SLM model,
#the direct effect is practically identical to the SLM coefficient but appears to be
#slightly bigger than the OLS coefficient. 

#b) 
#The indirect effect captures the expected effect that a one-unit increase of the IV
#in an observation would have in a neighboring unit. Therefore, it is an approximate
#measure of spatial interdependence. 

#c) In this case, the total effect is actually slightly smaller than the direct 
#effect because the indirect effects are negative. This occurs because, as we saw,
#rho is negative as well, suggesting that spillover effects are negative. However,
#there were not found to be statistically significant, which is aligned with our
#findings from the LM tests where spatial dependence was not found to be significant. 

#==========================
#2.3 Model Comparisons
#==========================

#a) 
AIC(ols_fit, sem_fit, slm_fit)
#The AIC Values are:
#OLS = 965.988
#SEM = 894.7021
#SLM = 967.927
#Since the lowest value is the SEM, this is found to be the best fit. This aligns
#with our findings from the LM tests in question 1.3b.

#b)
#Moran's I Statistic Test from part 1 was found to be 0.47 and statistically significant.
#This implies that there is spatial autocorrelation in the residuals of the OLS model,
#thus violating its assumption of independent observations. Using LM models to 
#navigate this, we found that only the p-value for Spatial Dependence on the Error Term
#was significant, indicating that SEM is more fitting than SLM. The coefficient for 
#log GDP was found to be very similar across models, but slightly larger under an OLS.
#Spillover effects extracted from the SLM show no significant spatial dependence, 
#indicating low interdependence in the outcome variable (life exp.) amongst observed countries. 
#One limitation of the queen contingency matrix is that island countries are excluded and that 
#all neighboring countries are treated equally, regardless of actual distance. 

#========================
#2.4 Spatial Durbin Model
#========================

#a) 
sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, Durbin = TRUE,
                   zero.policy = TRUE)

summary(sdm_fit)

#The lag.log_gdp coefficient captures the effect of neighboring countries' GDP
#on a country's life expectancy. In this case, this coefficient is negative (-3.83)
#and also statistically significant, suggesting that, holding a country's GDP constant,
#having richer neighbors is associated with a lower life expectancy in said country. 
#The rho value now is also positive and statistically significant, indicating that 
#countries resemble their neighbors more closely than what the other models suggested. 

#b)
AIC(slm_fit, sem_fit, sdm_fit)
#As shown by the AIC test, the model with the lowest value and thus best fit is still
#the SEM, thus suggesting that the added complexity of the Durbin Model is not justified. 