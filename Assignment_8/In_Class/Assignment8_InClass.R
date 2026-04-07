#===============================
#Assignment 8: Spatial Data II
#AQMS 2 - Francisco Villamil
#March 26th 2026
#================================

library(sf)
library(spData)
install.packages ("spdep")
library(spdep)
install.packages("spatialreg")
library(spatialreg)
library(ggplot2)
data(world)


#===========================
#1.1 
#===========================

#a) Set up stuff
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world)
#160 observations remain after adjusting. GDP per capita is logged because it is 
#a variable which skews a lot. Therefore, coefficients are easier to interpret 
#if thinking of GDP as a log (percentage increase) rather than absolute dollar-increases

#b) OLS regression of life expectancy
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

#The coefficient suggests that an increase in GDP is associated with an increase in
#life expectancy of 5 extra years. The association is statistically significant. 

#c) OLS residual mapping
world$ols_resid = residuals(ols_fit)
library(ggplot2)
ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("ols_residuals_map.pdf", width = 10, height = 5)

#People in Africa are shown to live shorter lives than what would be expected
#from their GDP per capita. This is also observable in some Gulf countries and 
#in Russia. Conversely, Mediterranean countries and some central american countries
#tend to live longer than what would be expected from their GDP.

#===========================
#1.2 Spatial Weights Matrix
#===========================

#a) Queen contiguity neighborhood
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)
#There are 16 total observations with no neighbors. This occurs due to island countries
"in which there are no land borders within the specified distances"

#b) Moran's Test on residuals
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
#Moran's I statistic is 0.437 and the p-value conveys statistical significance at
#conventional levels. This suggests that there is a somewhat strong spatial autocorrelation 
#amongst residuals in our observations. This violates the assumption of independent 
#observations needed to estimate reliable coefficients. 

#=============================
#1.3 Lagrange Multiplier Tests
#=============================

#a and b) Which test to use
lm_tests = lm.LMtests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)
summary(lm_tests)

#a)
#Running this test, we observe that only spatial dependence on the error term
#belongs in the model in a statistically significant way. Conversely, the Spatial
#lagged-dependency is not shown to be statistically significant. 


#b) Robust tests are also only significant in the Spatial Error Dependency, which 
#indicates that we should use the SEM model as a more appropriate way to analyze 
#the data. 

#==============================
#1.4 SEM 
#==============================

#a) Fit SEM
sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

#Once we acount for spatial dependence, the coefficient of log GDP per capita
#is reduced. Meanwhile, lambda is positive and statistically signficiant. This
#suggests that at least some of the variation in life expectancy originally attributed
#to GDP per capita is explained by spatial variations as well. 

#b) What does lambda represent
#Lambda represents the degree of outcome dependence on unobserved spatial
#factors. Therefore, it does not tell us specifically which factors to consider.
#It only indicates how much variation can be attributed to spatial differences. 

#c) Moran's I on residuals
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)
#Comparing this Moran’s I to the one from question 2b, the SEM substantially 
#reduces the spatial autocorrelation in the residuals. The test statistic is now 
#much closer to zero and the p-value is no longer significant (or much less so),
#indicating that the spatial error correction has absorbed most of the geographic 
#clustering that OLS left behind in its residuals

#===================
#1.5 Distance Based Weights
#====================

#a)
coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)

#Using this method, we observe 114 countries with no neighbors. This stands in stark
#contrast to the 16 countries with no neighbors from the queen's contiguity matrix. 
#This may occur because now to be considered a neighbor, it is not borders that matter but
#centroids. This means that big territories may be singled out as having "no neighbors"
#because 300km away from their centroid it is still the same country. Thus, increasing
#the number of observations with no neighbors. 

#b)
listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)
#Lambda in this case is 0.425 and statistically significant as p=0.00013. These 
#results differ from the SEM computed in 1.4, which yielded a lambda of 0.762 at 
#p=<2.22e-16. Similarly, the log gdp coefficient for the SEM model is 3.96 and 
#statistically significant, whereas for this centroid-based version the log gdp
#coefficient is 5.47 and statistically significant. This suggests that these models
#are highly sensitive to how neighborhoods are operationalized.

#c)
world$sem_dist_resid = residuals(sem_dist)

moran.test(world$sem_dist_resid, listw = listw_dist, zero.policy = TRUE)
#In comparison to the previous test, the Moran I statistic value dropped and the 
#p-value also became smaller. However, they both suggest that there is no statistical
#significance. 