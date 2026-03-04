#Gerardo Naranjo Franco
#AQM2 Francisco Villamil
#March 4th 2026

#========================================================
#Assignment 4 AT HOME
#=======================================================

#2) Setting it all up
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)
library(haven)
setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment4/assignment4_home")
df <- read_dta("infantmortality.dta")

#==========================
#2.1 Data exploration
#==========================

#2.1a) Summary and number of observations
summary(df)
nrow(df)
#There are 101 observations

#2.1b) Histogram for infant and income


#The corruption index ranges from its minimum to its maximum on the 0–10 scale. 
#GDP per capita has a large standard deviation relative to its mean and a maximum 
#far above the median, indicating right skewness.
#============================================================
#1.2 Exploratory Visualization
#============================================================

#A) Scatter plot of corruption and GDP
ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita (PPP)", y = "Corruption Perceptions Index")

#B) The relationship is positive: the richer a country is, the less corrupt 
#we would expect it to be

#C) Scatter plot with log-GDP per capita
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")
#The log function spreads out the data points in a way which captures a more linear
#relationship

#==========================================================
#3) Bivariate Regression
#=========================================================

#A) Estimate a bivariate regression of GDP and corruption
m1 = lm(ti_cpi ~ undp_gdp, data = df)
tidy(m1)

#B) Calculating the corruption index change for 10k USD change in GDP PC
tidy(m1)
coef(m1)["undp_gdp"]*10000
#We would expect the corruption perception index to increase by 1.72 (indicating
#less perceived corruption)

#C) Calculating for quartiles of interest
q25 = quantile(df$undp_gdp, 0.25)
q75 = quantile(df$undp_gdp, 0.75)
c(q25, q75)
#Using the predictions
predictions(m1,
            newdata = datagrid(undp_gdp = c(q25, q75)))
#This computation gets a glimpse of the IQR

#=========================
#4) Non-linear specifications
#=========================

#A) Creating a model for the log GDP per capita
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)
#The coefficient here also suggests that a 1% increase in GDP is linked to a 
#higher score in the index (less perceived corruption)

#A.1) Coefficient
coef(m2)["log(undp_gdp)"] * log(2)

#B) Plot predictions to better understand
plot_predictions(m2, condition = "undp_gdp")

#C) Creating the quadratic model
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
tidy(m3)

#D) Compare the R2 of all models
r2 = c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log" = summary(m2)$r.squared,
  "Quadratic" = summary(m3)$r.squared)
r2
#The R2 value which has the highest explanatory power comes from the quadratic
#model. This makes sense because GDP expansion has a bigger effect on smaller
#values than it does on bigger ones (diminishing returns, which makes this
#more suitable for the type of data that we are handling.)

#==============
#5) Marginal Effects
#=============

#A) AME of GPD in the model
avg_slopes(m2, variables = "undp_gdp")

#B) While the coefficient tells us the estimated change of the outcome variable
#with the increase of 1-unit on the Beta assuming linear relationships, 
#the AME tell us the average effect across the observations from 
#having such an increase in a concave function.

#C) Compute marginal effects for the m3 model
slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
#As mentioned, the effect of a 1-dollar increase in GDP is larger in poorer countries.
#For rich countries, a 1 dollar increase has less returns in the corruption index.
#This is aligned with the shape of the graph we observed. 

#============
#6) Prediction Plots
#============

#A) Prediction plot for the log model
p1 = plot_predictions(m2, condition = "undp_gdp")
p1
ggsave("pred_plot_m2.png", p1, width = 6, height = 4)

#B) Prediction plot for quadratic model
p2 = plot_predictions(m3, condition = "undp_gdp")
p2
ggsave("pred_plot_m3.png", p2, width = 6, height = 4)

#C) Interpreting
#Although differing slightly in values, both models are consistent with the finding
#that GPD growth has diminishing returns in terms of the corruption perception index.
#While poorer countries experience a bigger effect from increasing GDP, richer countries
#do not shift as dramatically with an increase in GDP

#========
#7) Residual Diagnostics
#========

#A) Residuals vs fitted for the level-level model
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")

#The plot suggests heteroskedasticity because residuals start to deviate from the 
#line at higher levels of fitted values. This suggests that the linear relationship
#may fail to capture the curvature of the relationship between the variables

#B) Residuals vs fitted for the log model
m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")

#In this case, the curvature is captured more appropriately. This improves the 
#residual pattern

#C) Cook's Distance for Influential Observations
n = nrow(df)
threshold = 4 / n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cname[influential]

plot(m2, which = 4)

#D) 
# Influential observations should not be removed automatically. They may 
#represent genuine cases (e.g., very wealthy or very corrupt countries) rather 
#than data errors. A recommended robustness check would be to reestimate 
#the model excluding these observations and compare the coefficients. If the 
#results are similar, the original estimates are robust.


#======
#8) Publishing table
#======

#A) Table comparing models
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

#B) Interpreting
#The level-log model (m2) is the preferred specification. It has the highest R2
#produces the best residual diagnostics, and its functional form has a clear 
#substantive interpretation: the relationship between wealth and corruption is 
#one of diminishing returns. The log transformation also avoids the quadratic 
#model’s problem of an eventual sign reversal at extreme values.
