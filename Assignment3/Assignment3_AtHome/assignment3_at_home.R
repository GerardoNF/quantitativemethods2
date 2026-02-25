#Gerardo Naranjo Franco
#AQMS II - Prof. Fran Villamil
#February 24th 2026

#Analyzing Voter Turnout ANES
library(tidyverse)
library(broom)
library(modelsummary)
library(marginaleffects)
setwd ("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment3")
star_data<-read.csv("star.csv")

#2.1 Data Preparation

#A) Creating the variable names
# Convert classtype to factor with labels
star_data$classtype <- factor(star_data$classtype,
                       levels = c(1, 2, 3),
                       labels = c("Small", "Regular", "Regular+Aide"))
# Convert race to factor
star_data$race <- factor(star_data$race,
                  levels = 1:6,
                  labels = c("White", "Black", "Asian",
                             "Hispanic", "Native American", "Other"))

#B) Small Binary Variable
star_data$small <- ifelse(star_data$classtype == "Small", 1, 0)

#C) Drop missing values
star_data <- subset(star_data, !is.na(hsgrad))
nrow(star_data)
#3047 observations remain after dropping missing values

#D) Compute high school graduation rate by class type
mean(star_data$hsgrad)
#The mean graduation rate overall for the students in this data set is
#0.83
aggregate(hsgrad ~ classtype, data = star_data, mean)
#For class type, the grad rates are as follows:
#Small - 0.8359202
#Regular - 0.8251619
#Regular + Aide - 0.8392857

#2.2 LPM and logit

#A) LPM estimation
lpm1 = lm(hsgrad ~ small, data = star_data)
summary (lpm1)
#B) Estimate using same predictor
logit1 = glm(hsgrad ~ small, family = binomial, data = star_data)
summary(logit1)

#C) The coefficient for small in the lpm1 model is very small. This 
#suggests that the difference in graduation rates between small and 
#non-small classes is not estimated to be significantly different
#(0.3% higher for small classes)

#D) Avg. Marginal Effects
library(marginaleffects)
avg_slopes(logit1)

#Indeed, the estimate is very similar to the coefficient calculated in the
#LPM1 model

#2.3) Adding controls to the model

#A) Creating the model
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star_data)
logit2 = glm(hsgrad ~ small + race + yearssmall,
             family = binomial, data = star_data)
summary(lpm2)
summary(logit2)

#B) Comparing the coefficients between the control models and the 
#bivariate ones, we observe that they are much higher for the control ones.
#(in terms of what we would estimate as an outcome)
#This indicates that randomization was not properly conducted, as 
#the presence of other variables in the model should not dramatically
#change results in the presence of proper randomization

#C) Computing marginal effects for yearssmall
avg_slopes(logit2, variables = "yearssmall")
#The estimate is 0.0283. This suggests that an extra year in a small
#class increases the log-odds of graduation by that amount. 

#2.4) Predicted Probabilities

#A) Computing predicted probabilities for specific profiles
#White student in a small class with 3 years in small classes
p1 <- predictions(
  logit2,
  newdata = data.frame(
    race = factor("White", levels = levels(star_data$race)),
    small = 1,
    yearssmall = 3
  )
)

p1
#A white student who spends 3 years in a small class has an estimated
#graduation probability of 86.9% with a 95%CI of 84.5%-89%

#Black student in a regular class with 0 years of small classes
p2 <- predictions(
  logit2,
  newdata = data.frame(
    race = factor("Black", levels = levels(star_data$race)),
    small = 0,
    yearssmall = 0
  )
)

p2
#In this case, the estimated graduation probability of a Black student 
#in a regular class, who has spent 0 years in small classes is 72.9% 
#with a 95%CI of 69.5%-76.2%

#B) Plotting predicted probabilities for yearssmall for small
#and non-small clases
graph1 = plot_predictions(logit2, condition = c("yearssmall", "small"))
graph1
ggsave("pred_prob_grad.png", graph1, width = 6, height = 4)

#2.5 Interactions

#A) Calculating effects for different races in small classes
logit3 = glm(hsgrad ~ small * race + yearssmall,
             family = binomial, data = star_data)
summary(logit3)

#B) Calculating marginal effect
avg_slopes(logit3, variables = "small", by = "race")

#C) The small class effect appears to be larger for Asian and Black groups
#than for other races included in this sample. This is only brought
#to light by analyzing the interaction of "small" and "race" in the model

#2.6 Presenting results and discussion

#A) Creating modelsummary table
library(modelsummary)
modelsummary(
  list(
    "LPM Bivariate" = lpm1,
    "LPM Controlled" = lpm2,
    "Logit Bivariate" = logit1,
    "Logit Controlled" = logit2
  ),
  vcov = list("robust", "robust", NULL, NULL),
  stars = TRUE
)

#B) Coefficient Plot
modelplot(
  list(
    "LPM Bivariate" = lpm1,
    "LPM Controlled" = lpm2,
    "Logit Bivariate" = logit1,
    "Logit Controlled" = logit2
  )
)
ggsave ("coef_plot_star.png",
       width = 7,
       height = 5)

#C) Last sentences
#Analyzing the results, it becomes clear that there is an intuition
#problem with running the regressions including "small" and "yearssmall"
#as variables of interest. In the bivariate model, the effect of "small"
#on graduation probabilities (estimated) is positive but not significant.
#Once we run the models with controls, the effect becomes negative
#and significant. However, the effect of "yearssmall" is positive and
#significant. How can being in a small class make you less likely
#to graduate, but spending an additional year in one make you more
#likely to do so? Therefore, the models in which both "small" and "yearssmall"
#are included are probably problematic from an endogeneity point of view. 
#Intuitively and theoretically, we should expect small class sizes to 
#improve the chances of graduation. What we can take away from these results,
#however, is the idea that different race groups are impacted in different
#ways from the incorporation of small classes. Which suggests that 
#there are underlying mechanisms in the way that being in a small class
#supports or undermines a student's probability of graduating
#on the basis of their racial group. 
#Nonetheless, another important consideration is that the R2
#value for the models is quite low, which reduces the explanatory
#power of them all and leaves a significant percentage of the variation
#unnacounted for by the variables of interest. 





