#Gerardo Naranjo Franco
#AQMS II - Prof. Fran Villamil
#February 19th 2026

#Analyzing Voter Turnout ANES
library(tidyverse)
library(broom)
library(modelsummary)
library(marginaleffects)
setwd ("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment3")
df<-read.csv("anes_timeseries_2020.csv")

#1.1 Set up and preparation
#Identifying the key variables
class(NA_character_)
class (NA_real_)
class (NA)

df = df %>%
  mutate(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_),
    education = case_when (
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse (V201231x < 0, NA, V201231x)
    )
    
#B) Dropping missing variables
df <- df %>%
  drop_na(voted, age, female, education, income, party_id)

#C) Compute overall turnout rate and show a summary
mean(df$voted)
summary(df)
#The mean for the voted variable is 0.8609832, which shows the
#average proportion of electoral participation

#1.2 Exploratory Visualization

#A) Bar graph showing turnout rate by education level
turnout_by_edu = df%>%
  group_by(education) %>%
  summarise(turnout = mean(voted))

p1<-ggplot(turnout_by_edu, aes(x = factor(education), y=turnout)) +
geom_col() +
  labs (x = "Years of education", y="Turnout rate")

ggsave("turnout_by_education.png", plot = p1)

#B) Describe the plot
#The graph shows that more years of education is positively
#correlated with turnout in voting. Meaning, that the more years
#of education that someone has, the more likely they are
#to vote. 

#1.3 Linear Probability Model

#A) Estimate an LPM of voting as an outcome of age, education, income
#and female
lpm = lm(voted ~ age + education + income + female, data = df)

#B) Print using tidy
tidy(lpm)

#C) Interpret the coefficient
#Since this is a linear model, the coefficient is interpreted as the
#expected change in the variable for every 1-unit change in X.
#In this case, it shows that every additional year in education is 
#estimated to yield a 1.9% probability increase in voting. 

#D)Checking predicted probabilities
preds_lpm = predict(lpm)
sum(preds_lpm < 0)
#This first sum gives 0 as a result
sum(preds_lpm > 1)
#This one gives 802
range(preds_lpm)
#This gives [1] 0.515 and [2] 1.170

#1.4 Logistic Regression

#A) Run a logit with the same predictors
logit = glm(voted ~ age + education + income + female,
            family = binomial, data = df)

#B) Print using tidy
tidy(logit)

#C) Compute the odd ratios and interpret
exp(coef(logit))
#According to the coeffcients, education and gender are the variables
#which have a higher influence on the odds of voting. For education, 
#the odds of voting go up by around 24.8% and for gender, females
#odds of voting go up by 34.4%.

#1.5 Comparing LPM and Logit

#A) Compute average marginal effects using logit
avg_slopes(logit)

#B) The marginal effects yield similar results to what is shown by the 
#coefficients in the logit model. This suggests that both methods 
#show similar relationships between the involved variables. 

#C) Side by side comparison
modelsummary(list("LPM" = lpm, "Logit" = logit),
             vcov = list("robust", NULL), output = "markdown")

#1.6 Predicted Probabilities

#A) Plot of probabilities
p1 = plot_predictions(logit, condition = "education")
p1
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

#B) Predicted probabilities by age and gender
p2 = plot_predictions(logit, condition = c("age", "female"))
p2
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

#C) Both age and education show a positive relationship with 
#voting turnout, although in age, males are slightly less likely to
#vote than women in general. 

#1.7 Results Display

#A) Coefficient plot comparing LPM and logit
p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
p3
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

#C) The conclusions that can be drawn from both models are similar:
#gender, education, and income are all positively associated with 
#voter turnout. The linear approximation works well. 