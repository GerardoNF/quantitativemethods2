#Gerardo Naranjo Franco
#AQMS 2 - Prof. Francisco Villamil
#March 11th 2026

library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment5")
df = read.csv("presidential_approval.csv")

#===================
#2.1 Data Exploration
#====================

#a)
n_distinct(df$InstrID)
n_distinct(df$CourseID)
#There are 48 different instructors and 254 different courses in the data
obs_per_instr <- df %>%
  group_by(InstrID) %>%
  summarise(n_obs = n())
mean(obs_per_instr$n_obs)
summary(df)
#The average number of course observations per instructor is 17.5. This therefore
#feels like a small panel

#b)
plot_scatter=ggplot(df, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Percent of students receiving A/A-",
    y = "Average course evaluation",
    title = "Teaching evaluations and grade inflation"
  )
ggsave(
  filename = "scatter_eval_apct.png",
  plot = plot_scatter,
  width = 7,
  height = 5,
  dpi = 300
)
#The relationship between both variables appears to be positive.
#However, the slope is quite flat and the intercept for ACE is quite high already.
#This suggests that the relationship between both variables, for the given sample, 
#seems minimal. This does surprise me, as one would expect evaluations to go
#up with higher grades more drastically. 

#===================
#2.2 Pooled OLS Baseline
#====================

#a) 
m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
modelsummary(m1, stars = TRUE)
#The coefficient for Apct is 0.359 and it is significant at conventional levels.
#This suggests that a 1-percent increase in the percentage of students with an 
#A or A- in a class is associated with an increase in the teaching evaluation of
#0.359. This captures the positive relationship shown in the graph above. 

#b) 
#The OLS model above may yield biased estimates of Apct through the model's 
#ignorance of other unobservable characteristics in instructors which may affect
#both the percentage of A's given in a class as well as the overall evaluation. 
#For example, a very laid back teacher who is not heavily invested in the course's
#evaluation may be more likely to give higher grades, but also to be more chatty
#and close with the students. Both of these things are confounding in assessing which
#one drove teaching evaluations higher. 
#Similarly, courses which are focused on easier topics may yield higher scores due
#to how easy they are. The easiness of the course could also be a cause behind students
#giving it a higher evaluation score. 

#===================
#2.3 Fixed Effects
#====================

#a)
library(fixest)
m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df
)

#b)
install.packages("webshot2")
modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "regression_models.png")

#c)
#Coefficients in the FE and TFE models shrink but remains positive and significant.
#The FE model controls for time-invariant instructors characteristics (i.e leniency,
#charisma, enthusiasm, etc). This means that the model compares instructors to 
#themselves rather than to one another. The TFE model also controls for time shocks
#in the sample; meaning it captures and accounts for time variations in the way 
#evaluations fluctuated for reasons other than the instructor. 
#Given that the size of the effect shrinks in relation to the OLS, we can infer
#that the coefficient in the original model was at least partially being driven
#by variations in instructor characteristics. However, the change is minimal. 

#===================
#2.4 Random Effects and Hausman Test
#====================

#a)
install.packages("plm")
library(plm)
pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")

#b) 
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

#c)
#The results of the Hausman test yield a p-value of 0.178. In this context, the 
#null hypothesis is that the unobserved individual instructor characteristics 
#are not correlated with the regressors, in which case, the Random Effects model 
#would be prefered. However, since the p-value allows us to reject the null 
#hypothesis, we now have evidence to infer that these unobserved characteristics 
#are indeed correlated with the regressors, which amplifies the need for interpreting
#the data using a Fixed Effects model. 