#===============================
#Assignment 9: Survival Analysis
#AQMS 2 - Francisco Villamil
#April 13th 2026
#================================

setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/Assignment_9")
install.packages("survival")
library(survival)
library(broom)
library(marginaleffects)
library(ggplot2)
lung <- survival::lung
lung$dead<-lung$status - 1
#===========================
#2.1 Kaplan-Meier Survival Curves
#==========================

#a)
obs<-nrow(lung)
deaths<-sum(lung$dead ==1, na.rm=TRUE)
censored<-sum(lung$dead == 0, na.rm= TRUE)
prop_censored<-(censored/obs)
obs
deaths
censored
prop_censored
#There are 228 total observations, with 165 deaths, 63 censored individuals,
#and a proportion of censored observations of 27.63%

#b)
km_fit <- survfit(Surv(time, dead) ~ 1, data = lung)

print(km_fit)
#As shown, the median survival time is 310 days. This means that we can expect 50%
#of patiens to survive at least 310 days after being admitted.

#c)
km_sex<-survfit(Surv(time, dead) ~ sex, data = lung)
km_df<-broom::tidy(km_sex)
ggplot(km_df, aes(x=time, y=estimate, color = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=strata),
              alpha = 0.2, linetype = 0) +
  labs(title = "Kaplan-Meier Survival Curves by Sex",
       x="Time (days)",
       y="Survival Probability")+
  theme_minimal()
ggsave("km_survival_by_sex.pdf", widht=8, height=5)

logrank<-survdiff(Surv(time, dead)~sex, data=lung)
logrank
p_value<-1 - pchisq(logrank$chisq, df=1)
p_value

#As the graph and the tests show, women are expected to survive slightly longer than
#men, as shown by the positioning of their curve in the KM graph on top of the male
#curve. There are sections of the graph in which the CI do not overlap, which in
#combination with a p-value of 0.00131, allow us to reject the H0 and confirm that
#the curves are different at statistically significant levels. 

#===================================
#2.2 Cox Proportional Hazards Model
#===================================

#a)
cox_model<-coxph(Surv(time, dead)~age+sex+ph.ecog, data = lung)
summary(cox_model)
#As shown by the model, women show a hazard ratio which suggests they are more likely
#to survive than men. Specifically, around 42% more likely (1-0.574). The z-score 
#and p-value indicate that this is a statistically significant difference.

#b)
exp(coef(cox_model)["ph.ecog"])
#The hazard ratio for this variable is 1.5899. This suggests that a one-unit increase
#in this indicator brings an individual closer to death. Specifically around 59% 
#more likely. 

#c)
ph_test<-cox.zph(cox_model)
ph_test
#The p-value for all variables is above 0.05, which suggests that none of the variables
#change significantly over time. This is confirmed in the GLOBAL test, which suggests
#that the overall model is fine in explaining the observed results.

#d)
#The KM analysis suggested higher survival rates for females than males over time.
#In the Cox Model, only sex and ph.ecog were found to be significant. For sex, the model
#suggests that females are approximately 42% less likely to die at a given time than
#their male counterparts. Additionally, the ph.ecog tells us that a worsening score 
#(measured as an increase of one-unit) is associated with 58% higher chances of death.
#Testing for the proportional hazards assumption, we observe that it holds for all 
#variables and thus, for the overall model. This tells us that, potentially, men who
#score worse on the ph test are the segment most at risk of death within the scope
$of the analyzed variables. 