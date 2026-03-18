#Gerardo Naranjo Franco
#AQMS 2 - Prof. Francisco Villamil
#March 12th 2026

install.packages("did")
library(did)
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(haven)
library(tidyr)
setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment6")
df <- read.csv("minwage.csv")

#=====================
#Part 1: In Class
#1.1 Data Set up
#======================

#a)
df= df%>%mutate(NJ =ifelse(location !="PA", 1,0))
table(df$NJ)
df%>%
  group_by(NJ)%>%
  summarise (
    mean_wage_before = mean(wageBefore,  na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm=TRUE))

#b)
means = df%>%
  group_by(NJ) %>%
  summarise(
    before = mean(fullBefore, na.rm=TRUE),
    after = mean(fullAfter, na.rm = TRUE),
    change = after - before)
means

nj_change = means$change [means$NJ==1]
pa_change = means$change [means$NJ==0]
#These results suggest that employment in New Jersey, on average, increased after
#the policy was implemented. Conversely, employment in Pennsylvania decreased
#on average after the policy was implemented. This suggests that there is an 
#association between the minimum wage and the level of employment in these 2 states. 
#The size of this effect is calculated by the following
did_est = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")
#The effect of the minimum wage increase seems to be associated with a 2.9% increase
#in employment. 

#c)
df_long = df%>%
  mutate(id = row_number())%>%
  pivot_longer(
    cols=c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period=="fullAfter", 1, 0),
    NJ = ifelse (location !="PA",1,0))
nrow(df_long)
nrow(df)
#The long-format dataset has exactly twice as many rows as the original. The DiD 
#regression requires long format because the interaction post × NJ is the DiD 
#estimator: it captures how the within-NJ change in employment (post-pre) differs 
#from the corresponding within-PA change.

#==================================
#1.2 DiD Regression
#==================================

#a)
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")
#The coefficient for the interaction captures the Treatment Effect. It matches
#our previous hand calculations. The NJ coefficient captures baseline differences.

#b)
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"),
  output = "markdown")
#The DiD coefficient does not change, indicating that the use of FE in this case
#is not required. 

#c)
#We need to observe similar and comparable trends in employment between both states.
#The assumption relies on us assuming that had the minimum wage not been modified, 
#these trends would have held, thus allowing the inference that any deviation
#from this prediction is due to the minimum wage shock.
#The assumption could only be violated in the presence of another shock which may
#have impacted one state and not the other after the treatment had taken place. 
#Otherwise, their economic environments are similar enough to present them as 
#quasi-counterfactuals

#==================================
#1.3 Wage as Validation Check
#==================================

#a) DiD for wages
df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")

#The interaction coefficient post:NJ is positive and statistically significant: 
#wages rose substantially in NJ relative to PA after the policy change, and the 
#magnitude is consistent with the $0.80 minimum wage increase ($5.05 - $4.25).
#This is precisely the sign and magnitude one would expect if the law was actually 
#binding.

#b)
#The wage DiD serves as a “first stage” or manipulation check. If wages had not 
#risen in NJ after the minimum wage increase, it would be unclear whether the study 
#is truly estimating the effect of a minimum wage change at all — the law might 
#not have been binding, or firms might have already been paying above the new minimum. 
#The fact that wages did rise in NJ gives us confidence that the treatment actually 
#occurred as intended, so the employment 4 DiD can be credibly interpreted as a 
#causal response to the minimum wage increase rather than a spurious or null comparison.