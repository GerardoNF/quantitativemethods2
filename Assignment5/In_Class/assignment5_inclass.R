#Gerardo Naranjo Franco
#AQMS 2 - Prof. Francisco Villamil
#March 5th 2026

install.packages("fixest")
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(haven)
setwd("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment5")
df <- read_dta("teaching_evals.dta")

#=========================
#1) Set up the data
#==========================

#1.1) Set up and data exploration
length(unique(df$State))
length(unique(df$Year))
table(unique(df$State))

#1b) 
summary(df$PresApprov)
summary(df$UnemPct)
df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))
ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")
#States do not move consistently together over time. There are slight variations
#in the presidential approval rates which seem to be specific to a particular
#state observation. For example, California exhibits a sharp drop in the late 
#90s, whereas New York was peaking. 

#1c) 
ggplot(df, aes(x=UnemPct, y=PresApprov, color=State))+
  geom_point(alpha = 0.4) +
  #geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Unemployment rate (%)", y="Presidential approval (%)")


#=============================
#1.2 Pooled OLS
#==============================

#2a)
m_pooled = lm(PresApprov ~ UnemPct, data=df)
summary(m_pooled)

#2b)
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)
modelsummary(list(m_pooled, m_pooled2), stars = TRUE)

#=============================
#1.3 Fixed Effects
#==============================

#a)
m_fe = feols(PresApprov ~ UnemPct | State, data = df)
modelsummary(list("Pooled OLS" = m_pooled, "State FE" = m_fe),
             vcov = ~State,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"),
             output = "markdown")
#The coefficient on UnemPct changes relative to pooled OLS. The state fixed 
#effects model compares approval within the same state across different years, 
#removing the influence of any time-invariant state characteristics

#b)
# State fixed effects absorb all time-invariant differences across states — 
#including geography, political culture, long-run economic structure, and 
#regional identity. This is precisely why South drops from the model: it does not
#vary within a state over time, so its effect is indistinguishable from the 
#state-specific intercept (fixed effect). Any time-invariant variable is collinear 
#with the set of state dummies and cannot be estimated separately

#c) 
#The coefficient on UnemPct in the state FE model identifies a within-state effect: 
#it measures how approval changes in a given state when its unemployment rate 
#rises or falls, compared to that state’s own average. This is fundamentally 
#different from pooled OLS, which compares states with different unemployment 
#levels to each other. The FE estimator controls for all stable state-level 
#confounders (observed or not) but cannot account for time-varying omitted variables.

#======================
#4 TWFE
#======================

#a and b)
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

#c)
#Year fixed effects absorb common time shocks: national economic cycles, 
#presidential scandals, wars, or any other event that affects approval in all 
#states simultaneously in a given year. If national unemployment rises during a
#recession, both the unemployment rate and presidential approval will move 
#together in all states at once — not because of a state-level effect but 
#because of the shared macro environment. Adding year dummies removes this
#source of confounding and identifies the effect of a state’s unemployment 
#relative to the national average in each year. If the coefficient on UnemPct 
#changes noticeably after adding year FEs, it suggests that common time trends
#were partly driving the relationship estimated with state FEs alone


