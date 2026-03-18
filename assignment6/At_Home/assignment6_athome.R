#Gerardo Naranjo Franco
#AQMS 2 - Prof. Francisco Villamil
#March 18th 2026

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(did)
data("mpdta")

#=============
#2.1 Data Structure and Visualization
#=============

#a)
n_distinct(mpdta$countyreal)
#There are 500 unique counties in the dataset
unique(mpdta$first.treat)
#There are 3 first-treatment variables and one baseline one for counties which
#never received treatment
table(mpdta$first.treat)
#There are 1545 untreated counties, 100 treated in 2004, 200 treated in 2006, and
#655 treated in 2007.

#Staggered treatment effect in this case refers to the idea that counties receive
#treatment at different stages in time. This is why comparisons cannot simply be 
#made between treated and non-treated observations. This would result in comparisons
#made between units who are at very different stages of treatment. 

#b)
library(dplyr)
library(ggplot2)
mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))
ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line()
geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("2_1b_cohort_trends.png", plot = p_cohort, width = 7, height = 5, dpi = 300)

#Counties display similar trends except for those who adopted treatment
#in 2004. For these, there was a slightly steeper decrease in teen employment
#which kept occuring even after treatment adoption in 2004. All other counties
#remain relatively flat with vert slight variations and a weak upward trend in the
#years between 2004 and 2006.

#==============================
#2.2 Naive TWFE vs. Callaway-Sant´anna estimator
#==============================

#a)
mpdta <- mpdta %>%
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

m_twfe <- feols(lemp ~ treated_post + lpop | countyreal + year, data = mpdta)
summary(m_twfe)

modelsummary(m_twfe, stars = TRUE)
#The coefficient for this model is negative and significant, indicating that 
#treatment is associated with a slight decrease in teen employment in 
#observed counties.However, this model is only taking into account county
#and year fixed effects without accounting for the staggered nature of the 
#implementation of the treatment.

#b)
cs_never <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated"
)

summary(cs_never)

agg_never_simple <- aggte(cs_never, type = "simple")
summary(agg_never_simple)
#Using the ATT Treatment Effect, we observe that the effect remains negative but
#increases in magnitude, now being -0.0418. This measure is prefered in staggered 
#settings because it takes into account each cohort individually rather than 
#pooling them all together into the same category as the TWFE model does. 

#c)
agg_never_dynamic <- aggte(cs_never, type = "dynamic")
summary(agg_never_dynamic)

p_event_never <- ggdid(agg_never_dynamic) +
  labs(title = "Event study: Callaway-Sant'Anna with never-treated controls")

p_event_never
ggsave("2_2c_eventstudy_nevertreated.png", plot = p_event_never, width = 7, height = 5, dpi = 300)

#The graph shows that pre-treatment leads are not distinguishable from 0, as shown
#by the plotted points and the errors. This supports the parallel trends assumption
#in suggesting that given the absence of treatment, observed units would have been
#plausibly expected to follow the same trajectories over time. Conversely, the
#post-treatment estimates suggest that treatment effects are negative and follow
#such a trajectory for 2 time-units after the implementation of treatment before
#stabilizing and recovering slightly after period 2. 

#===========================
#2.3 Pre-Testing Parallel Trends
#===========================

#a)
cs_never_boot <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = TRUE
)

summary(cs_never_boot)

#Running this test, the framework is that the Null Hypothesis (H0) is that all
#pre-treatment effects are equal to 0. The Alternative Hypothesis (H1) is that 
#at least one of them won't be. Given that the p-value is 0.23 approximately and this
#is larger than 0.05, we fail to reject the null hypothesis and thus provide
#statistical evidence that the parallel trends assumption is supported. 

#b)
p_gt <- ggdid(cs_never_boot) +
  labs(title = "Group-time ATT estimates with uniform confidence bands")

p_gt
ggsave("2_3b_group_time_ATT.png", plot = p_gt, width = 8, height = 6, dpi = 300)
#As shown by the graph, pre-treatment effects are not distinguishable from 0. 
#As time advances after treatment, the effect starts to differentiate from 0

#c)
#Although the pre-test is helpful in understanding the state of observed units
#prior to the presence of treatment, it does not account for potentially deviating
#factors influencing the observed effects after treatment has been implemented. 
#Therefore, it does not prove that trajectories between cohorts would have remained
#similar in the abscence of treament in post-treatment periods. 

#==============================
#2.4 Using not-yet units as controls
#=================================

#a)
cs_nyt <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "notyettreated"
)

summary(cs_nyt)

agg_nyt_simple <- aggte(cs_nyt, type = "simple")
summary(agg_nyt_simple) 

#The ATT using not-yet treated controls remains similar to those observed previously.
#Instead of being -0.0418, now the coefficient is -0.0414. This suggests that 
#the selection of controls leaves results mostly unaffected. An explanation for this
#could be that anticipation effects are not significant in this dataset. 

#b)
agg_nyt_dynamic <- aggte(cs_nyt, type = "dynamic")

p_event_nyt <- ggdid(agg_nyt_dynamic) +
  labs(title = "Event study: Callaway-Sant'Anna with not-yet-treated controls")

p_event_nyt
ggsave("2_4b_eventstudy_notyettreated.png", plot = p_event_nyt, width = 7, height = 5, dpi = 300)

#The comparison between employing not-yet treated units as controls versus 
#never-treated units yields similar results in both trends and effects. Therefore, 
#changing the employed controls for this analysis does not significantly alter the 
#conclusions.

#c)
#Not-yet treated controls may be desirable in cases where we do not expect anticipation
#to significantly alter the trajectory that a unit is expected to follow. Including
#these as controls allows for a larger pool of controls which may yield more precise
#results. However, employing never-treated controls may provide cleaner comparisons
#in cases where we fear that external noise may impact the trajectory that certain 
#units would have followed in the abscence of a tratment. 

#============================
#2.5 TWFE Forbidden Comparison
#=============================

#a)
#TWFE is problematic in staggered settings because it employs already-treated units
#as controls for newly-treated units as time moves on. This is a problem because
#already-treated units may be already experiencing the effects of treatment and thus
#are not reliable benchmarks to compare newly-treated units against. 

#b) 
#Although the coefficients are not substantially different in the TWFE model and 
#the Callaway-Sant Anna estimate, the latter is prefered in this study because it
#takes into consideration the staggered nature of the treatment under study.
#Additionally, it provides more robust evidence to support the parallel trends
#assumption before treatment is applied. 


