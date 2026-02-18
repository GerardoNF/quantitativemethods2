#Setting up everything before getting started
setwd ("/Users/gerardonaranjo/Desktop/quantitativemethods2/assignment2/Assignment2_Part2_STAR")
library (dplyr)
library(ggplot2)
library(broom)
library (modelsummary)
star_data <-read.csv("star.csv")

#2.1 Data Preparation
#Creating the labels for class type
star_data$classtype<-factor(star_data$classtype,
                            levels = c(1,2,3),
                            labels = c("Small",
                                       "Regular",
                                       "Regular+Aide"))
#Creating the labels for race
star_data$race<-factor(star_data$race,
                       levels = 1:6,
                       labels = c("White",
                                  "Black",
                                  "Asian",
                                  "Hispanic",
                                  "Native American",
                                  "Other"))
#Creating the labels for class size
star_data$small<-ifelse(star_data$classtype=="Small", 1,0)

#Reporting the number of observations for gr4reading and math
nrow(star_data)
#There are 6325 observations, out of which usable:
sum(!is.na(star_data$g4reading))
sum(!is.na(star_data$g4math))
#For reading, we have 2353 usable observations
#For math we have 2395

#2.2 Comparing groups
#The mean 4th grade reading score by class type
tapply(star_data$g4reading, star_data$classtype, mean, na.rm=TRUE)
#Small classes are the ones who score highest (723.39)

#Bivariate regression on gr4reading on small
m_reading<-lm(g4reading ~ small, data = star_data)
summary(m_reading)
#Interpretation
#The small coeff. is equal to 3.10. This suggests that
#students in a small class can be estimated to 
#score 3.10 points higher in the reading tests
#than students in non-small classes

#Verify coefficient with dif-in-means
723.3912 - 719.8900
#The difference is close to the coefficient found

#Repeating the pattern for math
m_math<-lm(g4math ~ small, data = star_data)
summary(m_math)
#The results are not similar. Here, being in a
#small class only makes a student's predicted
#score higher by 0.5912 points.

#2.3 Adding controls
#Multiple regression of reading, small, race, and yearsmall
m_reading_multi<-lm(g4reading ~ small + race + yearssmall, data = star_data)
summary(m_reading_multi)

#Comparison
#Once we control for other factors, the small coefficient
#changes dramatically. It goes from 3.1 to -4. This
#suggests that the randomization process was not
#properly done

#Interpreting yearssmall coefficient
#The coefficient is 2.170. Suggesting that for every
#increase of 1 year spent in a small class, we can estimate
#a student's reading scores to go up by 2.170

#2.4 Interactions
#Effect of being in a small class and race
m_classtype_race<-lm(g4reading ~ small*race + yearssmall, data = star_data)
tidy(m_classtype_race)

#Estimated effects of small for white and black students
#Since White is the baseline, we see that the effect of "small"
#classes in White students is estimated to yield a score 5.32
#points lower than students in other class types. 
#For Black students, we add the baseline plus its interaction coeff. 
#This yields that Black students in small classes are estimated to 
#score 1.65 points higher on these tests. 
#However, none of these coefficients is statistically significant, 
#indicating non-meaningful findings. 

#2.5 Result Presentation
modelsummary(
  list("Bivariate" = m_reading,
       "Multiple" = m_reading_multi,
       "Interaction" = m_classtype_race),
  vcov = "robust",
  output = "star_models.html"
)

#Now the coefficient plot
p_star<-modelplot(
  list("Bivariate" = m_reading,
       "Multiple" = m_reading_multi,
       "Interaction" = m_classtype_race),
  vcov = "robust"
)

p_star

ggsave("star_coef_plot.png",
       plot = p_star,
       width = 8,
       height = 6)

#2.6 Brief Discussion
#Looking purely at the effect of small class sizes in reading test scores,
#one would assume that the effect of smaller class sizes is beneficial. 
#This is captured in the bivariate coefficient of the models shown. 
#Once controls for race and years spent in a small class are accounted for
#(both as controls and interactions), the effect becomes increasingly blurry.
#In the interaction model (between race and small classes), the coefficients even
#suggest that small class sizes are more beneficial for Black students than they 
#are for White students. Nonetheless, none of these coefficients is ever
#shown to be statistically significant. Therefore, we cannot confidently
#accept the alternative hypothesis and suggest that small class size is 
#causally linked to higher test scores, even if randomization allegedly took 
#place in the assortment of the data (which usually allows for causal inference).


