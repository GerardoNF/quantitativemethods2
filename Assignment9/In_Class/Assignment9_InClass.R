#===============================
#Assignment 9: Other Outcomes
#AQMS 2 - Francisco Villamil
#April 9th 2026
#================================

library(carData)
install.packages("MASS")
library(MASS)
install.packages("nnet")
library(nnet)
install.packages("pscl")
library(pscl)
install.packages("AER")
library(AER)
library(marginaleffects)
library(ggplot2)
data(BEPS)

#==================
#1 Ordered Logit: Perceptions of National Economy
#==================

#a)
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)
#The table shows that the distribution of responses is highly concentrated in
#between values of 2, 3 and 4, with 3 being the most common response. Very few
#people chose the extremes. An OLS is not appropriate because it would assume that
#the "distance" between 1 and 2 is the same as between 2 and 3, for example. This 
#is not the case, as shown by the distribution

#b)
m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)
#In consideration that the polr model treats coefficients with reverse signs, then
#the interpretation of the Europe coefficient (-0.123) indicates that positive 
#perceptions towards the EU are positively associated with perceptions of the economy. 

#c)
avg_slopes(m_ologit)
#As shown by the marginal effects, the values are positive at lower values of 
#pro-EU sentiment (indicating a negative association), while the opposite is true
#for higher values. This is aligned with our coefficient interpretation, showing that
#increased pro-EU sentiments are positively associated with positive perceptions
#on the economy.

#d)
predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))
#While females are shown as more likely to have pessimistic perceptions and males
#more likely to have optimistic ones, the differences are slim and their CI overlap.
#This suggests that gender is not a significant driver of economic perceptions in the
#studied sample.

#==================================
#2 Multinomial logit: vote choice
#==================================

#a)
BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)
#The model produces two sets of log-odds coefficients: Labour vs. Conservative and 
#Liberal Democrat vs. Conservative. The coefficient on Blair in the Labour vs. 
#Conservative equation is strongly positive: higher approval of Tony Blair is 
#associated with substantially greater log-odds of voting Labour rather than Conservative. 
#This makes intuitive sense — Blair was the Labour leader, so voters who rated 
#him favorably were much more likely to have voted for his party. By contrast, 
#the Blair coefficient in the Liberal Democrat vs. Conservative equation is
#expected to be smaller or near zero, since Blair approval does not strongly 
#differentiate Liberal Democrat voters from Conservatives.

#b)
avg_slopes(m_mlogit)
#The coefficient for Blair on Labour is 0.1156, indicating that a one-unit increase
#in Blair approval (on a 1/5 scale) is positively associated with voting for the 
#Labour party. 

#c)
#The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the 
#odds ratio between any two alternatives (e.g., Labour vs. Conservative) is unaffected 
#by the presence or characteristics of the third alternative (Liberal Democrats). 
#In the red bus / blue bus analogy, IIA fails because two alternatives are near-perfect 
#substitutes and removing one simply shifts its probability to the other rather 
#than distributing it proportionally. For British party choice, IIA is a moderate 
#concern: Labour and the Liberal Democrats are both centre-left parties, sharing
#some ideological space, so some voters may treat them as partial substitutes in 
#a way IIA cannot accommodate. The Conservatives, however, occupy a clearly distinct 
#ideological position (right-wing), so the three-party menu is not
#as degenerate as two buses of different colours. Overall, IIA is plausible for 
#Conservative vs. the others but is a more legitimate worry for the Labour/Liberal 
#Democrat distinction.

#===============================
#3 Poisson Regression: Publication Counts
#==============================
library(pscl)
library(AER)
library(MASS)
library(marginaleffects)
data(bioChemists)

#a)
summary(bioChemists$art)
var(bioChemists$art)
ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") +
  theme_minimal() +
  labs(title = "Publications in last 3 years of PhD",
       x = "Number of articles", y = "Count")
#The distribution of art is right-skewed, with a mode at zero and a long upper tail. 
#The mean is around 1.69 while the variance is approximately 3.71 — roughly twice 
#the mean. Under the Poisson assumption, the variance should equal the mean; a ratio 
#substantially above 1 indicates overdispersion. This pattern is a first signal 
#that a standard Poisson model may underestimate uncertainty and produce 
#anti-conservative standard errors.

#b)
m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)
#The coefficient of ment is 0.025543
exp(coef(m_pois)["ment"])
#Exponentiated, this is 1.025872. This suggests that mentors who publish more articles
#are associated with a boost in student publication by that factor. It is a small
#effect, but positive, suggesting that more productive mentors are associated with
#more output in students. The residual deviance is also substantially larger than 
#the degrees of freedom, suggesting overdispersion. 

#c)
dispersiontest(m_pois)
#The dispersion value is well above 1 (1.82454), suggesting overdispersion. This is
#supported by a p-value which indicates statistical signficance. Thus, we reject
#the null hypothesis of equidispersion. This tells us that the Poisson standard 
#erros underestimate uncertainty and inflate t-statistics. We need a model that
#accounts for overdispersion. 

#======================
#4 Negative Binomial Regression
#======================

#a)
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)
#The coefficient on ment is similar to the Poisson estimate, indicating that the 
#point estimate is reasonably stable. The key difference is in the standard errors: 
#the negative binomial model produces larger, more honest uncertainty
#estimates. The estimated overdispersion parameter theta (shown in the summary) 
#quantifies how much the variance exceeds the Poisson prediction; a smaller theta 
#means more severe overdispersion. Here theta is moderate, indicating meaningful 
#but not extreme extra-Poisson variation.

#b)
AIC(m_pois)
#3314.113
AIC(m_nb)
#3135.917

#The negative binomial AIC is substantially lower than the Poisson AIC, despite 
#the NB model having one additional parameter (theta). Under AIC, the improvement 
#in fit more than compensates for the added complexity. This confirms that 
#overdispersion is a genuine feature of the data, not noise, and that the negative 
#binomial is the more appropriate model for these publication counts.

#c)
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
#The predicted number of articles for men exceeds that for women, holding marital 
#status, number of young children, PhD prestige, and mentor productivity constant 
#at their sample means. The confidence intervals provide information on whether 
#this gender gap is statistically distinguishable: if the intervals do not overlap, 
#the difference is significant at conventional levels. The gap reflects a persistent 
#within-group gender difference in publication productivity that is not simply an 
#artefact of other observable characteristics.

#d)
#The Poisson model is not adequate for this dataset. The variance-to-mean ratio of art 
#is roughly double, the residual deviance far exceeds the degrees of freedom, and the 
#formal dispersiontest() rejects equidispersion with a p-value well below 0.001. The 
#negative binomial model, which adds a dispersion parameter to accommodate this extra 
#variation, achieves a substantially lower AIC and produces more reliable (wider) standard errors. 
#On substantive findings: the mentor’s productivity (ment) has a positive and 
#statistically significant effect, with an IRR slightly above 1 — each additional 
#mentor article is associated with a modest multiplicative increase in expected
#student articles, suggesting that working with a productive mentor confers a real, 
#if small, boost. Gender (fem) andnumber of young children (kid5) are both negative 
#and statistically significant: women publish fewer articles on average, and each 
#additional child under age 5 is associated with reduced output. PhD program prestige 
#(phd) and marital status (mar) are not statistically significant in the negative 
#binomial model. Together, the results point to early-career productivity being shaped 
#by mentor environment, gender, and family demands — patterns consistent with broader literature on PhD student outcomes in STEM fields.