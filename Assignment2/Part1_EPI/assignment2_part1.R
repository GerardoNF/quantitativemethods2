setwd ("/Users/gerardonaranjo/Desktop/quantitativemethods2/quantitativemethods2/assignment2")
library (dplyr)
library(ggplot2)
library(broom)
library (modelsummary)
epi_data <-read.csv("qog_std_cs_jan26.csv")
summary(epi_data)

#Renaming and dropping certain variables
clean_epi_data<-epi_data %>%
  select(
    country = cname,
    epi = epi_epi,
    women_parl = wdi_wip,
    gov_eff = wbgi_gee,
    green_seats = cpds_lg
  )
summary (clean_epi_data)

#1.2 Exploratory Visualization
#Creating the plot
p1<-ggplot(clean_epi_data, aes(x=women_parl, y=epi,)) +
  geom_point() + 
  geom_smooth(method = "lm", se=TRUE) +
  labs(
    x= "Women in Parliament Percentage",
    y= "Environmental Performance Index",
    title = "Women in Parliament and EPI Scores"
  ) +
  theme_minimal()
p1
ggsave("epi_scatter.png", plot = p1, width = 7, height = 5)

#The plot displays a positive correlation between both
#variables, showing that countries with more 
#women in parliament tend to have higher EPI scores. 

#1.3 Running the bivariate regression
m1 <- lm(epi ~ women_parl, data=clean_epi_data)
#Summary of the results
summary(m1)
#The coefficient in women_parl indicates the change we can
#expect in EPI scores for each additional percentage unit
#in the amount of women in parliament, in this case, 
#suggesting that EPI scores go up for about 0.308 for every
#extra 1% in women in parliament.

#Calculating the predicted IQR with percentiles
p25<-quantile(clean_epi_data$women_parl, 0.25, na.rm = TRUE)
p75<-quantile(clean_epi_data$women_parl, 0.75, na.rm = TRUE)

coef(m1) ["women_parl"] * (p75 - p25)
#The predicted IQR is around 5.64

#1.4 Multiple Regression
#Including gov. effectiveness as control
m2 <- lm(epi ~ women_parl + gov_eff, data=clean_epi_data) 
#Summary m2
summary(m2)

#Once government effectiveness is controlled for, the 
#coefficient of women_parl goes down signficantly. 
#This suggests that a big portion of the previously found
#effect was driven by government effectiveness.

#1.5 Demonstrating OVB
#The formula says that the bivariate coefficient must
#equal the multiple model coefficients times the aux coeff.

#We can extract the first three with:
beta1_biva <- coef(m1) ["women_parl"]
beta1_multi <- coef(m2) ["women_parl"]
beta2_multi <- coef(m2) ["gov_eff"]
#Then run the auxiliary regression and create delta
aux <- lm(gov_eff ~ women_parl, data = clean_epi_data)
delta <- coef(aux) ["women_parl"]
#Now the math from the formula
beta1_multi + beta2_multi * delta
beta1_biva
#Running both of the lines above, we see that the right 
#side of the equation is= 0.3307, and the left side is
# equal to 0.3078

#Interpretation
#The coefficient in women_parl changed because government
#effectiveness was an omitted variable driving much of the
#fluctuation in the relationship between women in parl. and 
#epi scores, suggesting that gov. eff. is related to both.

#1.6 Robust SEs
#First, the classical SEs
modelsummary(m2, output = "markdown")
#Then, the one using robust SE
modelsummary(m2, vcov = "robust", output = "markdown")

#Interpretation
#The SE did not change dramatically, suggesting that 
#this was not interfering with the findings.

#1.7 The Results
modelsummary(list("Bivariate" = m1,
                  "Multiple" = m2),
             vcov = "robust")

#Plotting the coefficients
p2<-modelplot(list("Bivariate" = m1, 
               "Multiple" = m2),
          vcov = "robust")
p2
ggsave("coeff_plot.png", plot=p2, width=7, height=5)