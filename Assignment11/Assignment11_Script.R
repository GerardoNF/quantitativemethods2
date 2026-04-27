#=======
#Setting Up
#========
library(here)       
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(modelsummary)
library(marginaleffects)
library(readstata13)
library(haven)
library(readr)
library(ggrepel)     

#==============
#1 Loading the datasets
#==============
educational_gaps <- read_csv(here("educational_gaps.csv")) #Dataset with PPI and countries
tax_raw <- read_csv(
  here("API_GC.TAX.TOTL.GD.ZS_DS2_en_csv_v2_1167.csv"),   #Dataset about tax revenue
  skip = 4
)
gdp_raw <- read_csv(
  here("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_245.csv"),   #Dataset about GDP in per year
  skip = 4
)

#=====================
#2 Cleaning the data
#=====================
# Extract only country code and 2019 tax revenue
tax_2019 <- tax_raw %>%
  select(iso3 = `Country Code`, tax_revenue_2019 = `2019`)

#GDP for 2019
gdp_pc_2019 <- gdp_raw %>%
  select(iso3 = `Country Code`, gdp_pc_2019= `2019`)

# Merge into educational_gaps by ISO3 country code
educational_gaps_merged <- educational_gaps %>%
  left_join(gdp_pc_2019, by = "iso3") %>%
  left_join(tax_2019, by = "iso3")

#Creating final dataset for GDP model
df_clean <- educational_gaps_merged[complete.cases(educational_gaps_merged[, c("education", "gdp_pc_2019", "youth_pop_share")]), ]

#Dataset to keep track of missing observations for tax data
df_taxesmissing <- educational_gaps_merged[complete.cases(educational_gaps_merged[, c("education", "gdp_pc_2019", "tax_revenue_2019", "youth_pop_share")]), ]

#Constructing per capita variable for tax revenue
df_clean <-df_clean %>%
  mutate(tax_pc= (tax_revenue_2019 / 100) * gdp_pc_2019)
#Creating the log value for tax per capita
df_clean <- df_clean %>%
  mutate(log_tax_revenue_pc = log(tax_pc))
#Creating the log variable for GDP per capita
df_clean <- df_clean %>%
  mutate(log_gdppc_2019 = log(gdp_pc_2019))
df_taxesmissing <- df_taxesmissing %>%
  mutate(log_gdppc_2019 = log(gdp_pc_2019))

#===============================
#3 Model Building
#==============================
#Computing the model of education in terms of GDP per capita and Youth Population
model1 <- lm(education ~ log_gdppc_2019 + youth_pop_share, data = df_clean)
summary(model1)
modelsummary(
  list("Expected LAYS" = model1),
  stars = TRUE,
  title = "Education as a Function of GDP per Capita and Youth Population",
  vcov = "robust",
  output = "model1_summary.tex",
  escape=FALSE)

#Computing the model of education in terms of Tax Revenue per capita and Youth Population
model2 <- lm(education ~ log_tax_revenue_pc + youth_pop_share, data = df_clean)
summary(model2)
modelsummary(model2,
             stars = TRUE,
             title = "Education as a Function of Tax Revenue and Youth Population Share",
             coef_rename = c("log_tax_revenue_pc" = "Tax Revenue Per Capita",
                             "youth_pop_share" = "Youth Population Share"),
             vcov = "robust",
output = "model2_summary.tex",
escape= FALSE)

#Creating model with missing tax observations for GDP as robustness check
model_gdp_restricted <- lm(education ~ log_gdppc_2019 + youth_pop_share, 
                           data = df_taxesmissing)
summary(model_gdp_restricted)
modelsummary(model_gdp_restricted,
             stars = TRUE,
             title = "Education as a Function of GDP per Capita and Youth Population (Restricted Sample)",
             coef_rename = c("log_gdppc_2019" = "Log GDP Per Capita",
                             "youth_pop_share" = "Youth Population Share"),
             vcov = "robust",
             output = "model3_summary.tex",
             escape = FALSE)

#Creating a table comparing all three models
modelsummary(
  list("GDP (Full Sample)" = model1,
       "Tax Revenue per Capita" = model2,
       "GDP (Restricted Sample)" = model_gdp_restricted),
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c("log_gdp_pc" = "Log GDP per Capita",
                  "log_tax_revenue_pc" = "Log Tax Revenue per Capita",
                  "youth_pop_share" = "Youth Population Share"),
  notes = c("Standard errors in parentheses.",
            "Expected learning-adjusted years of schooling (LAYS) regressed on structural predictors.",
            "Tax revenue model restricted to countries with available fiscal data (N = 116)."),
  title = "Structural Predictors of Learning-Adjusted Years of Schooling",
  output = "comparison_table.tex"
)

#===================
#4 Computing the education gap variable
#===================

#Adding the predicted values (for both samples)
df_clean$predicted_education <- predict(model1)
df_taxesmissing$predicted_education <- predict(model2)

#Computing the gap (for both samples)
df_clean$education_gap <- (df_clean$education - df_clean$predicted_education)
df_taxesmissing$education_gap <- (df_taxesmissing$education - df_taxesmissing$predicted_education)

#====================================
#Visualizing the Gap for GDP Model
ggplot(df_clean, aes(x = predicted_education, y = education)) +
  geom_point(color = "gray30", alpha = 0.7, size = 1.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "black",
              linewidth = 0.8) +
  geom_text_repel(
    data = subset(df_clean, abs(education_gap) > 1.3),
    aes(label = country),
    size = 2.5,
    max.overlaps = 20
  ) +
  labs(
    title = "Observed vs. Expected Learning-Adjusted Years of Schooling",
    subtitle = "Expected values derived from structural model (Log GDP per capita and youth population share)",
    x = "Predicted LAYS (Structural Expectation)",
    y = "Observed LAYS",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("Figure2_MappingGap.png",
       width = 10,
       height = 6,
       dpi = 300)

#========================================
#Visualizing the Gap for Tax Revenue Model
ggplot(df_taxesmissing, aes(x = predicted_education, y = education)) +
  geom_point(color = "gray30", alpha = 0.7, size = 1.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "black",
              linewidth = 0.8) +
  geom_text_repel(
    data = subset(df_taxesmissing, abs(education_gap) > 1.3),
    aes(label = country),
    size = 2.5,
    max.overlaps = 20
  ) +
  labs(
    title = "Observed vs. Expected Learning-Adjusted Years of Schooling",
    subtitle = "Expected values derived from structural model (Log Tax Revenue per Capita and Youth Population Share)",
    x = "Predicted LAYS (Structural Expectation)",
    y = "Observed LAYS",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("Figure4_TaxandLAYS.png",
       width = 10,
       height = 6,
       dpi = 300)

#===================================
#5 Visualizing the predictive strength of models
#===================================

#GDP model first
library(ggplot2)
library(viridis)

ggplot(df_clean, aes(x = log_gdppc_2019,
                     y = education,
                     color = youth_pop_share)) +
#Scatter points
  geom_point(size = 2.6, alpha = 0.85) +
#Regression line
  geom_smooth(method = "lm",
              se = FALSE,
              color = "black",
              linewidth = 0.9) +
#Professional color scale
  scale_color_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Youth Population Share (%)"
  ) +
#Labels
  labs(
    title = "Structural Predictors of Educational Attainment",
    x = "Log GDP per Capita",
    y = "Learning-Adjusted Years of Schooling"
  ) +
#Journal-style theme
  theme_classic(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )
ggsave("Figure1_Structural_Predictors.png",
       width = 8,
       height = 6,
       dpi = 300)
#--------------------------------------
#Testing the tax-revenue one
ggplot(df_clean, aes(x = log_tax_revenue_pc,
                     y = education,
                     color = youth_pop_share)) +
#Scatter points
  geom_point(size = 2.6, alpha = 0.85) +
#Regression line
  geom_smooth(method = "lm",
              se = FALSE,
              color = "black",
              linewidth = 0.9) +
#Professional color scale
  scale_color_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Youth Population Share (%)"
  ) +
#Labels
  labs(
    title = "Structural Predictors of Educational Attainment",
    x = "Log Tax Revenue per Capita",
    y = "Learning-Adjusted Years of Schooling"
  ) +
#Journal-style theme
  theme_classic(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )
ggsave("Figure5_Structural_Predictors_Tax.png",
       width = 8,
       height = 6,
       dpi = 300)

#======================================
#6 Testing Concern about Non-Linear Relationship
#=======================================

# Model with interaction term
model_interaction <- lm(education ~ log_gdppc_2019 * youth_pop_share, data = df_clean)
modelsummary(model_interaction,
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             coef_rename = c("log_gdppc_2019" = "Log GDP per Capita",
                             "youth_pop_share" = "Youth Population Share"),
             title = "Structural Predictors of Learning-Adjusted Years of Schooling",
             output = "interaction_table.tex"
)

# Compare the models to test differences
summary(model_interaction)
AIC(model1, model_interaction)
anova(model1, model_interaction)


#Visualizing the effect
df_clean$gdp_group <- cut(df_clean$log_gdppc_2019, 
                          breaks = quantile(df_clean$log_gdppc_2019, 
                                            probs = c(0, 0.33, 0.66, 1), 
                                            na.rm = TRUE),
                          labels = c("Low Income", "Middle Income", "High Income"),
                          include.lowest = TRUE)

#Plotting difference in effect at varying GDP levels
ggplot(df_clean, aes(x = youth_pop_share, y = education, 
                     color = gdp_group, fill = gdp_group)) +
  geom_point(alpha = 0.4, size = 1.8, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.12, linewidth = 0.8) +
  scale_color_manual(values = c("Low Income"  = "#2C3E6B",
                                "Middle Income" = "#A63D2F", 
                                "High Income" = "#2A6B4F")) +
  scale_fill_manual(values = c("Low Income"  = "#2C3E6B",
                               "Middle Income" = "#A63D2F", 
                               "High Income" = "#2A6B4F")) +
  labs(x = "Youth Population Share (%)",
       y = "Learning-Adjusted Years of Schooling (LAYS)",
       color = NULL,
       fill = NULL,
       title = "Demographic Burden and Educational Attainment by Income Level",
       caption = "Note: Income groups defined by tertiles of log GDP per capita (2019).\nShaded bands represent 95% confidence intervals. N = 147.") +
  theme_classic() +
  theme(
    # Text
    plot.title = element_text(size = 11, face = "bold", hjust = 0,
                              margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0,
                                margin = margin(t = 8)),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9, color = "black"),
    
    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    
    # Panel
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Margins
    plot.margin = margin(12, 12, 8, 12)
  )

ggsave("interaction_plot.png", width = 6.5, height = 5, dpi = 300)