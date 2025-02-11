# This Code showcases the development of a multivariate logistic regression model using both categorical and continuous predictors of a basic level "trauma patients survival data" to predict a health-related outcome. The model was built using R and fine-tuned through various preprocessing, diagnostic, and optimization techniques.

#Key steps in the project:

#1)Model Construction: A multivariate logit model was built incorporating both categorical and continuous predictors.

#2)Data Transformation: Nonlinear continuous variables were identified using a Lowess smoother and transformed using the FracPoly method.

#3)Model Optimization: Influential data points were identified and removed based on diagnostic tests, improving model fit.

#4)Model Selection: By eliminating predictor terms with higher p-values, the model was optimized for better performance.

#5)Enhanced Predictive Accuracy: The addition of the "physical fitness" variable significantly improved the model’s predictive power.--- CONVERT THIS PARA ALONE- NOTHING ELSE  TO R PROGRAM FORMAT - MENTION THAT THIS IS SURVIVAL DATA
# Load necessary libraries
library(haven)      # For reading Stata .dta files
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(splines)    # For fractional polynomial transformations
library(pROC)       # For ROC analysis
library(caret)      # For classification metrics

# Load the dataset
mortality <- read_dta("mortality.dta")

# Data Cleaning: Filtering only val == 0 observations
mortality <- mortality %>% filter(val == 0)

# Summary Statistics for Continuous and Categorical Variables
summary_stats <- mortality %>%
  group_by(died) %>%
  summarise(
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    sbp_mean = mean(sbp, na.rm = TRUE),
    sbp_sd = sd(sbp, na.rm = TRUE),
    rr_mean = mean(rr, na.rm = TRUE),
    rr_sd = sd(rr, na.rm = TRUE),
    gcs_mean = mean(gcs, na.rm = TRUE),
    gcs_sd = sd(gcs, na.rm = TRUE),
    asaps_mean = mean(asaps, na.rm = TRUE),
    asaps_sd = sd(asaps, na.rm = TRUE)
  )
print(summary_stats)

# Check missing values
table(is.na(mortality))

# Visualizing relationships using LOWESS Smoothing Curves
lowess_plot <- function(x_var, y_var, data) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = paste("LOWESS Curve for", x_var, "vs", y_var))
}

lowess_plot("sbp", "died", mortality)
lowess_plot("gcs", "died", mortality)
lowess_plot("rr", "died", mortality)
lowess_plot("age", "died", mortality)
lowess_plot("asaps", "died", mortality)

# Creating interaction terms
mortality <- mortality %>%
  mutate(
    prod_sbp_rr = sbp * rr,
    prod_sbp_gcs = sbp * gcs,
    prod_rr_gcs = rr * gcs,
    prod_age_gcs = age * gcs,
    prod_age_rr = age * rr,
    prod_age_sbp = age * sbp,
    prod_asaps_age = asaps * age,
    prod_asaps_rr = asaps * rr,
    prod_asaps_sbp = asaps * sbp
  )

# Model 1: Basic Logistic Regression Model (Initial Model)
model <- glm(died ~ age + sbp + rr + gcs, data = mortality, family = binomial)

# Model 2: Extended Model (Adding Interaction Terms)
model_extended <- glm(died ~ age + sbp + rr + gcs + asaps +
                      prod_sbp_rr + prod_sbp_gcs + prod_rr_gcs + prod_age_gcs, 
                      data = mortality, family = binomial)

# Model 3: Refitted Model (Removing Insignificant Predictors)
model_refit <- glm(died ~ age + sbp + rr + gcs + asaps + 
                   prod_age_rr + prod_sbp_gcs, 
                   data = mortality, family = binomial)

# Model 4: Optimized Model with Physical Fitness Variable
model_asaps <- glm(died ~ age + sbp + rr + gcs + asaps + physical_fitness + 
                   prod_age_rr + prod_sbp_gcs + prod_asaps_age + prod_asaps_rr + prod_asaps_sbp,
                   data = mortality, family = binomial)

summary(model_asaps)

# Model Fit Statistics
AIC(model, model_extended, model_refit, model_asaps)
BIC(model, model_extended, model_refit, model_asaps)

# Goodness-of-fit Test
anova(model_asaps, test = "Chisq")

# ROC Curve Analysis
mortality$predicted_asaps <- predict(model_asaps, type = "response")
roc_curve_asaps <- roc(mortality$died, mortality$predicted_asaps)
plot(roc_curve_asaps, main = "ROC Curve for Model with asaps")
auc(roc_curve_asaps)
