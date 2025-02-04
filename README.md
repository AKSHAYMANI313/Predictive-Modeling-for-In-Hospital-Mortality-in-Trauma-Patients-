# Predictive-Modeling-for-In-Hospital-Mortality-in-Trauma-Patients-
This repository showcases the development of a multivariate logistic regression model using both categorical and continuous predictors to predict a health-related outcome. The model was built using STATA and fine-tuned through various preprocessing, diagnostic, and optimization techniques.

Key steps in the project:

1)Model Construction: A multivariate logit model was built incorporating both categorical and continuous predictors.

2)Data Transformation: Nonlinear continuous variables were identified using a Lowess smoother and transformed using the FracPoly method.

3)Model Optimization: Influential data points were identified and removed based on diagnostic tests, improving model fit.

4)Model Selection: By eliminating predictor terms with higher p-values, the model was optimized for better performance.

5)Enhanced Predictive Accuracy: The addition of the "physical fitness" variable significantly improved the model’s predictive power.
