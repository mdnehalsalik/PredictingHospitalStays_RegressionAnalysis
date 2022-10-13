# PredictingHospitalStays_RegressionAnalysis

This project focuses on analyzing a dataset containing characteristics of hospitals participating in the SENIC project. The SENIC data contains 113 samples (rows) and 11 variables. We will fit a multiple linear regression (MLR) model to predict the average length of stay of all patients in a hospital. During the project, we performed several sets of hypothesis tests associated with this model using R to come to a final most appropriate model. At the end of the project, we aimed to achieve the below set of results.
➢ Model fitting together with justification of the model, assumptions checking, and remediation.
➢ Computing the point estimates of regression coefficients using a design matrix and normal equations.
➢ Interpreting adjusted 𝑅2, denoted by 𝑎𝑑𝑗.𝑅2.
➢ Conducting a hypothesis test for model adequacy using the overall F-test.
➢ Conducting multiple hypothesis tests for the regression coefficients based on t-test.
➢ Obtaining confidence intervals for individual regression coefficients.
➢ Making a conclusion based on the test and the confidence intervals.
➢ Model diagnostics and transformation if required.

We started the model fitting by first importing the dataset and then performing the exploratory data analysis.
Exploratory data analysis provided some insights into how the dataset looks like. 
The data set with 19 observations is too small observation to make a good multiple linear regression model. 
Latter we fit a full model based on all the predictors in the model and found that man assumptions were getting violated. 
Then we looked for the multicollinearity in the model and removed the predictor responsible for multicollinearity based on the performed analysis. We did stepwise regression and looked into all the criteria required to select the final model. 
Once the final model was selected, we performed the assumption diagnostics and assumption checking. Based on the assumption checking, the normality assumption of the model was violated. 
Latter we performed the box cox transformation to fix the normality issue of the model and finally got our transformed model.
