#for assignment 2 we will run the following code 

library(tidyverse)
library (titanic)
library (gridExtra)
library (magrittr)
library(lsr)  
library(sciplot)
library(lm.beta)
library(psych) 
library(car) 
library(lmtest) 	
library(sandwich) 	
library(boot) 	
library(lmboot)

# as this assigment builds upon the first assignment, I will use the same dataset and exclude the same participants that had an error.
home_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

home_sample_1 %>% 
  filter(pain > 10) 

home_sample_1 %>% 
  filter(STAI_trait < 20)

home_sample_corrected <- home_sample_1 %>% 
  slice(-c(34,88))

# now that we have removed the errors, I will define the model described as the "initial model" that the fellow researcher defined.
initial_model <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + IQ + household_income + weight, data = home_sample_corrected)

# again I want to check for outliers in my model:
initial_model %>% 
  plot(which = 4)

#based on the plot, we see that no. 46, 84 and 85 are outliers so I want to to explore them further
home_sample_corrected %>% 
  slice(c(46, 84, 85))

#for good practice we have to check for the assumptions for regression again now that we defined a new model
describe(residuals(initial_model)) #normality
# both skewness and kurtosis for the model are < 1.0 which indicates that the assumption of normality is satisfied

residualPlots(initial_model) #linearity
# all p-values are above 0.05 so it is not statistically significant

ncvTest(initial_model) #homogeneity
# I do not have a statistically significant p-value

# The last part of this step is to check that there is no excess multicollinearity. If the values are above 3, that means the variables correlate with each other too much. 
vif(initial_model)
# since all values for the model are below 3, there is not an excess multicollinearity.

#since we have checked for assumptions for regression again and they are all statistically insignificant, so we can do the backward regression.  This method first fits a complete model with all of thespecified predictors, and then determins which predictor has the smallest amount of unique added explanatoryvalue to the model, and excludes it from the list of predictors, refitting the model without this predictor.
backward_model = step(initial_model, direction = "backward")

# again I want to check for outliers in my model:
backward_model %>% 
  plot(which = 4)

#based on the plot, we see that no. 46, 102 and 115 are outliers so I want to to explore them further
home_sample_corrected %>% 
  slice(c(46, 102, 115))

# we have a new model now called "backward_model" where we will check for assumptions for regression again for good practice
describe(residuals(backward_model)) #normality
# again skewness and kurtosis for the model are < 1.0 which indicates that the assumption of normality is satisfied

residualPlots(backward_model) #linearity
# all p-values are above 0.05 so it is not statistically significant

ncvTest(backward_model) #homogeneity
# I do not have a statistically significant p-value

# The last part of this step is to check that there is no excess multicollinearity. If the values are above 3, that means the variables correlate with each other too much. 
vif(backward_model)
# since all values for the model are below 3, there is not an excess multicollinearity.

# now I have determined the model based on the fellow researchers, and since I have a theory based model, defined as "model_3", I will use the two models to do a comparison based on AIC (and using the anova() function if appropriate).
# first step again will be to report the results of the backward model to test statistics (R2, F, df, and p value).

summary(backward_model)
#running a summary shows us that R2: 0.52, on F(,4, 153) = 41.41 P-value: < 0.01

# now i want to see the coefficients of the predictors in a table format (NB LADY: include the table in the report)
#running a code to do the coefficients table
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}
coef_table(backward_model)

# Now that I have the values, I am ready to write the regression equation of the backward model.
# 𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2 +…+ bn * Xn
# With my values, that gives me the numbers: y = 1.28 - 0.04 * age + 0.11 * pain_cat - 0.27 * mindfulness + 0.53 * cortisol_serum

# now the next step is to compare the models based on an AIC. First I will compare the initial model with the backwards model:
AIC (initial_model)
AIC (backward_model)
# the AIC is lower for the backward model with more than 2 points, which indicates that the backward model is a better fit than the initial model as a lower AIC indicates a better fit.

# Now I will compare the backward model with the theory based model, defined as "model_3".
AIC (backward_model)
AIC (model_3)

# the AIC is lower for the backward model, which must mean that the fellow student was right about her approach as her model is a better fit.
# ANOVA can be used for nested models, but if AIC shows statistic significance and ANOVA does not, then we will always look at the AIC as it is more reliable.
# I will do an ANOVA to see if this result can be confirmed
anova(backward_model, model_3)


# Now the next step is to test these two different models "backward_model" and "model_3" on new data. Similarly, this dataset has 160 participants. 
home_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

# like assignment 1, I want to check the data for errors and outliers by checking that all values are within the given ranges, and that skewness and kurtosis to see that the dataset is somehow normally distributed, meaning that all values are between -1.0 and 1.0.:
summary(home_sample_2)
describe(home_sample_2)

# what we see from the analysis is we do not need to exclude anything, although the skewness and kurtosis is outside the range of -1.0 and 1.0 for the variable "sex", because it just tells us that there is an unequal distribution among males and females, which is to be expected.

# next part is to make the prediction on pain by using the two models on the new file, to check their validity. 
#first i will check the predictions on my theory based model using the new dataset.
predict_pain_theory <- predict(model_3, home_sample_2)

# secondly i will check the prediction on the backwards model also using the new dataset.
predict_pain_backwards <- predict(backward_model, home_sample_2)

# now we want to check which ones of the two models have most errors by calculating the sum of squared residuals. 
Squared_residuals_theory <- sum((home_sample_2[,"pain"]-predict_pain_theory)^2)
Squared_residuals_theory
# the result I get is 243.8192

Squared_residuals_backwards <- sum((home_sample_2[,"pain"]-predict_pain_backwards)^2)
Squared_residuals_backwards
# the result I get is 249.5637

# the higher the number, the more errors the model has. This means that, based on the sums that I get on the prediction, the backwards model has more errors. That means that with the theory based model I would be able to predict pain more accurately than with the backwards model.