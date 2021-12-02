# first step is to install the necessary packages to run the codes

library(psych)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(tidyverse)

# then I will import the file from the assignment
home_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

#First we check for errors and outliers in the variables mentioned for model 2. 
#checking for errors by checking that all values are within the given ranges, and that kewness and kurtosis to see that the dataset is somehow normally distributed, meaning that all values are between -1.0 and 1.0.:
summary(home_sample_1)
describe(home_sample_1)

# what we see from the analysis is that there is a pain value that doesn't fit with the scale because the skewness and kurtosis is much outside -1.0 and 1.0 and maximum value is 55, but it should only be max 10 according to the description. Same goes for the STAI_trait where the skewness and kurtosis iss off and the scale is 20-80 but there is a minimum value of 4.20.
# for that reason, I know would like to see in which rows that error appears.

#Checking which rows have unusual data:
home_sample_1 %>% 
  filter(pain > 10) 
home_sample_1 %>% 
  filter(STAI_trait < 20)

# ID in this case means "row", so now the code tells us, that an error appears in row 88 and 34. Next step is now to cut them out of the dataset
home_sample_corrected <- home_sample_1 %>% 
  slice(-c(34,88))

# now that we have the data in order, we can start working with the data. The sex variable has to be converted from a category to a factor in order to be able to work with it as a category otherwise R wouldn't be able distinguish between male and female if it isn't defined as a factor.
home_sample_1 <- home_sample_1 %>%
  mutate(sex = factor(sex)) 

# Next step now is to do the two models and afterwards compare them

#model 1:
model_1 <- lm (pain ~ age + sex, data = home_sample_corrected)
model_2 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = home_sample_corrected)

#checking for outliers in my model (number 4 is to get a certain plot, and I want to see "Cook's distance plot"):
model_1 %>% 
  plot(which = 4)

#based on the plot, we see that no. 8, 23 and 46 are outliers so I want to to explore them further
home_sample_corrected %>% 
  slice(c(8,23,46))

#based on this code, I can see that all values are within the range so I will not exclude anything

# now i have defined both models according to the factors that should be included to check the final model to see if the assumptions of linear regression hold true: if the normality, linearity and homogeneity of variance and that there is no excess multicollinearity.
# First i check for normality for the two models
describe(residuals(model_1))
describe(residuals(model_2))
#skewness and kurtosis have to be smaller than 1 because the closer to zero the more normally distributed our model is. So we see that both skewness and kurtosis for models are < 1.0 which indicates that the assumption of normality is satisfied

# Second i check for linearity
#model 1:
residualPlots(model_1)
# both p-values are above 0.05 so it is not statistically significant

#model 2:
residualPlots(model_2)
# all p-values are above 0.05 so it is not statistically significant

# Thirdly i check for homogeneity of variance
ncvTest(model_1)
ncvTest(model_2)
# now i see that I do not have a statistically significant p-value for any of the models, so the homogeneity is satisfied as well as the p-value is above 0.05

# The last part of this step is to check that there is no excess multicollinearity. If the values are above 3, that means the variables correlate with each other too much. 
vif(model_1)
# since all values for the two models are below 3, there is not an excess multicollinearity. 

vif(model_2)
# here we see that values cortisol_serum and cortisol_saliva are too high, so we must exclude one of them to be able to do a hierarchical regression. And since cortisol_saliva is not as good a predictor as cortisol_serum based on the research presented in the assignment description, I will exclude cortisol_saliva from the dataset and form a new model without this variable, but run all the assumptions again since the model we are using has not been modified by removing a variable.
model_3 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = home_sample_corrected)
vif (model_3)

#again I want to check for outliers in my model:
model_3 %>% 
  plot(which = 4)

#based on the plot, we see that no. 46, 64 and 85 are outliers so I want to to explore them further
home_sample_corrected %>% 
  slice(c(46, 64, 85))

#based on this code, I can see that all values are within the range so I will not exclude anything


describe(residuals(model_3)) #normality
# both skewness and kurtosis for the model are < 1.0 which indicates that the assumption of normality is satisfied

residualPlots(model_3) #linearity
# all p-values are above 0.05 so it is not statistically significant

ncvTest(model_3) #homogeneity
# I do not have a statistically significant p-value

#now I am ready to compare the two models: first step will be to report the results of the model test statistics (R2, F, df, and p value).
# we get the R2, F, df, and p value by 
summary(model_1) 
#running a summary shows us that R2: 0.09, F(2, 155) = 7.23 and P-value: < 0.01

summary(model_3)
#running a summary shows us that the R2: 0.52, F(6, 151) = 27.59 and P-value: < 0.01

# now i want to see the coefficients of the predictors in a table format.
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

coef_table(model_1)
coef_table(model_3)
# (b: small beta, 95%CI lb: lower boundary, 95%CI ub: upper boundary)

# Now that I have the values, I am ready to write the regression equation of model 2 (which I have now defined as "model_3" after removing a variable)
# 𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2 +…+ bn * Xn
# With my values, that gives me the numbers: y=1.47 - 0.04 * age + 0.16 * sex - 0.01 * STAI_trait + 0.11 * pain_cat -0.28 * mindfulness + 0.57 * cortisol_serum
# The regression equation gives us the estimation of the effect of the predictors on pain for an individual

# Finally, the two models can be compared in terms of how much variance they explain of pain’s variability in the sample
# first I will compare them by doing a Akaike information criterion (AIC) and the lower the AIC is, the better the model fits to our data 
AIC (model_1) #574.1267
AIC (model_3) #479.2624
# Here I see that the AIC is lower for model 2 (defined as "model_3"), hence fits better to our data

# second I will compare them by analysing the F test statistic and p value of the model comparison returned by the anova() function.
anova(model_1, model_3)
#it now appears that model 3 explains more variance than 1, and that there is a statistically significant result, so the two models are unequal and model 2 (defined as "model_3")

