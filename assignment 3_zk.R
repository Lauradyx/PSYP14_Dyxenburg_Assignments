
library(tidyverse)
library(titanic)
library(gridExtra)
library(magrittr)
library(lsr)  
library(sciplot)
library(lm.beta)
library(psych) 
library(car) 
library(lmtest) 	
library(sandwich) 	
library(boot) 	
library(lmboot)
library(merTools)
library(influence.ME)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn) 
library(cAIC4)


#I will run the new dataset that should be used for this assignment
data_file_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")

#As always, I check for errors and outliers in the data set. 
#checking for errors by checking that all values are within the given ranges, and that skewness and kurtosis to see that the dataset is somehow normally distributed, meaning that all values are between -1.0 and 1.0.:
#however, before checking the data set I have to change all variables into factors in order to analyse it. The "sex" variable is again characters, so it should be mutated.

data_file_3_corrected <- data_file_3 %>%
  mutate(sex = factor(sex)) 

#For data file 3
summary(data_file_3_corrected)
describe(data_file_3_corrected)

#it appears that there are several errors: there is an error under the variable "sex", where "women" should be changed to "female" and perhaps under the variable "household_income" where the household income is in minus and that seems to be strange since an income will be a positive amount. For that reason and since I have no idea how this mistake occured, I will cut out this row
#first I change the error in the variable "sex"
data_file_3_corrected %>% 
  filter(sex == "woman")
#when running this code I see that ID 25 has the error

data_file_3_corrected=data_file_3_corrected %>%
  mutate(sex=recode(sex,
                    "woman"="female"))

#then I cut out the ID with an error in household
data_file_3_corrected %>% 
  filter(household_income < 0)

#Now the code tells us, that an error appears in row 2. Next step is now to cut them out of the dataset
data_file_3_corrected <- data_file_3_corrected %>% 
  slice(-c(2))

# now that we have removed the errors, I will build a linear mixed model accounting for the clustering of the data at different hospital sites. so we are going to have random intercept for hopsital ID but fixed intercept for the other predictors. 
# for this we use lmer function, to get mixed effects. 
linear_model_random_hosp <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_file_3_corrected)

# now I have determined the model and want to compare it to my theory based model defined in part 1.
# first I would like to see model coefficients and the confidence intervals of the coefficients for all fixed effect predictors.
summary(linear_model_random_hosp) 
# the summary gives me p-values it shows that age, pain_category and cortisol_serum are important predictors of my model as the p-values are the most significant

# now i want to see the coefficients of the predictors in a table format but first i need to load some packages for the lmer function
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

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
#getting coefficients for linear model with random intercept(Confidence Interval and, beta values):   
confint(linear_model_random_hosp)

# Confint shows that the CI intervals show me if something is statistically significant. if e.g. the lower boundary and upper boundary are on each side of a 0 then it is not statistically significant.

stdCoef.merMod (linear_model_random_hosp) 
# gives me the std. beta coefficients. From that I will see which of my predictors gives me the most variance of the predicted variable - is in other words most impacting 
# stdcoef = std. beta. The ones that explains the most variance is cortisol_serum but also pain_cat.   

#I also want to get the coefficients of the theory based model (Confidence Interval, beta values):   
coef_table(model_3)
#what i see from these coefficients is that also for this model, cortisol_serum and pain_cat are the most powerful predictors in this theory based model, but a difference is that pain_cat is higher here whereas it had a lower value than cortisol_serum in the random model.
#now I compare coefficients from the two models. 

# to get a marginal and conditional R2. 
r.squaredGLMM(linear_model_random_hosp) #
# the marginal R2 is 0.38 so the fixed predictors explains 38% of variance in our model. This is because with marginal R2 you will see how much variance can be explained with the fixed predictors (excluding random predictors which in this case is hospital). Conditional R2 is 0.46 so all predicters including fixed and random explains 46% of the variance in the model)

#Next step is to use the regression equation we made to predict pain in part 2 of the assignment, and use to predict pain within a new data set, data file 4.
data_file_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

#As always, I check for errors and outliers in the data set. 
#checking for errors by checking that all values are within the given ranges, and that skewness and kurtosis to see that the dataset is somehow normally distributed, meaning that all values are between -1.0 and 1.0.:
#however, before checking the data set I have to change all variables into factors in order to analyse it. The "sex" variable is again characters, so it should be mutated.
data_file_4_corrected <- data_file_4 %>%
  mutate(sex = factor(sex)) 

summary(data_file_4_corrected)
describe(data_file_4_corrected)
# after checking the dataset, it appears there are no errors. So now i can apply the regression equation on the dataset
predict_pain_random <- predict(linear_model_random_hosp, data_file_4_corrected, allow.new.levels = TRUE)
# I will now use this funtion to compute the variance explained by the model on data file 4 by using the formula we learned in class: 1-(RSS/TSS)
RSS <- sum((data_file_4_corrected[,"pain"]- predict_pain_random)^2)
RSS

mean_model <- lm (pain ~ 1, data=data_file_4_corrected)
TSS <- sum((data_file_4_corrected$pain - predict(mean_model))^2)
TSS

R2 <- 1 - (RSS/TSS)
R2

#by calculating 1-(RSS/TSS) in R, I get 0.39 which means that this predicted model explains 39% of variance in data file 4.
# this number must be compared to the marginal and conditional R2 values computed for the model on data file 3. The marginal R2 explained 38% and conditional explained 46% so this predicted model explaining 38% is not as accurate as the conditional R2. 

# now we have to build a new linear mixed model on dataset 3 only including the most influential predictor from the previous model which is cortisol_serum as it had the highest std. beta value. 
linear_model_random_slope <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_file_3_corrected)

# now with this model I have to visualise the fitted regression lines for each hospital separately. 
data_file_3_corrected = data_file_3_corrected %>% 		
  mutate(predict_slope = predict(linear_model_random_slope))

data_file_3_corrected %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 3) +		
  geom_line(color='red', aes(y=predict_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

#now we have the graphs for each hospital with random intercept and random slope.

#for the discussion part, the last step now is to see whether the random intercept or the random slope model is a better fit for the data. I will do this by using the AIC comparison again as for part 2.
cAIC(linear_model_random_hosp)$caic 
cAIC(linear_model_random_slope)$caic #AIC for this model is 671

#AIC for the linear model for random hospital variable is 618 and for random slope is 671 and as a lower AIC is more desired, the random hopspital model is a better fit
