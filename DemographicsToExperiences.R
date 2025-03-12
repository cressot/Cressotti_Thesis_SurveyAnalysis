
#### Start ####

library(ggplot2)      # for visualization
library(reshape2)     # for reshaping data
library(corrplot)     # for correlation plot
library(dplyr)        # for data manipulation
library(ggcorrplot)   # for enhanced correlation heatmap

source("~/Desktop/PSU 2024-2025/R_Tools_Examples_Script/Scripts/cor.matrix.r")

SurveyResults_AND_Demographics_Clean <- na.omit(SurveyResults_AND_Demographics)

str(SurveyResults_AND_Demographics_Clean$AGE_65_P)


# Convert to numeric
SurveyResults_AND_Demographics_Clean$AGE_65_P <- as.numeric(SurveyResults_AND_Demographics_Clean$AGE_65_P)
SurveyResults_AND_Demographics_Clean$LOW_INC_P <- as.numeric(SurveyResults_AND_Demographics_Clean$LOW_INC_P)
SurveyResults_AND_Demographics_Clean$POC_P <- as.numeric(SurveyResults_AND_Demographics_Clean$POC_P)
SurveyResults_AND_Demographics_Clean$ED_BACH_P <- as.numeric(SurveyResults_AND_Demographics_Clean$ED_BACH_P)
#### end ####


#### Location w/ Yard Percents ####

# ANOVA to test if yard type percentage differs by location (Q15_Place)
model_grass_location <- aov(Q11_1_Grass ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_grass_location)
model_hardscape_location <- aov(Q11_2_Hardscape ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_hardscape_location)
model_veg_location <- aov(Q11_3_Veg ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_veg_location)
model_mulch_location <- aov(Q11_4_Mulch ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_mulch_location)
model_shrub_location <- aov(Q11_5_Shrub ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_shrub_location)
model_tree_location <- aov(Q11_6_Tree ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_tree_location)
model_other_location <- aov(Q11_7_Other ~ Q15_Place, data = SurveyResults_AND_Demographics)
summary(model_other_location)

# Check normality for yard type percentage
shapiro.test(SurveyResults_AND_Demographics$Q11_1_Grass)
#### end ####


#### Location w/ Experience ####
# First, make sure your place variable is a factor
SurveyResults_AND_Demographics$Q15_Place <- factor(SurveyResults_AND_Demographics$Q15_Place,
                                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                                                   labels = c("North Plains", "Bethany Lake", "Council Crest Park", 
                                                              "Marquam Nature Park", "George Himes Park", "Moonshadow Park", 
                                                              "Gabriel Park", "Sellwood Park", "Ross Island", 
                                                              "Johnson Creek City Park", "Foster Floodplains Natural Area", 
                                                              "Jenne Butte Park", "Gresham-Barlow School District - West Campus", 
                                                              "East Gresham Elementary School", "Kelly Creek Elementary School", 
                                                              "West Orient Middle School"))

# Assuming you have the ordered experience variable (e.g., Q3_1_CoyNE) in your dataset
# Here we'll fit an ordinal logistic regression model with the ordered experience and place
library(MASS)  # For the 'polr' function
# Fit the ordinal logistic regression model
model_ordinal <- polr(Experience_with_Coy_Ordered ~ Q15_Place, data = SurveyResults_AND_Demographics_Clean, method = "logistic")
# Summary of the model
summary(model_ordinal)

odds_ratio <- exp(0.03013)
print(odds_ratio)


library(MASS)  # For the 'polr' function
# Fit the ordinal logistic regression model
model_ordinal_cat <- polr(Experience_with_Cat_Ordered ~ Q15_Place, data = SurveyResults_AND_Demographics_Clean, method = "logistic")
# Summary of the model
summary(model_ordinal_cat)

odds_ratio <- exp(0.03013)
print(odds_ratio)





# First, ensure that your place variable is a factor
SurveyResults_AND_Demographics$Q15_Place <- as.factor(SurveyResults_AND_Demographics$Q15_Place)

# Create a vector of experience types (assuming the binary columns are present)
experience_types_binomial <- c("Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q3_4_CoyFS", 
                               "Q3_5_CoyFed", "Q3_6_CoyPA", "Q3_7_CoyOther")

# Create an empty list to store results
logistic_results_binomial <- list()

# Loop through each binomial experience column and fit a logistic regression model
for (exp in experience_types_binomial) {
  
  # Clean the response variable to make sure it's binary (0 or 1)
  # Replace any non-0/1 values (e.g., NA or -99) with NA
  SurveyResults_AND_Demographics[[exp]] <- ifelse(SurveyResults_AND_Demographics[[exp]] == -99 | 
                                                    is.na(SurveyResults_AND_Demographics[[exp]]), NA, 
                                                  SurveyResults_AND_Demographics[[exp]])
  
  # Only keep rows where the experience column is binary (0 or 1)
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics[[exp]]) & 
                                                   SurveyResults_AND_Demographics[[exp]] %in% c(0, 1), ]
  
  # Fit a logistic regression model for each experience type with place
  log_model_binomial <- glm(as.formula(paste(exp, "~ Q15_Place")),  
                            data = cleaned_data, 
                            family = binomial(link = "logit"))
  
  # Store the results in the list
  logistic_results_binomial[[exp]] <- summary(log_model_binomial)
}

# Print the logistic regression results for binomial experience
logistic_results_binomial




# Create an empty list to store Chi-square results
chi_square_results_binomial <- list()

# Loop through each binomial experience column and run a Chi-square test
for (exp in experience_types_binomial) {
  # Create a contingency table for the binomial experience and place
  chi_sq_table <- table(SurveyResults_AND_Demographics[[exp]], SurveyResults_AND_Demographics$Q15_Place)
  # Perform the Chi-square test
  chi_test_binomial <- chisq.test(chi_sq_table)
  # Store the results
  chi_square_results_binomial[[exp]] <- chi_test_binomial
}
# Print the Chi-square test results for binomial experience
chi_square_results_binomial




# First, ensure that your place variable is a factor
SurveyResults_AND_Demographics$Q15_Place <- as.factor(SurveyResults_AND_Demographics$Q15_Place)

# Create a vector of experience types (assuming the binary columns are present)
experience_cats_types_binomial <- c("Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q4_4_CatFS", 
                               "Q4_5_CatFed", "Q4_6_CatTouch", "Q4_7_CatPA", "Q4_8_CatOther")

# Create an empty list to store results
logistic_results_binomial <- list()

# Loop through each binomial experience column and fit a logistic regression model
for (exp in experience_cats_types_binomial) {
  
  # Clean the response variable to make sure it's binary (0 or 1)
  # Replace any non-0/1 values (e.g., NA or -99) with NA
  SurveyResults_AND_Demographics[[exp]] <- ifelse(SurveyResults_AND_Demographics[[exp]] == -99 | 
                                                    is.na(SurveyResults_AND_Demographics[[exp]]), NA, 
                                                  SurveyResults_AND_Demographics[[exp]])
  
  # Only keep rows where the experience column is binary (0 or 1)
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics[[exp]]) & 
                                                   SurveyResults_AND_Demographics[[exp]] %in% c(0, 1), ]
  
  # Fit a logistic regression model for each experience type with place
  log_model_cats_binomial <- glm(as.formula(paste(exp, "~ Q15_Place")),  
                            data = cleaned_data, 
                            family = binomial(link = "logit"))
  
  # Store the results in the list
  logistic_results_binomial[[exp]] <- summary(log_model_cats_binomial)
}

# Print the logistic regression results for binomial experience
logistic_results_binomial




# Create an empty list to store Chi-square results
chi_square_results_binomial <- list()

# Loop through each binomial experience column and run a Chi-square test
for (exp in experience_types_binomial) {
  # Create a contingency table for the binomial experience and place
  chi_sq_table <- table(SurveyResults_AND_Demographics[[exp]], SurveyResults_AND_Demographics$Q15_Place)
  # Perform the Chi-square test
  chi_test_binomial <- chisq.test(chi_sq_table)
  # Store the results
  chi_square_results_binomial[[exp]] <- chi_test_binomial
}
# Print the Chi-square test results for binomial experience
chi_square_results_binomial


#### end ####


#### Location w/ Yard Activities - Q9 ####

SurveyResults_AND_Demographics$Q9_1_CatOut[SurveyResults_AND_Demographics$Q9_1_CatOut == -99] <- NA

# Example for Q9_1_CatOut (binomial responses) vs Place
table_Q9_1 <- table(SurveyResults_AND_Demographics$Q15_Place, SurveyResults_AND_Demographics$Q9_1_CatOut)
chi_square_Q9_1 <- chisq.test(table_Q9_1)
print(chi_square_Q9_1)

# Logistic regression for Q9_1_CatOut and Place
model_Q9_1 <- glm(Q9_1_CatOut ~ Q15_Place, data = SurveyResults_AND_Demographics, family = binomial)
summary(model_Q9_1)



# List of Q9 variables
Q9_vars <- c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", 
             "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash",
             "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other")

# Perform Chi-square tests for each Q9 variable
chi_square_results <- lapply(Q9_vars, function(var) {
  
  # Remove rows where either Place or the current Q9 variable is NA
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics$Q15_Place) & !is.na(SurveyResults_AND_Demographics[[var]]), ]
  
  # Create contingency table using the column name correctly
  table_data <- table(cleaned_data$Q15_Place, cleaned_data[[var]])
  
  # Perform Chi-square test if the table is not empty
  if (nrow(table_data) > 1 && ncol(table_data) > 1) {
    chisq_test <- chisq.test(table_data)
    p_value <- chisq_test$p.value
  } else {
    # Handle cases where the table has too few rows or columns to run Chi-square
    p_value <- NA
  }
  
  # Return variable name and p-value
  return(c(var = var, p_value = p_value))
})

# Convert the list of results to a data frame for easier viewing
chi_square_results_df <- do.call(rbind, chi_square_results)
colnames(chi_square_results_df) <- c("Variable", "p_value")

# View the results
print(chi_square_results_df)


# List of Q9 variables
Q9_vars <- c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", 
             "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash",
             "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other")

# Perform logistic regression for each Q9 variable
logistic_regression_results_Q9 <- lapply(Q9_vars, function(var) {
  
  # Remove rows where either Q15_Place or the current Q9 variable is NA
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics$Q15_Place) & 
                                                   !is.na(SurveyResults_AND_Demographics[[var]]), ]
  
  # Ensure the Q9 variable only contains 0 and 1 (replace other values with NA)
  cleaned_data[[var]] <- ifelse(cleaned_data[[var]] != 0 & cleaned_data[[var]] != 1, NA, cleaned_data[[var]])
  
  # Remove rows with NA values in the Q9 variable after the cleaning step
  cleaned_data <- cleaned_data[!is.na(cleaned_data[[var]]), ]
  
  # Fit logistic regression model (Q15_Place as predictor, Q9 variable as outcome)
  logistic_model <- glm(cleaned_data[[var]] ~ cleaned_data$Q15_Place, 
                        family = binomial, data = cleaned_data)
  
  # Extract p-value for the Place predictor
  p_value <- summary(logistic_model)$coefficients[2, 4]  # Extract the p-value for Q15_Place
  
  # Return variable name and p-value
  return(c(var = var, p_value = p_value))
})

# Convert the list of results to a data frame for easier viewing
logistic_regression_results_Q9_df <- do.call(rbind, logistic_regression_results_Q9)
colnames(logistic_regression_results_Q9_df) <- c("Variable", "p_value")

# View the results
print(logistic_regression_results_Q9_df)

#### end ####


#### Location w/ Yard Items - Q10 ####

Q10_vars <- c("Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", 
              "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", 
              "Q10_9_Woodpile", "Q10_10_Other")

# Perform Chi-square tests for each Q10 variable
chi_square_results_Q10 <- lapply(Q10_vars, function(var) {
  
  # Remove rows where either Q15_Place or the current Q10 variable is NA
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics$Q15_Place) & 
                                                   !is.na(SurveyResults_AND_Demographics[[var]]), ]
  
  # Create contingency table using the column name correctly
  table_data <- table(cleaned_data$Q15_Place, cleaned_data[[var]])
  
  # Perform Chi-square test if the table is not empty
  if (nrow(table_data) > 1 && ncol(table_data) > 1) {
    chisq_test <- chisq.test(table_data)
    p_value <- chisq_test$p.value
  } else {
    # Handle cases where the table has too few rows or columns to run Chi-square
    p_value <- NA
  }
  
  # Return variable name and p-value
  return(c(var = var, p_value = p_value))
})

# Convert the list of results to a data frame for easier viewing
chi_square_results_Q10_df <- do.call(rbind, chi_square_results_Q10)
colnames(chi_square_results_Q10_df) <- c("Variable", "p_value")

# View the results
print(chi_square_results_Q10_df)



# Logistic Regression
Q10_vars <- c("Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", 
              "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", 
              "Q10_9_Woodpile", "Q10_10_Other")
logistic_regression_results_Q10 <- lapply(Q10_vars, function(var) {
  cleaned_data <- SurveyResults_AND_Demographics[!is.na(SurveyResults_AND_Demographics$Q15_Place) & 
                                                   !is.na(SurveyResults_AND_Demographics[[var]]), ]
  cleaned_data[[var]] <- ifelse(cleaned_data[[var]] != 0 & cleaned_data[[var]] != 1, NA, cleaned_data[[var]])
  cleaned_data <- cleaned_data[!is.na(cleaned_data[[var]]), ]
  logistic_model <- glm(cleaned_data[[var]] ~ cleaned_data$Q15_Place, 
                        family = binomial, data = cleaned_data)
  p_value <- summary(logistic_model)$coefficients[2, 4]  # Extract the p-value for Q15_Place
  return(c(var = var, p_value = p_value))
})
logistic_regression_results_Q10_df <- do.call(rbind, logistic_regression_results_Q10)
colnames(logistic_regression_results_Q10_df) <- c("Variable", "p_value")
print(logistic_regression_results_Q10_df)



#DO more to see which place each behavior is more related to?????

#### end ####



#### Gender Data - Coy ####

Linear_Coy_Gender <- lm(Q1_CoySH ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                        data = SurveyResults_AND_Demographics)
summary(Linear_Coy_Gender)

SurveyResults_AND_Demographics_Clean$Experience_with_Coy_Ordered <- factor(SurveyResults_AND_Demographics_Clean$Q2_CatSH, 
                                                                           ordered = TRUE,
                                                                           levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104))
Ordinal_Coy_Gender <- polr(Experience_with_Coy_Ordered ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA,
                           data = SurveyResults_AND_Demographics_Clean, 
                           method = "logistic")
summary(Ordinal_Coy_Gender)
brant(Ordinal_Coy_Gender)


model_CoyType_gender <- glm(Q3_1_CoyNE ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_2_CoyBE ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_3_CoyWat ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_4_CoyFS ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_5_CoyFed ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_6_CoyPA ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)
model_CoyType_gender <- glm(Q3_7_CoyOther ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_gender)

#### end ####


#### Gender Data - Cat ####
Linear_Cat_Gender <- lm(Q2_CatSH ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                     data = SurveyResults_AND_Demographics)
summary(Linear_Cat_Gender)

SurveyResults_AND_Demographics_Clean$Experience_with_Cat_Ordered <- factor(SurveyResults_AND_Demographics_Clean$Q2_CatSH, 
                                                                               ordered = TRUE,
                                                                               levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104))
Ordinal_Cat_Gender <- polr(Experience_with_Cat_Ordered ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA,
                           data = SurveyResults_AND_Demographics_Clean, 
                           method = "logistic")
summary(Ordinal_Cat_Gender)
brant(Ordinal_Cat_Gender)


model_CatType_gender <- glm(Q4_1_CatNE ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_2_CatBE ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_3_CatWat ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_4_CatFS ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_5_CatFed ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_6_CatTouch ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q4_7_CatPA ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CoyType_gender_other <- glm(Q4_8_CatOther ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_gender_other)


model_CatType_gender <- glm(Q5_1_MyCat ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q5_2_NeighCat ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q5_3_Stray ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)
model_CatType_gender <- glm(Q5_4_Unsure ~ Q14_1_Female + Q14_2_Male + Q14_3_Non + Q14_4_NA, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_gender)

#### end ####



#### Age - Coy Freq ####

install.packages("MASS")
library(MASS)
# Ensure that Age_Group is a factor (since it's categorical)
SurveyResults_AND_Demographics$Age_Group <- factor(SurveyResults_AND_Demographics$Age_Group,
                                                   levels = c(1, 2, 3, 4, 5),
                                                   labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
# Make sure Q1_CoySH (Coyote Experience) is treated as an ordered factor
SurveyResults_AND_Demographics$Q1_CoySH <- factor(SurveyResults_AND_Demographics$Q1_CoySH,
                                                  levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104),
                                                  ordered = TRUE)
#ordinal regression model
ord_model <- polr(Q1_CoySH ~ Q13_Age, data = SurveyResults_AND_Demographics, Hess = TRUE)
summary(ord_model)
confint(ord_model)



#IF WANT NON-PARAMETRIC - USE??
# Ensure that Age_Group is a factor
SurveyResults_AND_Demographics$Q13_Age <- factor(SurveyResults_AND_Demographics$Q13_Age,
                                                   levels = c(1, 2, 3, 4, 5),
                                                   labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
# Perform Kruskal-Wallis Test
kruskal_result <- kruskal.test(Q1_CoySH ~ Q13_Age, data = SurveyResults_AND_Demographics)
# Output the result
kruskal_result

#### end ####

#### Age - Cat Freq####

install.packages("MASS")
library(MASS)
# Ensure that Age_Group is a factor (since it's categorical)
SurveyResults_AND_Demographics$Q13_Age <- factor(SurveyResults_AND_Demographics$Q13_Age,
                                                   levels = c(1, 2, 3, 4, 5),
                                                   labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
# Make sure Q2_CatSH (Coyote Experience) is treated as an ordered factor
SurveyResults_AND_Demographics$Q2_CatSH <- factor(SurveyResults_AND_Demographics$Q2_CatSH,
                                                  levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104),
                                                  ordered = TRUE)
#ordinal regression model
ord_model <- polr(Q2_CatSH ~ Q13_Age, data = SurveyResults_AND_Demographics, Hess = TRUE)
summary(ord_model)
confint(ord_model)


#IF WANT NON-PARAMETRIC - USE??
# Ensure that Age_Group is a factor
SurveyResults_AND_Demographics$Q13_Age <- factor(SurveyResults_AND_Demographics$Q13_Age,
                                                 levels = c(1, 2, 3, 4, 5),
                                                 labels = c("18-29", "30-39", "40-49", "50-59", "60+"))
# Perform Kruskal-Wallis Test
kruskal_result <- kruskal.test(Q2_CatSH ~ Q13_Age, data = SurveyResults_AND_Demographics)
# Output the result
kruskal_result

#### end ####


#### Age - Coy Typ ####
str(SurveyResults_AND_Demographics$Q13_Age)

# Convert Q13_Age to a factor if it is not already
SurveyResults_AND_Demographics$Q13_Age <- factor(SurveyResults_AND_Demographics$Q13_Age,
                                                 levels = c(1, 2, 3, 4, 5),
                                                 labels = c("18-29", "30-39", "40-49", "50-59", "60+"))

# Create a vector of the experience types
experience_types <- c("Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q3_4_CoyFS", 
                      "Q3_5_CoyFed", "Q3_6_CoyPA", "Q3_7_CoyOther")

# Create an empty list to store results
chi_square_results <- list()

# Loop through each experience type and run a Chi-square test
for (exp in experience_types) {
  # Perform the Chi-square test
  test_result <- chisq.test(table(SurveyResults_AND_Demographics[[exp]], SurveyResults_AND_Demographics$Q13_Age))
  
  # Store the results in the list
  chi_square_results[[exp]] <- test_result
}

# Print the Chi-square results
chi_square_results

#### end ####


#### Age - Cat Type ####

str(SurveyResults_AND_Demographics$Q13_Age)

# Convert Q13_Age to a factor if it is not already
SurveyResults_AND_Demographics$Q13_Age <- factor(SurveyResults_AND_Demographics$Q13_Age,
                                                 levels = c(1, 2, 3, 4, 5),
                                                 labels = c("18-29", "30-39", "40-49", "50-59", "60+"))

# Create a vector of the experience types
experience_types_cat <- c("Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q4_4_CatFS", 
                      "Q4_5_CatFed", "Q4_6_CatPA", "Q4_7_CatTouch", "Q4_8_CatOther")

# Create an empty list to store results
chi_square_results <- list()

# Loop through each experience type and run a Chi-square test
for (exp in experience_types_cat) {
  # Perform the Chi-square test
  test_result <- chisq.test(table(SurveyResults_AND_Demographics[[exp]], SurveyResults_AND_Demographics$Q13_Age))
  
  # Store the results in the list
  chi_square_results[[exp]] <- test_result
}

# Print the Chi-square results
chi_square_results

#### end ####



#### US Census Bureau Demographics - Coy Exp ####
  # Low Income, People of Color, Ages 65-75, Bachelors Degree
library(dplyr)
summary(SurveyResults_AND_Demographics$Q1_CoySH)

model_coyotes_linear <- lm(Q1_CoySH ~ AGE_65_P + LOW_INC_P + POC_P + ED_BACH_P, 
                           data = SurveyResults_AND_Demographics) # Linear regression model 
summary(model_coyotes_linear) # Summary of the model

library(MASS) 

#Updated Approach: Ordinal Logistic Regression
# Convert the Experience variable into an ordered factor
SurveyResults_AND_Demographics_Clean$Experience_with_Coyotes_Ordered <- factor(SurveyResults_AND_Demographics_Clean$Q1_CoySH, 
                                                    ordered = TRUE,
                                                    levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104))

summary(SurveyResults_AND_Demographics_Clean$Experience_with_Coyotes_Ordered)

# Ordinal logistic regression using polr()
model_coyotes_ordinal <- polr(Experience_with_Coyotes_Ordered ~ AGE_65_P + LOW_INC_P + POC_P + ED_BACH_P, 
                              data = SurveyResults_AND_Demographics_Clean, 
                              method = "logistic")

summary(model_coyotes_ordinal)

library(brant)
brant(model_coyotes_ordinal)


#AGES - Coy

Linear_Coy_Age <- lm(Q1_CoySH ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                           data = SurveyResults_AND_Demographics)
summary(Linear_Coy_Age)


Ordinal_Coy_Age <- polr(Experience_with_Coyotes_Ordered ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                              data = SurveyResults_AND_Demographics_Clean, 
                              method = "logistic")
summary(Ordinal_Coy_Age)
brant(Ordinal_Coy_Age)


#INCOME - Coy

Linear_Coy_Income <- lm(Q1_CoySH ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                     data = SurveyResults_AND_Demographics)
summary(Linear_Coy_Income)


Ordinal_Coy_Income <- polr(Experience_with_Coyotes_Ordered ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                        data = SurveyResults_AND_Demographics_Clean, 
                        method = "logistic")
summary(Ordinal_Coy_Income)
brant(Ordinal_Coy_Income)

#RACE - Coy

Linear_Coy_Race <- lm(Q1_CoySH ~ WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                     data = SurveyResults_AND_Demographics)
summary(Linear_Coy_Race)


Ordinal_Coy_Race <- polr(Experience_with_Coyotes_Ordered ~ WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                        data = SurveyResults_AND_Demographics_Clean, 
                        method = "logistic")
summary(Ordinal_Coy_Race)
brant(Ordinal_Coy_Race)

#EDUCATION - Coy

Linear_Coy_Educ <- lm(Q1_CoySH ~ ED_LT_HS_P + ED_HS_P + ED_ASSOC_P + ED_BACH_P + ED_GRAD_P, 
                     data = SurveyResults_AND_Demographics)
summary(Linear_Coy_Educ)


Ordinal_Coy_Educ <- polr(Experience_with_Coyotes_Ordered ~ ED_LT_HS_P + ED_HS_P + ED_ASSOC_P + ED_BACH_P + ED_GRAD_P, 
                        data = SurveyResults_AND_Demographics, 
                        method = "logistic")
summary(Ordinal_Coy_Educ)
brant(Ordinal_Coy_Educ)

#### end ####


#### US Census Bureau Demographics - Cat Exp ####
  # Low Income, People of Color, Ages 65-75, Bachelors Degree
library(dplyr)
summary(SurveyResults_AND_Demographics_Clean$Q2_CatSH)

model_cats_linear <- lm(Q2_CatSH ~ AGE_65_P + LOW_INC_P + POC_P + ED_HS_P, 
                           data = SurveyResults_AND_Demographics_Clean) # Linear regression model 
summary(model_cats_linear) # Summary of the model

library(MASS) 

#Updated Approach: Ordinal Logistic Regression
# Convert the Experience variable into an ordered factor
SurveyResults_AND_Demographics_Clean$Experience_with_Coyotes_Ordered <- factor(SurveyResults_AND_Demographics_Clean$Q2_CatSH, 
                                                                               ordered = TRUE,
                                                                               levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104))

summary(SurveyResults_AND_Demographics_Clean$Experience_with_Coyotes_Ordered)

# Ordinal logistic regression using polr()
model_cats_ordinal <- polr(Experience_with_Coyotes_Ordered ~ AGE_65_P + LOW_INC_P + POC_P + ED_BACH_P, 
                              data = SurveyResults_AND_Demographics_Clean, 
                              method = "logistic")

summary(model_cats_ordinal)

library(brant)
brant(model_cats_ordinal)






SurveyResults_AND_Demographics_Clean$Experience_with_Cats_Ordered <- factor(SurveyResults_AND_Demographics_Clean$Q2_CatSH, 
                                                                               ordered = TRUE,
                                                                               levels = c(0, 0.5, 1, 2, 6, 12, 26, 52, 104))

summary(SurveyResults_AND_Demographics_Clean$Experience_with_Cats_Ordered)

# Ordinal logistic regression using polr()
model_Cat_ordinal <- polr(Experience_with_Cats_Ordered ~ AGE_65_P + LOW_INC_P + POC_P + ED_BACH_P, 
                              data = SurveyResults_AND_Demographics_Clean, 
                              method = "logistic")

summary(model_Cat_ordinal)

library(brant)
brant(model_Cat_ordinal)



#AGES - Cat

Linear_Cat_Age <- lm(Q2_CatSH ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                     data = SurveyResults_AND_Demographics)
summary(Linear_Cat_Age)


Ordinal_Cat_Age <- polr(Experience_with_Cats_Ordered ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                        data = SurveyResults_AND_Demographics_Clean, 
                        method = "logistic")
summary(Ordinal_Cat_Age)
brant(Ordinal_Cat_Age)


#INCOME - Cat

Linear_Cat_Income <- lm(Q2_CatSH ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                        data = SurveyResults_AND_Demographics)
summary(Linear_Cat_Income)


Ordinal_Cat_Income <- polr(Experience_with_Cats_Ordered ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                           data = SurveyResults_AND_Demographics_Clean, 
                           method = "logistic")
summary(Ordinal_Cat_Income)
brant(Ordinal_Cat_Income)

#RACE - Cat

Linear_Cat_Race <- lm(Q2_CatSH ~ WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                      data = SurveyResults_AND_Demographics)
summary(Linear_Cat_Race)


Ordinal_Cat_Race <- polr(Experience_with_Cats_Ordered ~ WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                         data = SurveyResults_AND_Demographics_Clean, 
                         method = "logistic")
summary(Ordinal_Cat_Race)
brant(Ordinal_Cat_Race)

#EDUCATION - Cat

Linear_Cat_Educ <- lm(Q2_CatSH ~ ED_LT_HS_P + ED_HS_P + ED_ASSOC_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics)
summary(Linear_Cat_Educ)


Ordinal_Cat_Educ <- polr(Experience_with_Cats_Ordered ~ ED_LT_HS_P + ED_HS_P + ED_ASSOC_P + ED_BACH_P + ED_GRAD_P, 
                         data = SurveyResults_AND_Demographics_Clean, 
                         method = "logistic")
summary(Ordinal_Cat_Educ)
brant(Ordinal_Cat_Educ)

#### end ####


#### US Census Bureau Demographics - Coy Exp Type ####

# Logistic regression for coyote experience (e.g., "Never encountered" - Q3_1_CoyNE)

# Check for missing values in the specific response variable
table(SurveyResults_AND_Demographics$Q3_1_CoyNE, useNA = "ifany")
# Replace -99 with NA for the specific response variable
SurveyResults_AND_Demographics$Q3_1_CoyNE[SurveyResults_AND_Demographics$Q3_1_CoyNE == -99] <- NA
SurveyResults_AND_Demographics$Q3_2_CoyBE[SurveyResults_AND_Demographics$Q3_2_CoyBE == -99] <- NA
SurveyResults_AND_Demographics$Q3_3_CoyWat[SurveyResults_AND_Demographics$Q3_3_CoyWat == -99] <- NA
SurveyResults_AND_Demographics$Q3_4_CoyFS[SurveyResults_AND_Demographics$Q3_4_CoyFS == -99] <- NA
SurveyResults_AND_Demographics$Q3_5_CoyFed[SurveyResults_AND_Demographics$Q3_5_CoyFed == -99] <- NA
SurveyResults_AND_Demographics$Q3_6_CoyPA[SurveyResults_AND_Demographics$Q3_6_CoyPA == -99] <- NA
SurveyResults_AND_Demographics$Q3_7_CoyOther[SurveyResults_AND_Demographics$Q3_7_CoyOther == -99] <- NA
SurveyResults_AND_Demographics$Q4_1_CatNE[SurveyResults_AND_Demographics$Q4_1_CatNE == -99] <- NA
SurveyResults_AND_Demographics$Q4_2_CatBE[SurveyResults_AND_Demographics$Q4_2_CatBE == -99] <- NA
SurveyResults_AND_Demographics$Q4_3_CatWat[SurveyResults_AND_Demographics$Q4_3_CatWat == -99] <- NA
SurveyResults_AND_Demographics$Q4_4_CatFS[SurveyResults_AND_Demographics$Q4_4_CatFS == -99] <- NA
SurveyResults_AND_Demographics$Q4_5_CatFed[SurveyResults_AND_Demographics$Q4_5_CatFed == -99] <- NA
SurveyResults_AND_Demographics$Q4_6_CatTouch[SurveyResults_AND_Demographics$Q4_6_CatTouch == -99] <- NA
SurveyResults_AND_Demographics$Q4_7_CatPA[SurveyResults_AND_Demographics$Q4_7_CatPA == -99] <- NA
SurveyResults_AND_Demographics$Q4_8_CatOther[SurveyResults_AND_Demographics$Q4_8_CatOther == -99] <- NA



# Age (as categorical variables) - NE
model_CoyType_age <- glm(Q3_1_CoyNE ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                        data = SurveyResults_AND_Demographics, 
                        family = binomial)
summary(model_CoyType_age)

# Income (as categorical variables)
model_CoyType_income <- glm(Q3_1_CoyNE ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                           data = SurveyResults_AND_Demographics, 
                           family = binomial)
summary(model_CoyType_income)

# Race (as categorical variables)
model_CoyType_race <- glm(Q3_1_CoyNE ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                           HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_race)

# Education (as categorical variables)
model_CoyType_education <- glm(Q3_1_CoyNE ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                              data = SurveyResults_AND_Demographics, 
                              family = binomial)
summary(model_CoyType_education)

# Combine Age, Income, Race, and Education in one model (for Q3_1_CoyNE - Never encountered)
model_combined <- glm(Q3_1_CoyNE ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#BE
model_CoyType_age <- glm(Q3_2_CoyBE ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_2_CoyBE ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_2_CoyBE ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_2_CoyBE ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_2_CoyBE ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#Wat
model_CoyType_age <- glm(Q3_3_CoyWat ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_3_CoyWat ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_3_CoyWat ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_3_CoyWat ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_3_CoyWat ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#FS
model_CoyType_age <- glm(Q3_4_CoyFS ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_4_CoyFS ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_4_CoyFS ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_4_CoyFS ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_4_CoyFS ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#Fed
model_CoyType_age <- glm(Q3_5_CoyFed ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_5_CoyFed ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_5_CoyFed ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_5_CoyFed ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_5_CoyFed ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#PA
model_CoyType_age <- glm(Q3_6_CoyPA ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_6_CoyPA ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_6_CoyPA ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_6_CoyPA ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_6_CoyPA ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#Other
model_CoyType_age <- glm(Q3_7_CoyOther ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CoyType_age)

model_CoyType_income <- glm(Q3_7_CoyOther ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CoyType_income)

model_CoyType_race <- glm(Q3_7_CoyOther ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CoyType_race)

model_CoyType_education <- glm(Q3_7_CoyOther ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CoyType_education)
# Combine
model_combined <- glm(Q3_7_CoyOther ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_combined)

#### end ####


#### US Census Bureau Demographics - Cat Exp Type ####

model_CatType_age <- glm(Q4_1_CatNE ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_1_CatNE ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_1_CatNE ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_1_CatNE ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)

model_cat_combined <- glm(Q4_1_CatNE ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#BE
model_CatType_age <- glm(Q4_2_CatBE ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_2_CatBE ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_2_CatBE ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_2_CatBE ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_2_CatBE ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#Wat
model_CatType_age <- glm(Q4_3_CatWat ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_3_CatWat ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_3_CatWat ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_3_CatWat ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_3_CatWat ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#FS
model_CatType_age <- glm(Q4_4_CatFS ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_4_CatFS ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_4_CatFS ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_4_CatFS ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_4_CatFS ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#Fed
model_CatType_age <- glm(Q4_5_CatFed ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_5_CatFed ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_5_CatFed ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_5_CatFed ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_5_CatFed ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#Touch
model_CatType_age <- glm(Q4_6_CatTouch ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_6_CatTouch ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_6_CatTouch ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_6_CatTouch ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_6_CatTouch ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#PA
model_CatType_age <- glm(Q4_7_CatPA ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_7_CatPA ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_7_CatPA ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_7_CatPA ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_7_CatPA ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)

#Other
model_CatType_age <- glm(Q4_8_CatOther ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q4_8_CatOther ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q4_8_CatOther ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q4_8_CatOther ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q4_8_CatOther ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                        INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                        HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                        ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                      data = SurveyResults_AND_Demographics, 
                      family = binomial)
summary(model_cat_combined)




SurveyResults_AND_Demographics$Q5_1_MyCat[SurveyResults_AND_Demographics$Q5_1_MyCat == -99] <- NA
SurveyResults_AND_Demographics$Q5_2_NeighCat[SurveyResults_AND_Demographics$Q5_2_NeighCat == -99] <- NA
SurveyResults_AND_Demographics$Q5_3_Stray[SurveyResults_AND_Demographics$Q5_3_Stray == -99] <- NA
SurveyResults_AND_Demographics$Q5_4_Unsure[SurveyResults_AND_Demographics$Q5_4_Unsure == -99] <- NA


#Cat type
#Mine
model_CatType_age <- glm(Q5_1_MyCat ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q5_1_MyCat ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q5_1_MyCat ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q5_1_MyCat ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q5_1_MyCat ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                            INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                            ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_cat_combined)

#Neighbors
model_CatType_age <- glm(Q5_2_NeighCat ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q5_2_NeighCat ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q5_2_NeighCat ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q5_2_NeighCat ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q5_2_NeighCat ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                            INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                            ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_cat_combined)

#Stray/Feral
model_CatType_age <- glm(Q5_3_Stray ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q5_3_Stray ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q5_3_Stray ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q5_3_Stray ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q5_3_Stray ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                            INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                            ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_cat_combined)

#Unsure
model_CatType_age <- glm(Q5_4_Unsure ~ AGE_18_P + AGE_35_P + AGE_65_P, 
                         data = SurveyResults_AND_Demographics, 
                         family = binomial)
summary(model_CatType_age)

model_CatType_income <- glm(Q5_4_Unsure ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, 
                            data = SurveyResults_AND_Demographics, 
                            family = binomial)
summary(model_CatType_income)

model_CatType_race <- glm(Q5_4_Unsure ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_CatType_race)

model_CatType_education <- glm(Q5_4_Unsure ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                               data = SurveyResults_AND_Demographics, 
                               family = binomial)
summary(model_CatType_education)
# Combine
model_cat_combined <- glm(Q5_4_Unsure ~ AGE_18_P + AGE_35_P + AGE_65_P + LOW_INC_P + INC_50_P + 
                            INC_125_P + INC_200_P + POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + 
                            HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P + 
                            ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, 
                          data = SurveyResults_AND_Demographics, 
                          family = binomial)
summary(model_cat_combined)

#### end ####






# RUN ALL BLOW BEFORE CHAT CODE

#Coy attitudes

#Cat attitudes

#General attitudes

#Overall attitudes 

#### USCB Demograpgics - Yard Attributes ####
#### end ####


#### USCB Demograpgics - Yard Types ####

# ANOVA to test if yard type percentage differs by age group
model_grass_age <- aov(Q11_1_Grass ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_grass_age)
model_hardscape_age <- aov(Q11_2_Hardscape ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_hardscape_age)
model_veg_income <- aov(Q11_3_Veg ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_veg_income)
model_mulch_income <- aov(Q11_4_Mulch ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_mulch_income)
model_shrub_income <- aov(Q11_5_Shrub ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_shrub_income)
model_tree_income <- aov(Q11_6_Tree ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_tree_income)
model_other_income <- aov(Q11_7_Other ~ AGE_18_P + AGE_35_P + AGE_65_P, data = SurveyResults_AND_Demographics)
summary(model_other_income)

# ANOVA to test if yard type percentage differs by income level
model_grass_income <- aov(Q11_1_Grass ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_grass_income)
model_hardscape_income <- aov(Q11_2_Hardscape ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_hardscape_income)
model_veg_income <- aov(Q11_3_Veg ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_veg_income)
model_mulch_income <- aov(Q11_4_Mulch ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_mulch_income)
model_shrub_income <- aov(Q11_5_Shrub ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_shrub_income)
model_tree_income <- aov(Q11_6_Tree ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_tree_income)
model_other_income <- aov(Q11_7_Other ~ LOW_INC_P + INC_50_P + INC_125_P + INC_200_P, data = SurveyResults_AND_Demographics)
summary(model_other_income)


model_grass_race <- aov(Q11_1_Grass ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_grass_race)
model_hardscape_income <- aov(Q11_2_Hardscape ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_hardscape_income)
model_veg_income <- aov(Q11_3_Veg ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_veg_income)
model_mulch_income <- aov(Q11_4_Mulch ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_mulch_income)
model_shrub_income <- aov(Q11_5_Shrub ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_shrub_income)
model_tree_race <- aov(Q11_6_Tree ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_tree_race)
model_other_income <- aov(Q11_7_Other ~ POC_P + WHITE_A_P + MULTI_NH_P + BLACK_NH_P + HISPANIC_P + ASIAN_NH_P + AIAN_NH_P + NHPI_NH_P + OTHER_NH_P, data = SurveyResults_AND_Demographics)
summary(model_other_income)


model_grass_educ <- aov(Q11_1_Grass ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_grass_educ)
model_hardscape_income <- aov(Q11_2_Hardscape ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_hardscape_income)
model_veg_educ <- aov(Q11_3_Veg ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_veg_educ)
model_mulch_educ <- aov(Q11_4_Mulch ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_mulch_educ)
model_shrub_educ <- aov(Q11_5_Shrub ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_shrub_educ)
model_tree_educ <- aov(Q11_6_Tree ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_tree_educ)
model_other_educ <- aov(Q11_7_Other ~ ED_LT_HS_P + ED_HS_P + ED_BACH_P + ED_GRAD_P, data = SurveyResults_AND_Demographics)
summary(model_other_educ)


# Linear regression to test if continuous age affects yard type percentage
model_grass_age_continuous <- lm(Q11_1_Grass ~ Age, data = SurveyResults_AND_Demographics)
summary(model_grass_age_continuous)

# Similarly for other yard types
model_hardscape_age_continuous <- lm(Q11_2_Hardscape ~ Age, data = SurveyResults_AND_Demographics)
summary(model_hardscape_age_continuous)

# Post-hoc test (Tukey HSD) if the ANOVA result is significant
tukey_test_grass <- TukeyHSD(model_grass_age)
summary(tukey_test_grass)

#### end ####


#### Dem w/ Exp - Chat Code ####

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)

SurveyResults_Cleaned<-SurveyResults_AND_Demographics
data<-SurveyResults_Cleaned
summary(data)

# Convert gender variables to a single categorical variable
data$Gender <- case_when(
  data$Q14_1_Female == 1 ~ "Female",
  data$Q14_2_Male == 1 ~ "Male",
  data$Q14_3_Non == 1 ~ "Non-binary",
  data$Q14_4_NA == 1 ~ "Prefer not to say",
  TRUE ~ NA_character_
)

data$Gender <- factor(data$Gender)

# Ensure Age is numeric
data$Q13_Age <- as.numeric(data$Q13_Age)

# Remove NA values from Age before plotting
data_age_filtered <- data %>% filter(!is.na(Q13_Age) & Q13_Age != -99)

# Age distribution
ggplot(data_age_filtered, aes(x = Q13_Age)) +
  geom_histogram(binwidth = 1, fill = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Map categorical values to numeric scale for Q1 and Q2
data$Q1_CoySH <- recode(data$Q1_CoySH, `104`=104, `52`=52, `26`=26, `12`=12, `6`=6, `2`=2, `1`=1, `0.5`=0.5, `0`=0)
data$Q2_CatSH <- recode(data$Q2_CatSH, `104`=104, `52`=52, `26`=26, `12`=12, `6`=6, `2`=2, `1`=1, `0.5`=0.5, `0`=0)

# Filtered dataset excluding -99 for analysis
data_filtered <- data %>% filter(!if_any(everything(), ~ . == -99))


# Relationships between demographics and experiences
anova_result <- aov(Q1_CoySH ~ Gender + Q13_Age + Q15_Place, data = data_filtered)
summary(anova_result)
anova_result <- aov(Q2_CatSH ~ Gender + Q13_Age + Q15_Place, data = data_filtered)
summary(anova_result)


# Chi-square test for categorical variables
table_gender_experience <- table(data_filtered$Gender, data_filtered$Q1_CoySH)
chisq.test(table_gender_experience)


#### end ####
