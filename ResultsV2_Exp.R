

# Do experiences influence behaviors in yards + yard types? 

colnames(SurveyResults_AND_Demographics)
str(SurveyResults_AND_Demographics)

full_data_EXP <- SurveyResults_AND_Demographics
full_data_EXP[full_data_EXP == -99] <- NA

#OPTIONAL to REMOVE na's
full_data_clean_EXP <- na.omit(full_data_EXP)
colnames(full_data_clean_EXP)
str(full_data_clean_EXP)


#### Experiences - Frequency THEN frequency IF negative encoutners (negative encounter = 1, not = 0) ####
      # then tie to attitudes? 
      # then to demo? 



# Convert encounter frequency variables to factors
full_data_EXP$Q1_CoySH <- as.numeric(full_data_EXP$Q1_CoySH)
full_data_EXP$Q2_CatSH <- as.numeric(full_data_EXP$Q2_CatSH)
str(full_data_EXP$Q1_CoySH)
str(full_data_EXP$Q2_CatSH)


full_data_EXP$Q1_CoySH <- as.factor(full_data_EXP$Q1_CoySH)
full_data_EXP$Q2_CatSH <- as.factor(full_data_EXP$Q2_CatSH)
str(full_data_EXP$Q1_CoySH)
str(full_data_EXP$Q2_CatSH)

model_bbath <- glm(Q10_1_Bbath ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_bbath)
model_bfeed <- glm(Q10_2_Bfeed ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_bfeed)
model_Fence <- glm(Q10_3_Fence ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Fence)
model_Flower <- glm(Q10_4_Flower ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Flower)
model_Fruit <- glm(Q10_5_Fruit ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Fruit)
model_Water <- glm(Q10_6_Water ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Water)
model_Veg <- glm(Q10_7_Veg ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Veg)
model_Wind <- glm(Q10_8_Wind ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Wind)
model_Wood <- glm(Q10_9_Woodpile ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Wood)


model_CatOut <- glm(Q9_1_CatOut ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_CatOut)
model_TreeVeg <- glm(Q9_2_TreeVeg ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_TreeVeg)
model_Exter <- glm(Q9_3_Exter ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Exter)
model_Trap <- glm(Q9_4_Trap ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Trap)
model_Lights <- glm(Q9_5_Lights ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Lights)
model_PetFood <- glm(Q9_6_PetFood ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_PetFood)
model_DogOff <- glm(Q9_7_DogOff ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_DogOff)
model_LockTrash <- glm(Q9_8_LockTrash ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_LockTrash)
model_Prune <- glm(Q9_9_Prune ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Prune)
model_PestHerb <- glm(Q9_10_PestHerb ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_PestHerb)
model_Hab <- glm(Q9_11_CreateHab ~ Q1_CoySH + Q2_CatSH, data = full_data_EXP, family = binomial)
summary(model_Hab)




# List of Q11_ yard type variables (percentages)
yard_types <- c("Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", 
                "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")


# Log-transform Q11_ variables (adding a small constant)
full_data_EXP$Q11_1_Grass_log <- log(full_data_EXP$Q11_1_Grass + 0.1)
full_data_EXP$Q11_2_Hardscape_log <- log(full_data_EXP$Q11_2_Hardscape + 0.1)
full_data_EXP$Q11_3_Veg_log <- log(full_data_EXP$Q11_3_Veg + 0.1)
full_data_EXP$Q11_4_Mulch_log <- log(full_data_EXP$Q11_4_Mulch + 0.1)
full_data_EXP$Q11_5_Shrub_log <- log(full_data_EXP$Q11_5_Shrub + 0.1)
full_data_EXP$Q11_6_Tree_log <- log(full_data_EXP$Q11_6_Tree + 0.1)
full_data_EXP$Q11_7_Other_log <- log(full_data_EXP$Q11_7_Other + 0.1)


yard_types_logged <- c("Q11_1_Grass_log", "Q11_2_Hardscape_log", "Q11_3_Veg_log", 
                       "Q11_4_Mulch_log", "Q11_5_Shrub_log", "Q11_6_Tree_log", "Q11_7_Other_log")
models <- list()
for (var in yard_types_logged) {
  model <- lm(as.formula(paste(var, "~ Q1_CoySH + Q2_CatSH")), data = full_data_EXP)
  models[[var]] <- summary(model)
} # Run linear regression for each log-transformed yard type
models # Print summaries



# Binning percentages to comapre to ordered frequencies
library(MASS)
yard_binned_vars <- c("Q11_1_Grass_binned", "Q11_2_Hardscape_binned", "Q11_3_Veg_binned", 
                      "Q11_4_Mulch_binned", "Q11_5_Shrub_binned", "Q11_6_Tree_binned", "Q11_7_Other_binned")


models_ordinal <- list()
for (var in yard_binned_vars) {
  # Ordinal logistic regression for coyote encounter frequency
  model_coy <- polr(as.formula(paste("Q1_CoySH ~", var)), data = full_data_EXP, Hess = TRUE)
  
  # Ordinal logistic regression for cat encounter frequency
  model_cat <- polr(as.formula(paste("Q2_CatSH ~", var)), data = full_data_EXP, Hess = TRUE)
  
  models_ordinal[[var]] <- list(
    CoyModel = summary(model_coy),
    CatModel = summary(model_cat)
  )
}

models_ordinal






# All items / actions / types to combined frequncy
model_encounters_yItems <- lm(cbind(Q10_1_Bbath, Q10_2_Bfeed, Q10_3_Fence, Q10_4_Flower,  
                                       Q10_5_Fruit, Q10_6_Water, Q10_7_Veg, Q10_8_Wind,  
                                       Q10_9_Woodpile, Q10_10_Other) ~  
                                  Q1_CoySH + Q2_CatSH, data = full_data)

summary(model_encounters_yItems)


model_encounters_yActions <- lm(cbind(Q9_1_CatOut, Q9_2_TreeVeg, Q9_3_Exter, Q9_4_Trap,  
                                         Q9_5_Lights, Q9_6_PetFood, Q9_7_DogOff, Q9_8_LockTrash,  
                                         Q9_9_Prune, Q9_10_PestHerb, Q9_11_CreateHab, Q9_12_Other) ~  
                                     Q1_CoySH + Q2_CatSH, data = full_data)

summary(model_encounters_yActions)



model_encounters_yTypes <- lm(cbind(Q11_1_Grass, Q11_2_Hardscape, Q11_3_Veg, Q11_4_Mulch,  
                                    Q11_5_Shrub, Q11_6_Tree, Q11_7_Other) ~  
                                Q1_CoySH + Q2_CatSH, data = full_data)
summary(model_encounters_yTypes)
#### end ####

#### Normality + Residuals Check ####

# Function to check residual normality for all models
check_normality <- function(model_list) {
  for (var in names(model_list)) {
    model <- model_list[[var]]
    cat("\nResidual Normality Check for:", var, "\n")
    
    # Shapiro-Wilk Test (p < 0.05 means residuals are NOT normal)
    shapiro_test <- shapiro.test(resid(model))
    print(shapiro_test)
    
    # QQ Plot for visual assessment
    qqnorm(resid(model), main = paste("QQ Plot:", var))
    qqline(resid(model), col = "red")
  }
}

# Run normality check on all logit-transformed yard type models
check_normality(logit_models)






# Q-Q plot to check normality
qqnorm(residuals(model_encounters_yTypes))
qqline(residuals(model_encounters_yTypes), col = "red")

qqnorm(residuals(model_encounters_yItems))
qqline(residuals(model_encounters_yItems), col = "red")

qqnorm(residuals(model_encounters_yActions))
qqline(residuals(model_encounters_yActions), col = "red")

# Shapiro-Wilk test for normality
shapiro.test(residuals(model_encounters_yTypes))
shapiro.test(residuals(model_encounters_yTypes_log))
shapiro.test(residuals(model_encounters_yItems))
shapiro.test(residuals(model_encounters_yActions))

#### end ####

#### Grouping Yard Types / Actions / Characteristics as Per Meeting Notes ####

# Normalize percentage variables for the neutral group (example)
full_data$Q11_1_Grass_norm <- full_data$Q11_1_Grass / 100  # Normalize to 0-1 range
full_data$Q11_2_Hardscape_norm <- full_data$Q11_2_Hardscape / 100
full_data$Q11_3_Veg_norm <- full_data$Q11_3_Veg / 100
full_data$Q11_4_Mulch_norm <- full_data$Q11_4_Mulch / 100
full_data$Q11_5_Shrub_norm <- full_data$Q11_5_Shrub / 100
full_data$Q11_6_Tree_norm <- full_data$Q11_6_Tree / 100
full_data$Q11_7_Other_norm <- full_data$Q11_7_Other / 100

# Define the resistance group (binary variables, and percentages normalized)
resistance_vars <- c("Q3_4_CoyFS", "Q3_6_CoyPA", "Q4_4_CatFS", "Q4_7_CatPA", "Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", "Q9_7_DogOff", "Q9_10_PestHerb", "Q10_3_Fence", "Q10_8_Wind", "Q11_2_Hardscape_norm", "Q11_4_Mulch_norm") # Example binary resistance variables
encouragement_vars <- c("Q3_5_CoyFed", "Q4_5_CatFed", "Q4_6_CatTouch", "Q5_1_MyCat", "Q9_6_PetFood", "Q9_11_CreateHab", "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_9_Woodpile", "Q11_3_Veg_norm", "Q11_5_Shrub_norm", "Q11_6_Tree_norm") # Example binary encouragement variables
neutral_vars <- c("Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q5_2_NeighCat", "Q5_3_Stray", "Q5_4_Unsure", "Q9_8_LockTrash", "Q9_9_Prune", "Q11_1_Grass_norm")

# Sum binary variables for each group (if applicable)
full_data$resistance_score <- rowSums(full_data[resistance_vars], na.rm = TRUE)
full_data$encouragement_score <- rowSums(full_data[encouragement_vars], na.rm = TRUE)

# Now you have scores for each group (resistance, encouragement, and neutral)
# You can use these scores for analysis with attitude indexes


colnames(full_data)

install.packages("corrplot")  # Install if needed
library(corrplot)
source("~/Desktop/PSU 2024-2025/R_Tools_Examples_Script/Scripts/cor.matrix.r")
cor_matrix <- cor(full_data[, c("Utilitarian_Index", "Biocentric_Index", "Stewardship_Index", 
                                "resistance_score", "encouragement_score")], 
                  use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "square")
corrplot(cor_matrix, method = "number")

corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))


model_resistance <- lm(resistance_score ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, 
                       data = full_data)
summary(model_resistance)
model_encouragement <- lm(encouragement_score ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, 
                          data = full_data)
summary(model_encouragement)
model_neutral <- lm(neutral_score ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, 
                    data = full_data)
summary(model_neutral)


library(ggplot2)
ggplot(full_data, aes(x = Utilitarian_Index, y = resistance_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Utilitarian Index vs Resistance Score")
ggplot(full_data, aes(x = Utilitarian_Index, y = encouragement_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Utilitarian Index vs Encouragement Score")


ggplot(full_data, aes(x = Biocentric_Index, y = encouragement_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Biocentric Index vs Encouragement Score")
ggplot(full_data, aes(x = Biocentric_Index, y = resistance_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Biocentric Index vs Resistance Score")

ggplot(full_data, aes(x = Stewardship_Index, y = encouragement_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Stewardship Index vs Encouragement Score")
ggplot(full_data, aes(x = Stewardship_Index, y = resistance_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Stewardship Index vs Resistance Score")


par(mfrow = c(2, 2))
plot(model_resistance)
plot(model_encouragement)
plot(model_neutral)
par(mfrow = c(1, 1))

#No correlation <> them BUT some w/ bio + stew

#### end ####

#### PCA + Visualization ####

install.packages("factoextra")
library(dplyr)
library(factoextra)
library(ggplot2)

# ALL Yard Variables PCA
yard_data_full <- full_data[, c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", 
                                "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash", 
                                "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", 
                                "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", 
                                "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", 
                                "Q10_9_Woodpile", "Q11_1_Grass", "Q11_2_Hardscape", 
                                "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")]

colnames(yard_data_full)

yard_data_full <- yard_data_full %>%
  mutate(across(starts_with("Q11_"), ~ log(. + 0.1)))  # Log-transform Q11_ variables if needed

yard_data_scaled <- scale(yard_data_full) # Standardize data (mean=0, sd=1)

sum(is.na(yard_data_scaled))   # Count missing values
sum(is.infinite(yard_data_scaled))  # Count infinite values

yard_data_scaled <- na.omit(yard_data_scaled)

pca_result <- prcomp(yard_data_scaled, center = TRUE, scale. = TRUE) # Run PCA
summary(pca_result)

screeplot(pca_result, type = "lines", main = "Scree Plot") # Scree plot to check number of principal components
fviz_eig(pca_result)
fviz_pca_biplot(pca_result, label = "var")
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("blue", "red")) # Contributions of variables

pca_scores <- as.data.frame(pca_result$x) # Extract PCA scores

# Ensure that the rows in full_data match the number of rows in pca_scores
# If there are rows with missing values or other issues, subset them accordingly.
nrow(full_data)  # 244
nrow(pca_scores)  # 229
full_data_matched <- full_data[rownames(pca_scores), ] # Subset full_data to match the rows in pca_scores
full_data_pca <- cbind(full_data_matched, pca_scores) # Now combine the datasets
nrow(full_data_pca)  # Should match nrow(pca_scores) # Check the result

model_PC1 <- lm(PC1 ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, data = full_data_pca) # Run regression: PC1 as dependent variable
summary(model_PC1)




# Yard Items ONLY
yard_data_Items <- full_data[, c("Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", 
                                 "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", 
                                 "Q10_9_Woodpile")]

yard_data_scaled_Items <- scale(yard_data_Items)
sum(is.na(yard_data_scaled_Items)) 
sum(is.infinite(yard_data_scaled_Items))

yard_data_scaled_Items <- na.omit(yard_data_scaled_Items)

pca_result_Items <- prcomp(yard_data_scaled_Items, center = TRUE, scale. = TRUE)
summary(pca_result_Items)

screeplot(pca_result_Items, type = "lines", main = "Scree Plot")
fviz_eig(pca_result_Items)
fviz_pca_biplot(pca_result_Items, label = "var")
fviz_pca_var(pca_result_Items, col.var = "cos2", gradient.cols = c("blue", "red")) 

pca_scores_Items <- as.data.frame(pca_result_Items$x) # Extract PCA scores

nrow(full_data)
nrow(pca_scores_Items)
full_data_matched_Items <- full_data[rownames(pca_scores_Items), ]
full_data_pca_Items <- cbind(full_data_matched_Items, pca_scores_Items) 
nrow(full_data_pca_Items)

model_PC1_Items <- lm(PC1 ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, data = full_data_pca_Items) 
summary(model_PC1_Items)




# Yard Actions ONLY
yard_data_Actions <- full_data[, c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", 
                                   "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash",
                                   "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab")]
colnames(yard_data_Actions)

yard_data_scaled_Actions <- scale(yard_data_Actions)

sum(is.na(yard_data_scaled_Actions))
sum(is.infinite(yard_data_scaled_Actions))
yard_data_scaled_Actions <- na.omit(yard_data_scaled_Actions)

pca_result_Actions <- prcomp(yard_data_scaled_Actions, center = TRUE, scale. = TRUE) # Run PCA
summary(pca_result_Actions)

screeplot(pca_result_Actions, type = "lines", main = "Scree Plot")
fviz_eig(pca_result_Actions)
fviz_pca_biplot(pca_result_Actions, label = "var")
fviz_pca_var(pca_result_Actions, col.var = "cos2", gradient.cols = c("blue", "red"))

pca_scores_Actions <- as.data.frame(pca_result_Actions$x)

nrow(full_data)
nrow(pca_scores_Actions)
full_data_matched_Actions <- full_data[rownames(pca_scores_Actions), ] 
full_data_pca_Actions <- cbind(full_data_matched_Actions, pca_scores_Actions) 
nrow(full_data_pca_Actions)

model_PC1_Actions <- lm(PC1 ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, data = full_data_pca_Actions)
summary(model_PC1_Actions)



# Yard Types ONLY
yard_data_Types <- full_data[, c( "Q11_1_Grass", "Q11_2_Hardscape", 
                                  "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")]
colnames(yard_data_Types)

yard_data_Types <- yard_data_Types %>%
  mutate(across(starts_with("Q11_"), ~ log(. + 0.1)))  

yard_data_scaled_Types <- scale(yard_data_Types)
sum(is.na(yard_data_scaled_Types))   
sum(is.infinite(yard_data_scaled_Types)) 

yard_data_scaled_Types <- na.omit(yard_data_scaled_Types)

pca_result_Types <- prcomp(yard_data_scaled_Types, center = TRUE, scale. = TRUE) # Run PCA
summary(pca_result_Types)

screeplot(pca_result_Types, type = "lines", main = "Scree Plot")
fviz_eig(pca_result_Types)
fviz_pca_biplot(pca_result_Types, label = "var")
fviz_pca_var(pca_result_Types, col.var = "cos2", gradient.cols = c("blue", "red")) 

pca_scores_Types <- as.data.frame(pca_result_Types$x)

nrow(full_data)
nrow(pca_scores_Types) 
full_data_matched_Types <- full_data[rownames(pca_scores_Types), ] 
full_data_pca_Types <- cbind(full_data_matched_Types, pca_scores_Types)
nrow(full_data_pca_Types) 

model_PC1_Types <- lm(PC1 ~ Utilitarian_Index + Biocentric_Index + Stewardship_Index, data = full_data_pca_Types)
summary(model_PC1_Types)



# Violn Plots

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Assuming you have the Q11_ yard type percentages and encounter frequencies
# Let's make a dataset that includes the yard types and encounter frequencies
# Select the relevant columns: Q11_ variables for yard types and Q1_CoySH/Q2_CatSH for encounter frequencies
plot_data <- full_data_EXP[, c("Q1_CoySH", "Q2_CatSH", "Q11_1_Grass", 
                               "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", 
                               "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")]

# Reshape the data into a long format for plotting
melted_data <- melt(plot_data, id.vars = c("Q1_CoySH", "Q2_CatSH"), 
                    measure.vars = c("Q11_1_Grass", "Q11_2_Hardscape", 
                                     "Q11_3_Veg", "Q11_4_Mulch", 
                                     "Q11_5_Shrub", "Q11_6_Tree"))

# Create a new column that groups yard types into two categories: 
# 1 for Grass, Hardscape, Mulch; 2 for Veg, Shrub, Tree
melted_data$group <- ifelse(melted_data$variable %in% c("Q11_1_Grass", "Q11_2_Hardscape", "Q11_4_Mulch"), 
                            "Grass, Hardscape, Mulch", "Veg, Shrub, Tree")

# Create the violin plot comparing yard types (Q11_) and encounter frequencies (Coyotes and Cats)
ggplot(melted_data, aes(x = value, y = ifelse(grepl("Coy", variable), Q1_CoySH, Q2_CatSH), fill = variable)) +
  geom_violin(trim = FALSE, scale = "area") +
  facet_wrap(~ group + ifelse(grepl("Coy", variable), "Coyote", "Cat"), scales = "free_y", ncol = 2) +
  labs(title = "Distribution of Yard Types by Encounter Frequencies (Coyote and Cat)",
       x = "Yard Type Percentage", 
       y = "Encounter Frequency (Coyote or Cat)",
       fill = "Yard Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) # Adjust x-axis labels if necessary







#### end ####

#### ChatGPT Recommended Visualizations ####

# Scatter Plot Matrix / Pairplot
# Load necessary libraries for pairplot
install.packages("GGally")
library(GGally)

# Prepare the data
scatter_data <- full_data_EXP[, c("Q1_CoySH", "Q2_CatSH", "Q11_1_Grass", 
                                  "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", 
                                  "Q11_5_Shrub", "Q11_6_Tree")]

# Create scatter plot matrix
ggpairs(scatter_data, aes(color = factor(Q1_CoySH), alpha = 0.5), 
        title = "Scatter Plot Matrix for Yard Types and Encounter Frequencies")







# Side-by-Side Bar Charts
# Prepare the data in long format
bar_data <- melt(full_data_EXP[, c("Q1_CoySH", "Q2_CatSH", 
                                   "Q11_1_Grass", "Q11_2_Hardscape", 
                                   "Q11_3_Veg", "Q11_4_Mulch", 
                                   "Q11_5_Shrub", "Q11_6_Tree")],
                 id.vars = c("Q1_CoySH", "Q2_CatSH"))

# Create a factor variable for each yard type
bar_data$yard_type <- factor(bar_data$variable)

# Create a factor variable for the encounter frequency (Coyote and Cat)
bar_data$Freq_Coyote <- cut(bar_data$Q1_CoySH, breaks = c(0, 6, 26, 104), 
                            labels = c("None", "Low", "High"))

bar_data$Freq_Cat <- cut(bar_data$Q2_CatSH, breaks = c(0, 6, 26, 104), 
                         labels = c("None", "Low", "High"))


# Create the side-by-side bar chart for Coyote
ggplot(bar_data, aes(x = yard_type, fill = Freq_Coyote)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart: Yard Types vs. Coyote Encounter Frequencies", 
       x = "Yard Type", y = "Count") +
  theme_minimal()

# Create the side-by-side bar chart for Cat
ggplot(bar_data, aes(x = yard_type, fill = Freq_Cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart: Yard Types vs. Cat Encounter Frequencies", 
       x = "Yard Type", y = "Count") +
  theme_minimal()



#### end ####




#### Do ^ again but only using negative encounters? No enough to see? ####

# Define the resistance group (binary variables, and percentages normalized)
negative_encounters_vars <- c("Q3_4_CoyFS", "Q3_6_CoyPA", "Q4_4_CatFS", "Q4_7_CatPA")
full_data$negative_encounters_vars <- rowSums(full_data[negative_encounters_vars], na.rm = TRUE)
summary(full_data$negative_encounters_vars)




#### end ####








"Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash", "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other", "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", "Q10_9_Woodpile",, "Q10_10_Other", "Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other", "Q10_8_Wind", "Q10_9_Woodpile", "Q10_10_Other"