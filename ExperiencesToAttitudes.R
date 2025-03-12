
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)
SurveyResults_Cleaned<-SurveyResults_AND_Demographics
data<-SurveyResults_Cleaned
summary(data)
# Filter out -99 values for analysis (but do not replace in dataset)
data_filtered <- data %>% filter(!if_any(everything(), ~ . == -99))

#### Grouping ####
attitude_Bio <- c("Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")

attitude_Util <- c("Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")

attitud_Stew <- c("Q12_2_WProtect", "Q12_6_Wexplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")

# Correlation analysis between experiences and attitudes
experience_vars <- c("Q1_CoySH", "Q2_CatSH", "Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q3_4_CoyFS", "Q3_5_CoyFed", "Q3_6_CoyPA", "Q3_7_CoyOther", "Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q4_4_CatFS", "Q4_5_CatFed", "Q4_6_CatTouch", "Q4_7_CatPA", "Q4_8_CatOther", "Q5_1_MyCat", "Q5_2_NeighCat", "Q5_3_Stray", "Q5_4_Unsure")


attitude_vars <- c("Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO", "Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO", "Q8_1_GenN", "Q8_2_GenGS", "Q8_3_GenECoy", "Q8_4_GenECat", "Q8_5_GenVeg", "Q8_6_GenYard", "Q8_7_GenUY", "Q12_1_WLove", "Q12_2_WProtect", "Q12_3_WUse", "Q12_4_WControl", "Q12_5_WEmbrace", "Q12_6_Wexplore", "Q12_7_WStudy", "Q12_8_WAvoid", "Q12_9_WCare", "Q12_10_WIgnore", "Q12_11_WUnderst", "Q12_12_WAdmire", "Q12_13_WRespect")

small_behavior_vars <- c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash", "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other", "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", "Q10_9_Woodpile", "Q10_10_Other")

experience_freq_vars <- c("Q1_CoySH", "Q2_CatSH")

experience_type_vars <- c("Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q3_4_CoyFS", "Q3_5_CoyFed", "Q3_6_CoyPA", "Q3_7_CoyOther", 
                          "Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q4_4_CatFS", "Q4_5_CatFed", "Q4_6_CatTouch", "Q4_7_CatPA", "Q4_8_CatOther", 
                          "Q5_1_MyCat", "Q5_2_NeighCat", "Q5_3_Stray", "Q5_4_Unsure")
#### end ####

#### Visualizing: Experiences -> Attitudes ####

cor_matrix <- cor(data_filtered[, c(experience_vars, attitude_vars, small_behavior_vars)], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "number")

# Compute correlations
cor_matrix <- cor(data_filtered[, c(attitude_vars, experience_freq_vars, experience_type_vars)], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "number")

# Run regression analyses and store results
regression_results <- list()

for (att in attitude_vars) {
  for (exp in c(experience_freq_vars, experience_type_vars)) {
    model <- lm(data_filtered[[att]] ~ data_filtered[[exp]], data = data_filtered)
    regression_results[[paste(att, exp, sep = " ~ ")]] <- summary(model)
  }
}

# Print significant results
for (name in names(regression_results)) {
  result <- regression_results[[name]]
  if (any(result$coefficients[,4] < 0.05, na.rm = TRUE)) {
    print(name)
    print(result)
  }
}

# Create scatter plots with trend lines
plot_list <- list()
plot_counter <- 1
for (att in attitude_vars) {
  for (exp in experience_freq_vars) {
    plot_list[[plot_counter]] <- ggplot(data_filtered, aes(x = data_filtered[[exp]], y = data_filtered[[att]])) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = paste(exp, "vs", att), x = exp, y = att)
    plot_counter <- plot_counter + 1
  }
}

# Display multiple plots
library(gridExtra)
do.call("grid.arrange", c(plot_list[1:6], ncol = 3))

#### Q1 w/ Q6 ####
library(gridExtra)
p1 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_1_CoyTh)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Threat to People")
p2 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_2_CoyIH)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Important to Humans")
p3 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_3_CoyIE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Important to Environment")
p4 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_4_CoyThCat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Threat to Cats")
p5 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_5_CoyEF)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Not Needed")
p6 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_6_CoyFM)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Frightening")
p7 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_7_CoyBeau)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Beautiful")
p8 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = Q6_8_CoyKO)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Keep Away")

grid.arrange(p1, p2, ncol = 2)
library(patchwork)
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 # Simply add plots together
#### end ####
#### Q2 w/ Q6 ####
library(gridExtra)
p1 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_1_CoyTh)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Threat to People")
p2 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_2_CoyIH)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Important to Humans")
p3 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_3_CoyIE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Important to Environment")
p4 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_4_CoyThCat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Threat to Cats")
p5 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_5_CoyEF)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Not Needed")
p6 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_6_CoyFM)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Frightening")
p7 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_7_CoyBeau)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Beautiful")
p8 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = Q6_8_CoyKO)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Keep Away")

grid.arrange(p1, p2, ncol = 2)
library(patchwork)
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 # Simply add plots together
#### end ####
# ^ Should I repeat this to show ALL of them? (Q3 - Q5 against all attitudes)
ggplot(data_filtered, aes(x = Q1_CoySH, y = attitude_Bio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Experience vs Attitude", x = "Coyote Sightings", y = "Coyote Attitudes: Threat to People")


#### Yard Type w/ Experiences ####
q11_vars <- c("Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")
data_filtered <- data_filtered %>% mutate(across(all_of(q11_vars), as.numeric))
q11_summary <- data_filtered %>% summarise(across(all_of(q11_vars), mean, na.rm = TRUE))
print(q11_summary)

# Define yard type variables
q11_vars <- c("Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", 
              "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")

# Convert experience and yard type columns to numeric
data <- data %>% 
  mutate(across(all_of(experience_vars), as.numeric),
         across(all_of(q11_vars), as.numeric))

# Select only experience and yard type variables
experience_yard_data <- data %>%
  select(all_of(experience_vars), all_of(q11_vars)) %>%
  drop_na()

# Compute Pearson correlation
cor_results <- cor(experience_yard_data, use = "pairwise.complete.obs", method = "pearson")
print(cor_results)
corrplot(cor_results, method = "square")
corrplot(cor_results, method = "number")

# Compute Spearman correlation (if non-linear)
cor_results_spearman <- cor(experience_yard_data, use = "pairwise.complete.obs", method = "spearman")
print(cor_results_spearman)
corrplot(cor_results_spearman, method = "square")
corrplot(cor_results_spearman, method = "number")

# Compute correlation tests for each yard type
cor_test_results <- lapply(q11_vars, function(var) {
  test <- cor.test(data[[var]], data[[experience_vars]], method = "pearson", use = "complete.obs")
  return(data.frame(Yard_Type = var, Correlation = test$estimate, P_Value = test$p.value))
})

# Combine into a table
cor_test_results_df <- do.call(rbind, cor_test_results)
print(cor_test_results_df)



# Initialize an empty list to store results
cor_test_results <- list()

# Loop through each experience variable and compute correlation with each yard type variable
for (exp_var in experience_vars) {
  for (yard_var in q11_vars) {
    # Ensure there are no NA values before correlation test
    valid_data <- data %>% select(all_of(exp_var), all_of(yard_var)) %>% drop_na()
    
    # Run correlation test only if there is sufficient data
    if (nrow(valid_data) > 2) {  # Correlation needs at least 3 data points
      test <- cor.test(valid_data[[exp_var]], valid_data[[yard_var]], method = "pearson")
      cor_test_results <- append(cor_test_results, 
                                 list(data.frame(Experience = exp_var, 
                                                 Yard_Type = yard_var, 
                                                 Correlation = test$estimate, 
                                                 P_Value = test$p.value)))
    }
  }
}

# Convert the list into a data frame
cor_test_results_df <- do.call(rbind, cor_test_results)
print(cor_test_results_df)


# Initialize an empty list to store model results
lm_results <- list()

# Loop through each experience variable and fit a linear model
for (exp_var in experience_vars) {
  # Ensure that both dependent (experience) and independent (yard type) variables exist and are numeric
  valid_data <- data %>% select(all_of(exp_var), all_of(q11_vars)) %>% drop_na()
  
  # Check if there is enough data to fit the model
  if (nrow(valid_data) > length(q11_vars)) {  # Need more observations than predictors
    formula <- as.formula(paste(exp_var, "~", paste(q11_vars, collapse = " + ")))
    model <- lm(formula, data = valid_data)
    
    # Store summary in list
    lm_results[[exp_var]] <- summary(model)
  }
}

# Print results
lm_results


#### end ####


#### Attitudes w/ Exp Stats ####
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)
library(psych)

# Load dataset
data <- read_csv("SurveyResults_AND_Demographics.csv")
data_filtered <- data %>% filter(!if_any(everything(), ~ . == -99))
data[data == -99] <- NA


# Add computed indices to dataset
data$attitude_Bio <- attitude_Bio
data$attitude_Util <- attitude_Util
data$attitude_Stew <- attitud_Stew

# Compute attitude indices within mutate to maintain row integrity
data <- data %>%
  mutate(
    attitude_Bio = rowMeans(select(., c("Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")), na.rm = TRUE),
    attitude_Util = rowMeans(select(., c("Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")), na.rm = TRUE),
    attitude_Stew = rowMeans(select(., c("Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")), na.rm = TRUE)
  )

# Determine the most prevalent attitude per response
data <- data %>%
  mutate(
    dominant_attitude = case_when(
      attitude_Bio >= attitude_Stew & attitude_Bio >= attitude_Util ~ "Bio-Centric",
      attitude_Stew >= attitude_Bio & attitude_Stew >= attitude_Util ~ "Stewardship",
      TRUE ~ "Utilitarian"
    )
  )

# Correlation analysis
cor_matrix <- cor(data[, c(experience_vars, "attitude_Bio", "attitude_Util", "attitude_Stew")], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "number")

# Linear regression models
lm_Bio <- lm(attitude_Bio ~ ., data = data_filtered[, c(experience_vars, "attitude_Bio")])
lm_Util <- lm(attitude_Util ~ ., data = data_filtered[, c(experience_vars, "attitude_Util")])
lm_Stew <- lm(attitude_Stew ~ ., data = data_filtered[, c(experience_vars, "attitude_Stew")])

summary(lm_Bio)
summary(lm_Util)
summary(lm_Stew)


# Scatter plots with regression lines
p1 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = attitude_Bio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "a. Coyote Sightings vs Bio-centric Attitude", x = "Coyote Sightings", y = "Bio-centric Attitude")
p2 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = attitude_Stew)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "b. Coyote Sightings vs Stewardship Attitude", x = "Coyote Sightings", y = "Stewardship Attitude")
p3 <- ggplot(data_filtered, aes(x = Q1_CoySH, y = attitude_Util)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "c. Coyote Sightings vs Utilitarian Attitude", x = "Coyote Sightings", y = "Utilitarian Attitude")
grid.arrange(p1, p2, p3, ncol=2, nrow=2)

p1.1 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = attitude_Bio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "a. Cat Sightings vs Bio-centric Attitude", x = "Cat Sightings", y = "Bio-centric Attitude")
p2.1 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = attitude_Stew)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "b. Cat Sightings vs Stewardship Attitude", x = "Cat Sightings", y = "Stewardship Attitude")
p3.1 <- ggplot(data_filtered, aes(x = Q2_CatSH, y = attitude_Util)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "c. Cat Sightings vs Utilitarian Attitude", x = "Cat Sightings", y = "Utilitarian Attitude")
grid.arrange(p1.1, p2.1, p3.1, ncol=2, nrow=2)

#### end ####
