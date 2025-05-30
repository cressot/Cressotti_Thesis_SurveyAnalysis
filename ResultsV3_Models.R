
# Are attitudes predictors to behaviors in yards + yard types? 

SurveyResults_AND_Demographics <- read.csv("~/Desktop/Survey Chapter/Data/Survey Stats + Analysis/SurveyResults_AND_Demographics.csv")
GoodBad_Scoring_Survey <- read.csv("~/Downloads/GoodBad_Scoring_Survey.csv")


colnames(SurveyResults_AND_Demographics)
str(SurveyResults_AND_Demographics)
SurveyResults_AND_Demographics <- SurveyResults_AND_Demographics %>% 
  mutate(across(c("Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO", "Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO", "Q8_1_GenN", "Q8_2_GenGS", "Q8_3_GenECoy", "Q8_4_GenECat", "Q8_5_GenVeg", "Q8_6_GenYard", "Q8_7_GenUY", "Q12_1_WLove", "Q12_2_WProtect", "Q12_3_WUse", "Q12_4_WControl", "Q12_5_WEmbrace", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_8_WAvoid", "Q12_9_WCare", "Q12_10_WIgnore", "Q12_11_WUnderst", "Q12_12_WAdmire", "Q12_13_WRespect"), 
                as.numeric))
full_data <- SurveyResults_AND_Demographics
full_data[full_data == -99] <- NA
full_data[full_data == 99] <- NA


#OPTIONAL to REMOVE na's
full_data_clean <- na.omit(full_data)
colnames(full_data_clean)
str(full_data_clean)


#### Putting Attitude Index Together####
# Calculate the Utilitarian Index
full_data$Utilitarian_Index <- rowMeans(full_data[, c(
  "Q8_4_GenECat", "Q8_7_GenUY", "Q12_3_WUse", "Q12_4_WControl", 
  "Q12_8_WAvoid", "Q12_10_WIgnore")], na.rm = TRUE)

# Calculate the Biocentric Index
full_data$Biocentric_Index <- rowMeans(full_data[, c(
  "Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg", "Q12_1_WLove", "Q12_5_WEmbrace", 
  "Q12_9_WCare", "Q12_12_WAdmire")], na.rm = TRUE)

# Calculate the Stewardship Index
full_data$Stewardship_Index <- rowMeans(full_data[, c(
  "Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")], na.rm = TRUE)

# Calculate the Coyote Index
full_data$Coyote_Index <- rowMeans(full_data[, c(
  "Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", 
  "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO")], na.rm = TRUE)

# Calculate the Cat Index
full_data$Cat_Index <- rowMeans(full_data[, c(
  "Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", 
  "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO")], na.rm = TRUE)

# Calculate the Exp Type Index
full_data$ExpTypeNeg_Index <- rowMeans(full_data[, c(
  "Q3_4_CoyFS", "Q3_6_CoyPA", "Q4_4_CatFS", "Q4_7_CatPA")], na.rm = TRUE)

# Calculate the Exp Type Index
full_data$ExpTypePos_Index <- rowMeans(full_data[, c(
  "Q3_5_CoyFed", "Q4_5_CatFed", "Q4_6_CatTouch")], na.rm = TRUE)






# Calculate the Exp Freq Index
full_data_EXP$Q1_CoySH <- as.numeric(full_data_EXP$Q1_CoySH)
full_data_EXP$Q2_CatSH <- as.numeric(full_data_EXP$Q2_CatSH)
full_data$ExpFreq_Index <- rowMeans(full_data[, c(
  "Q1_CoySH", "Q2_CatSH")], na.rm = TRUE)




#### end ####



#### Normality ####

# Fit the model
model_all <- lm(cbind(Utilitarian_Index, Biocentric_Index, Stewardship_Index, Coyote_Index, Cat_Index, ExpTypeNeg_Index, ExpTypePos_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = full_data)


# Log-transform Q11_ variables (adding a small constant)
full_data$Q11_1_Grass_log <- log(full_data$Q11_1_Grass + 0.1)
full_data$Q11_2_Hardscape_log <- log(full_data$Q11_2_Hardscape + 0.1)
full_data$Q11_3_Veg_log <- log(full_data$Q11_3_Veg + 0.1)
full_data$Q11_4_Mulch_log <- log(full_data$Q11_4_Mulch + 0.1)
full_data$Q11_5_Shrub_log <- log(full_data$Q11_5_Shrub + 0.1)
full_data$Q11_6_Tree_log <- log(full_data$Q11_6_Tree + 0.1)
full_data$Q11_7_Other_log <- log(full_data$Q11_7_Other + 0.1)

# Normalize percentage variables for the neutral group (example)
full_data$Q11_1_Grass_norm <- full_data$Q11_1_Grass / 100  # Normalize to 0-1 range
full_data$Q11_2_Hardscape_norm <- full_data$Q11_2_Hardscape / 100
full_data$Q11_3_Veg_norm <- full_data$Q11_3_Veg / 100
full_data$Q11_4_Mulch_norm <- full_data$Q11_4_Mulch / 100
full_data$Q11_5_Shrub_norm <- full_data$Q11_5_Shrub / 100
full_data$Q11_6_Tree_norm <- full_data$Q11_6_Tree / 100
full_data$Q11_7_Other_norm <- full_data$Q11_7_Other / 100

#logged Q11 Model
model_all_log <- lm(cbind(Utilitarian_Index, Biocentric_Index, Stewardship_Index, Coyote_Index, Cat_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass_log + Q11_2_Hardscape_log + Q11_3_Veg_log + Q11_4_Mulch_log + Q11_5_Shrub_log + Q11_6_Tree_log, data = full_data)

#norm Q11 Model
model_all_norm <- lm(cbind(Utilitarian_Index, Biocentric_Index, Stewardship_Index, Coyote_Index, Cat_Index, ExpTypeNeg_Index, ExpTypePos_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass_norm + Q11_2_Hardscape_norm + Q11_3_Veg_norm + Q11_4_Mulch_norm + Q11_5_Shrub_norm + Q11_6_Tree_norm, data = full_data)


# Q-Q plot to check normality
qqnorm(residuals(model_all))
qqline(residuals(model_all), col = "red")

qqnorm(residuals(model_all_log))
qqline(residuals(model_all_log), col = "red")

qqnorm(residuals(model_all_norm))
qqline(residuals(model_all_norm), col = "red")


# Shapiro-Wilk test for normality
shapiro.test(residuals(model_all))
shapiro.test(residuals(model_all_log))
shapiro.test(residuals(model_all_norm))

#Original / normalized is slightly better to use as per ChatGPT 


plot(fitted(model_all), residuals(model_all),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
library(lmtest)
bptest(model_all)
plot(cooks.distance(model_all), type = "h",
     ylab = "Cook's Distance", main = "Influential Observations")
abline(h = 4/length(residuals(model_all)), col = "red", lty = 2)

#Heteroscedasticity detected - Confirmed by Breusch-Pagan test

#### end ####


#### Running Model ####
#Robust Regression on Model 
install.packages("sandwich")
library(sandwich)
library(lmtest)
coeftest(model_all, vcov = vcovHC(model_all, type = "HC1"))
coeftest(model_all, vcov = vcovHC(model_all_norm, type = "HC1"))

#Multipel Regression Model on ALL
summary(model_all)
summary(model_all_norm)


#Backward Stepwise Regression
data_clean <- na.omit(full_data)
model_util <- lm(cbind(Utilitarian_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = data_clean)

model_Bio <- lm(cbind(Biocentric_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = data_clean)

model_Stew <- lm(cbind(Stewardship_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = data_clean)

model_Coy <- lm(cbind(Coyote_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = data_clean)

model_Cat <- lm(cbind(Cat_Index) ~ Q9_1_CatOut + Q9_2_TreeVeg + Q9_3_Exter + Q9_4_Trap + Q9_5_Lights + Q9_6_PetFood + Q9_7_DogOff + Q9_8_LockTrash + Q9_9_Prune + Q9_10_PestHerb + Q9_11_CreateHab + Q10_1_Bbath + Q10_2_Bfeed + Q10_3_Fence + Q10_4_Flower + Q10_5_Fruit + Q10_6_Water + Q10_7_Veg + Q10_8_Wind + Q10_9_Woodpile + Q11_1_Grass + Q11_2_Hardscape + Q11_3_Veg + Q11_4_Mulch + Q11_5_Shrub + Q11_6_Tree, data = data_clean)

step_Util <- step(model_util, direction = "backward")
summary(step_Util)
step_Bio <- step(model_Bio, direction = "backward")
summary(step_Bio)
step_Stew <- step(model_Stew, direction = "backward")
summary(step_Stew)
step_Coy <- step(model_Coy, direction = "backward")
summary(step_Coy)
step_Cat <- step(model_Cat, direction = "backward")
summary(step_Cat)


#Encouraging + Discouraging 
GoodBad_Scoring_Survey <- read.csv("~/Downloads/GoodBad_Scoring_Survey.csv")
colnames(GoodBad_Scoring_Survey)

library(dplyr)
# Define good and bad action columns
good_actions <- c(
  "Q9_6_PetFood", "Q9_11_CreateHab", "Q10_1_Bbath", "Q10_2_Bfeed",
  "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg",
  "Q10_9_Woodpile", "Q11_3_Veg", "Q11_5_Shrub", "Q11_6_Tree"
)
bad_actions <- c(
  "Q9_.1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights",
  "Q9_7_DogOff", "Q9_8_LockTrash", "Q9_9_Prune", "Q9_.10_PestHerb",
  "Q.10_3_Fence", "Q.10_8_Wind", "Q11_2_Hardscape"
)


# Calculate totals
GoodBad_Scoring_Survey <- GoodBad_Scoring_Survey %>%
  mutate(
    total_good = rowSums(select(., all_of(good_actions)), na.rm = TRUE),
    total_bad = rowSums(select(., all_of(bad_actions)), na.rm = TRUE),
    final_score = as.numeric(Total_.1.01)  # Adjust column name as needed
  )


hist(GoodBad_Scoring_Survey$final_score)
hist(GoodBad_Scoring_Survey$total_good)
hist(GoodBad_Scoring_Survey$total_bad)

# Summarize results
summary_stats <- list(
  Good_Actions = list(
    Mean = mean(GoodBad_Scoring_Survey$total_good, na.rm = TRUE),
    Median = median(GoodBad_Scoring_Survey$total_good, na.rm = TRUE),
    Range = range(GoodBad_Scoring_Survey$total_good, na.rm = TRUE)
  ),
  Bad_Actions = list(
    Mean = mean(GoodBad_Scoring_Survey$total_bad, na.rm = TRUE),
    Median = median(GoodBad_Scoring_Survey$total_bad, na.rm = TRUE),
    Range = range(GoodBad_Scoring_Survey$total_bad, na.rm = TRUE)
  ),
  Final_Score = list(
    Mean = mean(GoodBad_Scoring_Survey$final_score, na.rm = TRUE),
    Median = median(GoodBad_Scoring_Survey$final_score, na.rm = TRUE),
    Range = range(GoodBad_Scoring_Survey$final_score, na.rm = TRUE)
  ),
  Correlation = cor(GoodBad_Scoring_Survey$total_good, GoodBad_Scoring_Survey$total_bad, use = "complete.obs")
)
print(summary_stats)

colnames(full_data)
colnames(GoodBad_Scoring_Survey)


install.packages("corrplot")  # Install if needed
library(corrplot)
source("~/Desktop/PSU 2024-2025/R_Tools_Examples_Script/Scripts/cor.matrix.r")

cor_matrix <- cor(full_data[, c("Utilitarian_Index", "Biocentric_Index", "Stewardship_Index", 
                                "total_good", "total_bad", "final_score")], 
                  use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))



cor_matrix <- cor(full_data[, c(
  "ExpTypeNeg_Index", "ExpTypePos_Index", "Q1_CoySH", "Q2_CatSH",
  "total_good", "total_bad", "final_score"
)], use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))
cor_matrix <- cor(full_data[, c("Utilitarian_Index", "Biocentric_Index", "Stewardship_Index", 
                                "total_good", "total_bad", "final_score")], 
                  use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))
cor_matrix <- cor(full_data[, c(
  "Utilitarian_Index", "Biocentric_Index", "Stewardship_Index", "Coyote_Index", "Cat_Index",
  "total_good", "total_bad", "final_score"
)], use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))



#Comparing to Attitudes
GoodBad_Scoring_Survey$SurveyID <- as.numeric(GoodBad_Scoring_Survey$SurveyID)
full_data$SurveyID <- as.numeric(full_data$SurveyID)

full_data <- full_data %>%
  left_join(GoodBad_Scoring_Survey %>% select(SurveyID, total_good, total_bad, final_score), by = "SurveyID")

# Correlation matrix of attitudes vs yard behavior
attitudes <- full_data %>%
  select(Coyote_Index, Cat_Index, Stewardship_Index, Biocentric_Index, Utilitarian_Index)
yard_scores <- full_data %>%
  select(total_good, total_bad, final_score)

cor_matrix <- cor(cbind(yard_scores, attitudes), use = "complete.obs")
print(round(cor_matrix, 2))


model_good <- lm(total_good ~ Coyote_Index + Cat_Index + Stewardship_Index + Biocentric_Index + Utilitarian_Index + ExpTypeNeg_Index + ExpTypePos_Index + Q2_CatSH + Q1_CoySH, data = full_data) # Model total_good score based on attitudes
summary(model_good)

model_bad <- lm(total_bad ~ Coyote_Index + Cat_Index + Stewardship_Index + Biocentric_Index + Utilitarian_Index + ExpTypeNeg_Index + ExpTypePos_Index + Q2_CatSH + Q1_CoySH, data = full_data) # Model total_bad actions
summary(model_bad)

model_final <- lm(final_score ~ Coyote_Index + Cat_Index + Stewardship_Index + Biocentric_Index + Utilitarian_Index + ExpTypeNeg_Index + ExpTypePos_Index + Q2_CatSH + Q1_CoySH, data = full_data) # Final score model
summary(model_final)


#Visualize
library(ggplot2)

#Good
p1 <- ggplot(full_data, aes(x = Utilitarian_Index, y = total_good)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Utilitarian Attitudes Index", y = "Actions Benefiting Wildlife") 
p2 <- ggplot(full_data, aes(x = Biocentric_Index, y = total_good)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Biocentric Attitudes Index", y = "Actions Benefiting Wildlife") # Example: Biocentric vs Good Actions
p3 <- ggplot(full_data, aes(x = Stewardship_Index, y = total_good)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Stewardship Attitudes Index", y = "Actions Benefiting Wildlife") 
p4 <- ggplot(full_data, aes(x = Coyote_Index, y = total_good)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Attitudes Index", y = "Actions Benefiting Wildlife") 
p5 <- ggplot(full_data, aes(x = Cat_Index, y = total_good)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Attitudes Index", y = "Actions Benefiting Wildlife") 

p6 <- ggplot(full_data, aes(x = ExpTypeNeg_Index, y = total_good)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Negative Experiences", y = "Actions Benefiting Wildlife")

p7 <- ggplot(full_data, aes(x = ExpTypePos_Index, y = total_good)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Positive Experiences", y = "Actions Benefiting Wildlife")

p8 <- ggplot(full_data, aes(x = Q1_CoySH, y = total_good)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Experience Frequency", y = "Actions Benefiting Wildlife")

p9 <- ggplot(full_data, aes(x = Q2_CatSH, y = total_good)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Experience Frequency", y = "Actions Benefiting Wildlife")

(p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | p8 | p9)


#Bad
p1 <- ggplot(full_data, aes(x = Utilitarian_Index, y = total_bad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Utilitarian Attitudes Index", y = "Actions Benefiting Wildlife") 
p2 <- ggplot(full_data, aes(x = Biocentric_Index, y = total_bad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Biocentric Attitudes Index", y = "Actions Benefiting Wildlife") 
p3 <- ggplot(full_data, aes(x = Stewardship_Index, y = total_bad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Stewardship Attitudes Index", y = "Actions Benefiting Wildlife") 
p4 <- ggplot(full_data, aes(x = Coyote_Index, y = total_bad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Attitudes Index", y = "Actions Benefiting Wildlife") 
p5 <- ggplot(full_data, aes(x = Cat_Index, y = total_bad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Attitudes Index", y = "Actions Benefiting Wildlife") 

p6 <- ggplot(full_data, aes(x = ExpTypeNeg_Index, y = total_bad)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Negative Experiences", y = "Actions Benefiting Wildlife")

p7 <- ggplot(full_data, aes(x = ExpTypePos_Index, y = total_bad)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Positive Experiences", y = "Actions Benefiting Wildlife")

p8 <- ggplot(full_data, aes(x = Q1_CoySH, y = total_bad)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Experience Frequency", y = "Actions Benefiting Wildlife")

p9 <- ggplot(full_data, aes(x = Q2_CatSH, y = total_bad)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Experience Frequency", y = "Actions Benefiting Wildlife")

(p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | p8 | p9)



#Full
ggplot(full_data, aes(x = Utilitarian_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Utilitarian Attitudes vs Wildlife-Friendly Actions",
       x = "Utilitarian Index", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Biocentric_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Biocentric Attitudes vs Wildlife-Friendly Actions",
       x = "Biocentric Index", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Stewardship_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Stewardship Attitudes vs Wildlife-Friendly Actions",
       x = "Stewardship Index", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Coyote_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Coyote Attitudes vs Wildlife-Friendly Actions",
       x = "Coyote Index", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Cat_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Cat Attitudes vs Wildlife-Friendly Actions",
       x = "Cat Index", y = "Actions Benefiting Wildlife") 


ggplot(full_data, aes(x = ExpTypeNeg_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Experinces vs Wildlife-Friendly Actions",
       x = "Negative Experinces", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = ExpTypePos_Index, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Experinces vs Wildlife-Friendly Actions",
       x = "Positive Experinces", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Q1_CoySH, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Experince Frequency vs Wildlife-Friendly Actions",
       x = "Coyote Experince Frequency", y = "Actions Benefiting Wildlife") 
ggplot(full_data, aes(x = Q2_CatSH, y = final_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Experince Frequency vs Wildlife-Friendly Actions",
       x = "Cat Experince Frequency", y = "Actions Benefiting Wildlife") 



install.packages("patchwork")
library(ggplot2)
library(patchwork)

p1 <- ggplot(full_data, aes(x = Utilitarian_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Utilitarian Attitudes Index", y = "Actions Benefiting Wildlife")

p2 <- ggplot(full_data, aes(x = Biocentric_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Biocentric Attitudes Index", y = "Actions Benefiting Wildlife")

p3 <- ggplot(full_data, aes(x = Stewardship_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Stewardship Attitudes Index", y = "Actions Benefiting Wildlife")

p4 <- ggplot(full_data, aes(x = Coyote_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Attitudes Index", y = "Actions Benefiting Wildlife")

p5 <- ggplot(full_data, aes(x = Cat_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Attitudes Index", y = "Actions Benefiting Wildlife")

p6 <- ggplot(full_data, aes(x = ExpTypeNeg_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Negative Experiences", y = "Actions Benefiting Wildlife")

p7 <- ggplot(full_data, aes(x = ExpTypePos_Index, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Positive Experiences", y = "Actions Benefiting Wildlife")

p8 <- ggplot(full_data, aes(x = Q1_CoySH, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Coyote Experience Frequency", y = "Actions Benefiting Wildlife")

p9 <- ggplot(full_data, aes(x = Q2_CatSH, y = final_score)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "",
       x = "Cat Experience Frequency", y = "Actions Benefiting Wildlife")

(p1 | p2 | p3) /
  (p4 | p5)

(p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p8 | p9 | p7)



# First convert the experience variables to factors
full_data$ExpTypeNeg_Factor <- as.factor(full_data$ExpTypeNeg_Index)
full_data$ExpTypePos_Factor <- as.factor(full_data$ExpTypePos_Index)
full_data$CoySH_Factor <- as.factor(full_data$Q1_CoySH)
full_data$CatSH_Factor <- as.factor(full_data$Q2_CatSH)

# Replace p6–p9 with categorical visualizations

p6 <- ggplot(full_data, aes(x = ExpTypeNeg_Factor, y = final_score)) +
  geom_boxplot(fill = "#F8766D", alpha = 0.7) +
  labs(title = "", x = "Negative Experiences (Grouped)", y = "Actions Benefiting Wildlife")

p7 <- ggplot(full_data, aes(x = ExpTypePos_Factor, y = final_score)) +
  geom_boxplot(fill = "#7CAE00", alpha = 0.7) +
  labs(title = "", x = "Positive Experiences (Grouped)", y = "Actions Benefiting Wildlife")

p8 <- ggplot(full_data, aes(x = CoySH_Factor, y = final_score)) +
  geom_boxplot(fill = "#00BFC4", alpha = 0.7) +
  labs(title = "", x = "Coyote Experience Frequency", y = "Actions Benefiting Wildlife")

p9 <- ggplot(full_data, aes(x = CatSH_Factor, y = final_score)) +
  geom_boxplot(fill = "#C77CFF", alpha = 0.7) +
  labs(title = "", x = "Cat Experience Frequency", y = "Actions Benefiting Wildlife")


(p6 | p7) /
  (p8 | p9)

# Round values before converting to factors
full_data$ExpTypeNeg_Factor <- as.factor(round(full_data$ExpTypeNeg_Index, 2))
full_data$ExpTypePos_Factor <- as.factor(round(full_data$ExpTypePos_Index, 2))



#### end ####




#### Visualizing PER Comments on Docs ####

# Required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Lookup vector for full question text
question_labels <- c(
  # Utilitarian
  Q8_4_GenECat = "Cats - enjoy (or would) in neighborhood",
  Q8_7_GenUY = "Work hard to upkeep yard",
  Q12_3_WUse = "Wildlife is here for us to use",
  Q12_4_WControl = "We need to control wildlife",
  Q12_8_WAvoid = "Humans need to avoid wildlife",
  Q12_10_WIgnore = "Humans need to ignore wildlife",
  
  # Biocentric
  Q8_1_GenN = "Like wildlife in my neighborhood",
  Q8_2_GenGS = "Greenspace is main attraction",
  Q8_5_GenVeg = "Native vegetation in yard",
  Q12_1_WLove = "Wildlife is something to be loved",
  Q12_5_WEmbrace = "Humans need to embrace wildlife",
  Q12_9_WCare = "Humans need to care for wildlife",
  Q12_12_WAdmire = "Wildlife should be admired",
  
  # Stewardship
  Q8_3_GenECoy = "Coyotes - enjoy (or would) in neighborhood",
  Q8_6_GenYard = "Yard is sanctuary",
  Q12_2_WProtect = "We need to protect wildlife",
  Q12_6_WExplore = "We need to explore nature",
  Q12_7_WStudy = "Wildlife is something to be studied",
  Q12_11_WUnderst = "We need to better understand wildlife",
  Q12_13_WRespect = "Wildlife should be respected",
  
  # Coyote Attitudes
  Q6_1_CoyTh = "Coyotes are a threat to people",
  Q6_2_CoyIH = "Coyotes are important to humans",
  Q6_3_CoyIE = "Coyotes are important to the environment",
  Q6_4_CoyThCat = "Coyotes are a threat to domestic cats",
  Q6_5_CoyEF = "The environment would be fine without coyotes",
  Q6_6_CoyFM = "Coyotes frighten me",
  Q6_7_CoyBeau = "Coyotes are a creature of beauty",
  Q6_8_CoyKO = "I actively keep coyotes out of my yard",
  
  # Cat Attitudes
  Q7_1_CatThB = "Cats are a threat to birds",
  Q7_2_CatRod = "Cats keep the rodent population down",
  Q7_3_CatIH = "Cats are important to humans",
  Q7_4_CatWO = "The environment would be fine without cats",
  Q7_5_CatFM = "Cats frighten me",
  Q7_6_CatBeau = "Cats are a creature of beauty",
  Q7_7_CatKO = "I actively keep cats out of my yard"
)


# Define the question groups
utilitarian_vars <- c("Q8_4_GenECat", "Q8_7_GenUY", "Q12_3_WUse", "Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")
biocentric_vars <- c("Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg", "Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")
stewardship_vars <- c("Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")
coyote_vars <- c("Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO")
cat_vars <- c("Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO")


# Function to calculate agreement percentages
calculate_agreement <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    summarise(across(everything(), ~ mean(.x %in% c(1, 2), na.rm = TRUE) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Percent_Agree")
}

# Calculate agreement for each attitude
utilitarian_agree <- calculate_agreement(full_data, utilitarian_vars)
biocentric_agree <- calculate_agreement(full_data, biocentric_vars)
stewardship_agree <- calculate_agreement(full_data, stewardship_vars)
coyote_agree <- calculate_agreement(full_data, coyote_vars)
cat_agree <- calculate_agreement(full_data, cat_vars)

# Bar chart function
plot_bar <- function(data, title) {
  ggplot(data, aes(x = reorder(question_labels[Question], -Percent_Agree), y = Percent_Agree)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(round(Percent_Agree, 1), "%")), 
              hjust = -0.05, size = 3.5) +  # adjusts text position and size
    coord_flip() +
    labs(title = title, x = "Question Statement", y = "% Agree (1 or 2)") +
    theme_minimal() +
    ylim(0, max(data$Percent_Agree, na.rm = TRUE) + 10)  # gives room for text
}

# Generate plots
plot_bar(utilitarian_agree, "Utilitarian Attitudes")
plot_bar(biocentric_agree, "Biocentric Attitudes")
plot_bar(stewardship_agree, "Stewardship Attitudes")
plot_bar(coyote_agree, "Coyote Attitudes")
plot_bar(cat_agree, "Cat Attitudes")



plot_pie <- function(data, title) {
  ggplot(data, aes(x = "", y = Percent_Agree, fill = question_labels[Question])) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Percent_Agree, 1), "%")),
              position = position_stack(vjust = 0.5), size = 3.5) +
    labs(title = title, fill = "Question") +
    theme_void() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

plot_pie(utilitarian_agree, "Utilitarian Attitudes")
plot_pie(biocentric_agree, "Biocentric Attitudes")
plot_pie(stewardship_agree, "Stewardship Attitudes")
plot_pie(coyote_agree, "Coyote Attitudes")
plot_pie(cat_agree, "Cat Attitudes")




experience_data$Group <- ifelse(grepl("^Q3_", experience_data$Question), "Coyote", "Cat")

experience_labels <- c(
  Q3_1_CoyNE = "I never encountered",
  Q3_2_CoyBE = "I briefly encountered",
  Q3_3_CoyWat = "I watched / observed",
  Q3_4_CoyFS = "I had food stolen",
  Q3_5_CatFed = "I fed / attracted",
  Q3_6_CoyPA = "My pet was attacked",
  
  Q4_1_CatNE = "I never encountered",
  Q4_2_CatBE = "I briefly encountered",
  Q4_3_CatWat = "I watched / observed",
  Q4_4_CatFS = "I had food stolen",
  Q4_5_CatFed = "I fed / attracted",
  Q4_6_CatTouch = "I touched the animal",
  Q4_7_CatPA = "My pet was attacked"
)


library(dplyr)
library(ggplot2)

# Original data example: you’ll replace this with your actual percentages
experience_grouped <- data.frame(
  Experience = rep(c("Never Encountered", "Briefly Encountered", "Watched and Observed",
                     "Food Stolen / Garden Eaten", "Fed or Attracted", "Pet Was Attacked", "Touched the Animal"), each = 2),
  Species = rep(c("Coyote", "Cat"), times = 7),
  Percent = c(35, 20, 25, 30, 18, 22, 8, 10, 6, 12, 5, 3, NA, 15)  # replace with actual values
)

# Remove NA if Touched the Animal doesn’t apply to Coyotes
experience_grouped <- experience_grouped %>% filter(!is.na(Percent))

# Plot: Grouped Bar Chart
ggplot(experience_grouped, aes(x = Experience, y = Percent, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Coyote" = "#E69F00", "Cat" = "#56B4E9")) +
  labs(title = "Comparison of Wildlife Encounters by Experience Type", 
       x = "", y = "Percent (%)", fill = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#### end ####


# Load necessary packages
library(dplyr)

# Load your dataset (skip this if you've already done it)
full_data <- read.csv("~/Desktop/Survey Chapter/Data/Survey Stats + Analysis/SurveyResults_AND_Demographics.csv")

# Replace special codes with NA
full_data[full_data == -99] <- NA
full_data[full_data == 99] <- NA

# Define agreement threshold (Likert ≥ 4 = Agree)
agree_threshold <- 4

# Create named list of questions with readable labels
attitude_questions <- list(
  "Wildlife is here for us to use" = "Q12_3_WUse",
  "We need to control wildlife" = "Q12_4_WControl",
  "Humans need to avoid wildlife" = "Q12_8_WAvoid",
  "Humans need to ignore wildlife" = "Q12_10_WIgnore",
  
  "Wildlife is something to be loved" = "Q12_1_WLove",
  "Humans need to embrace wildlife" = "Q12_5_WEmbrace",
  "Humans need to care for wildlife" = "Q12_9_WCare",
  "Wildlife should be admired" = "Q12_12_WAdmire",
  
  "We need to protect wildlife" = "Q12_2_WProtect",
  "We need to explore nature" = "Q12_6_WExplore",
  "Wildlife is something that needs to be studied" = "Q12_7_WStudy",
  "We need to better understand wildlife" = "Q12_11_WUnderst",
  "Wildlife should be respected" = "Q12_13_WRespect",
  
  "Coyotes are a threat to people" = "Q6_1_CoyTh",
  "The environment would be fine without coyotes" = "Q6_5_CoyEF",
  "Coyotes frighten me" = "Q6_6_CoyFM",
  "I actively keep coyotes out of my yard" = "Q6_8_CoyKO",
  "Coyotes are a threat to outdoor domestic cats" = "Q6_4_CoyThCat",
  "Coyotes are important to humans" = "Q6_3_CoyIE",
  "Coyotes are important to the environment" = "Q6_2_CoyIH",
  "Coyotes are a creature of beauty" = "Q6_7_CoyBeau",
  
  "Cats keep the rodent population down" = "Q7_2_CatRod",
  "Cats frighten me" = "Q7_5_CatFM",
  "I actively keep cats out of my yard" = "Q7_7_CatKO",
  "Cats are a threat to birds" = "Q7_1_CatThB",
  "Cats are important to humans" = "Q7_3_CatIH",
  "The environment would be fine without cats" = "Q7_4_CatWO",
  "Cats are a creature of beauty" = "Q7_6_CatBeau"
)

# Calculate percentage agreement for each question
agreement_results <- lapply(attitude_questions, function(q) {
  var <- full_data[[q]]
  agreed <- sum(var >= agree_threshold, na.rm = TRUE)
  total <- sum(!is.na(var))
  percent_agree <- round((agreed / total) * 100, 1)
  return(percent_agree)
})

# Create final table
agreement_table <- data.frame(
  Statement = names(agreement_results),
  Percent_Agree = unlist(agreement_results)
)

# View results
print(agreement_table)










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
full_data$neutral_score <- rowSums(full_data[neutral_vars], na.rm = TRUE)

# Now you have scores for each group (resistance, encouragement, and neutral)
# You can use these scores for analysis with attitude indexes


#### end ####


"Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash", "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other", "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", "Q10_9_Woodpile",, "Q10_10_Other", "Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other", "Q10_8_Wind", "Q10_9_Woodpile", "Q10_10_Other"



#### PCA ? ####

install.packages("factoextra")
library(dplyr)
library(factoextra)
library(ggplot2)

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


#### end ####








hist(full_data$Utilitarian_Index)
hist(full_data$Biocentric_Index)
hist(full_data$Stewardship_Index)
hist(full_data$Coyote_Index)
hist(full_data$Cat_Index)
hist(full_data$ExpTypeNeg_Index)
hist(full_data$ExpTypePos_Index)
hist(full_data$Q2_CatSH)
hist(full_data$Q1_CoySH)


addresses <- read.csv("~/Desktop/CommReport_Addresses.csv")
survey_data <- read.csv("~/Desktop/Survey Chapter/Data/Survey Stats + Analysis/SurveyResults_AND_Demographics.csv")
library(dplyr)

class(survey_data)
class(addresses)

survey_data$Passcode <- as.character(survey_data$Passcode)
addresses$Passcode <- as.character(addresses$Passcode)

library(dplyr)
library(tidyr)
library(readr)

# Load datasets
survey_data <- read_csv("SurveyResults_AND_Demographics.csv")
addresses <- read_csv("CommReport_Addresses.csv")

# Ensure matching types
survey_data$Passcode <- as.character(survey_data$Passcode)
addresses$Passcode <- as.character(addresses$Passcode)

# Join by Passcode
merged_data <- left_join(survey_data, addresses %>% select(Passcode, UWIN_ID), by = "Passcode")
install.packages("writexl")
library(writexl)
write_xlsx(merged_data, "Merged_Full_Data.xlsx")


# Your full list of variables
vars <- c(
  "Q1_CoySH", "Q2_CatSH",
  "Q3_1_CoyNE", "Q3_2_CoyBE", "Q3_3_CoyWat", "Q3_4_CoyFS", "Q3_5_CoyFed", "Q3_6_CoyPA", "Q3_7_CoyOther",
  "Q4_1_CatNE", "Q4_2_CatBE", "Q4_3_CatWat", "Q4_4_CatFS", "Q4_5_CatFed", "Q4_6_CatTouch", "Q4_7_CatPA", "Q4_8_CatOther",
  "Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO",
  "Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO",
  "Q8_1_GenN", "Q8_2_GenGS", "Q8_3_GenECoy", "Q8_4_GenECat", "Q8_5_GenVeg", "Q8_6_GenYard", "Q8_7_GenUY",
  "Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash",
  "Q9_9_Prune", "Q9_10_PestHerb", "Q9_11_CreateHab", "Q9_12_Other",
  "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", "Q10_5_Fruit", "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind",
  "Q10_9_Woodpile", "Q10_10_Other",
  "Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other",
  "Q12_1_WLove", "Q12_2_WProtect", "Q12_3_WUse", "Q12_4_WControl", "Q12_5_WEmbrace", "Q12_7_WStudy", "Q12_8_WAvoid",
  "Q12_9_WCare", "Q12_10_WIgnore", "Q12_11_WUnderst", "Q12_12_WAdmire", "Q12_13_WRespect"
)

# Step 1: Compute range per neighborhood
range_table <- merged_data %>%
  group_by(UWIN_ID) %>%
  summarise(across(all_of(vars), ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))) %>%
  ungroup()

# Step 2: Pivot to make variables the rows
range_long <- range_table %>%
  pivot_longer(-UWIN_ID, names_to = "Variable", values_to = "Range") %>%
  pivot_wider(names_from = UWIN_ID, values_from = Range)

# Step 3: View or export
print(range_long)


install.packages("writexl")
library(writexl)
write_xlsx(range_long, "Range_by_Neighborhood.xlsx")




