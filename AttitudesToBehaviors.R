library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)

SurveyResults_Cleaned<-SurveyResults_AND_Demographics
data<-SurveyResults_Cleaned
summary(data)

data_filtered <- data %>% filter(!if_any(everything(), ~ . == -99))

#### Stat test ####

q11_vars <- c("Q11_1_Grass", "Q11_2_Hardscape", "Q11_3_Veg", "Q11_4_Mulch", "Q11_5_Shrub", "Q11_6_Tree", "Q11_7_Other")
attitude_vars <- c("attitude_Bio", "attitude_Stew", "attitude_Util")

q9q10_vars <- c("Q9_1_CatOut", "Q9_2_TreeVeg", "Q9_3_Exter", "Q9_4_Trap", "Q9_5_Lights", 
                "Q9_6_PetFood", "Q9_7_DogOff", "Q9_8_LockTrash", "Q9_9_Prune", "Q9_10_PestHerb",
                "Q9_11_CreateHab", "Q9_12_Other",
                "Q10_1_Bbath", "Q10_2_Bfeed", "Q10_3_Fence", "Q10_4_Flower", "Q10_5_Fruit", 
                "Q10_6_Water", "Q10_7_Veg", "Q10_8_Wind", "Q10_9_Woodpile", "Q10_10_Other")

# Logistic regression for each attitude separately
bio_model <- glm(attitude_Bio ~ ., data = data[, c("attitude_Bio", q9q10_vars)])
stew_model <- glm(attitude_Stew ~ ., data = data[, c("attitude_Stew", q9q10_vars)])
util_model <- glm(attitude_Util ~ ., data = data[, c("attitude_Util", q9q10_vars)])

summary(bio_model)
summary(stew_model)
summary(util_model)


data$attitude_Bio_cat <- cut(data$attitude_Bio, breaks=3, labels=c("Low", "Medium", "High"))
anova_model <- aov(attitude_Bio_cat ~ ., data = data[, c("attitude_Bio_cat", q9q10_vars)])
data$attitude_Bio_num <- as.numeric(data$attitude_Bio_cat)  # Converts "Low" = 1, "Medium" = 2, "High" = 3

anova_model <- aov(attitude_Bio_num ~ ., data = data[, c("attitude_Bio_num", q9q10_vars)])
summary(anova_model)
#### end ####

#### Visualizing ####

cor_results_attitudes <- cor(data[, q11_vars], data[, attitude_vars], use = "pairwise.complete.obs", method = "spearman")  # Use "pearson" if linear
print(cor_results_attitudes)
corrplot(cor_results_attitudes, method = "square")
corrplot(cor_results_attitudes, method = "number")


data_filtered <- data %>% filter(!if_any(everything(), ~ . == -99))
B1 <- ggplot(data_filtered, aes(x = Q11_1_Grass, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Grass Percentage", x = "Grass Percentage", y = "Bio Attitude Score")
B2 <- ggplot(data_filtered, aes(x = Q11_2_Hardscape, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Hardscape Percentage", x = "Hardscape Percentage", y = "Bio Attitude Score")
B3 <- ggplot(data_filtered, aes(x = Q11_3_Veg, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Veg Garden Percentage", x = "Veg Garden Percentage", y = "Bio Attitude Score")
B4 <- ggplot(data_filtered, aes(x = Q11_4_Mulch, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Mulch Percentage", x = "Mulch Percentage", y = "Bio Attitude Score")
B5 <- ggplot(data_filtered, aes(x = Q11_5_Shrub, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs <10ft Percentage", x = " <10ft Percentage", y = "Bio Attitude Score")
B6 <- ggplot(data_filtered, aes(x = Q11_6_Tree, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs  >10ft Percentage", x = " >10ft Percentage", y = "Bio Attitude Score")
B7 <- ggplot(data_filtered, aes(x = Q11_7_Other, y = attitude_Bio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Other Percentage", x = "Other Percentage", y = "Bio Attitude Score")
grid.arrange(B1, B2, B3, B4, B5, B6, B7, ncol=3, nrow=3)

S1 <- ggplot(data_filtered, aes(x = Q11_1_Grass, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Grass Percentage", x = "Grass Percentage", y = "Stew Attitude Score")
S2 <- ggplot(data_filtered, aes(x = Q11_2_Hardscape, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Hardscape Percentage", x = "Hardscape Percentage", y = "Stew Attitude Score")
S3 <- ggplot(data_filtered, aes(x = Q11_3_Veg, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Veg Garden Percentage", x = "Veg Garden Percentage", y = "Stew Attitude Score")
S4 <- ggplot(data_filtered, aes(x = Q11_4_Mulch, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Mulch Percentage", x = "Mulch Percentage", y = "Stew Attitude Score")
S5 <- ggplot(data_filtered, aes(x = Q11_5_Shrub, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs <10ft Percentage", x = "<10ft Percentage", y = "Stew Attitude Score")
S6 <- ggplot(data_filtered, aes(x = Q11_6_Tree, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs >10ft Percentage", x = ">10ft Percentage", y = "Stew Attitude Score")
S7 <- ggplot(data_filtered, aes(x = Q11_7_Other, y = attitude_Stew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Other Percentage", x = "Other Percentage", y = "Stew Attitude Score")
grid.arrange(S1, S2, S3, S4, S5, S6, S7, ncol=3, nrow=3)

U1 <- ggplot(data_filtered, aes(x = Q11_1_Grass, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Grass Percentage", x = "Grass Percentage", y = "Util Attitude Score")
U2 <- ggplot(data_filtered, aes(x = Q11_2_Hardscape, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Hardscape Percentage", x = "Hardscape Percentage", y = "Util Attitude Score")
U3 <- ggplot(data_filtered, aes(x = Q11_3_Veg, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Veg Garden Percentage", x = "Veg Garden Percentage", y = "Util Attitude Score")
U4 <- ggplot(data_filtered, aes(x = Q11_4_Mulch, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Mulch Percentage", x = "Mulch Percentage", y = "Util Attitude Score")
U5 <- ggplot(data_filtered, aes(x = Q11_5_Shrub, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs <10ft Percentage", x = "<10ft Percentage", y = "Util Attitude Score")
U6 <- ggplot(data_filtered, aes(x = Q11_6_Tree, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs >10ft Percentage", x = ">10ft Percentage", y = "Util Attitude Score")
U7 <- ggplot(data_filtered, aes(x = Q11_7_Other, y = attitude_Util)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Attitude vs Other Percentage", x = "Other Percentage", y = "Util Attitude Score")
grid.arrange(U1, U2, U3, U4, U5, U6, U7, ncol=3, nrow=3)



B9.1 <- ggplot(data_filtered, aes(x = factor(Q9_1_CatOut), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Allowing Cats Outdoors", x = "Allowed Cats Outdoors", y = "Bio Attitude Score")
B9.2 <- ggplot(data_filtered, aes(x = factor(Q9_2_TreeVeg), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Cleared Tree/Veg", x = "Cleared Tree/Veg", y = "Bio Attitude Score")
B9.3 <- ggplot(data_filtered, aes(x = factor(Q9_3_Exter), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Exterminated", x = "Exterminated", y = "Bio Attitude Score")
B9.4 <- ggplot(data_filtered, aes(x = factor(Q9_4_Trap), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Trapped", x = "Trapped", y = "Bio Attitude Score")
B9.5 <- ggplot(data_filtered, aes(x = factor(Q9_5_Lights), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Lights at Night", x = "Lights at Night", y = "Bio Attitude Score")
B9.6 <- ggplot(data_filtered, aes(x = factor(Q9_6_PetFood), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pet Food Outside", x = "Pet Food Outside", y = "Bio Attitude Score")
B9.7 <- ggplot(data_filtered, aes(x = factor(Q9_7_DogOff), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Dogs Offleash", x = "Dogs Offleash", y = "Bio Attitude Score")
B9.8 <- ggplot(data_filtered, aes(x = factor(Q9_8_LockTrash), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Locked Trash", x = "Locked Trash", y = "Bio Attitude Score")
B9.9 <- ggplot(data_filtered, aes(x = factor(Q9_9_Prune), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pruned", x = "Pruned", y = "Bio Attitude Score")
B9.10 <- ggplot(data_filtered, aes(x = factor(Q9_10_PestHerb), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Applied Pest/Herbicide", x = "Applied Pest/Herbicide", y = "Bio Attitude Score")
B9.11 <- ggplot(data_filtered, aes(x = factor(Q9_11_CreateHab), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Created Habitat", x = "Created Habitat", y = "Bio Attitude Score")
B9.12 <- ggplot(data_filtered, aes(x = factor(Q9_12_Other), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Activity", x = "Other Activity", y = "Bio Attitude Score")
B10.1 <- ggplot(data_filtered, aes(x = factor(Q10_1_Bbath), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Bath", x = "Bird Bath", y = "Bio Attitude Score")
B10.2 <- ggplot(data_filtered, aes(x = factor(Q10_2_Bfeed), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Feeder", x = "Bird Feeder", y = "Bio Attitude Score")
B10.3 <- ggplot(data_filtered, aes(x = factor(Q10_3_Fence), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fence", x = "Fence", y = "Bio Attitude Score")
B10.4 <- ggplot(data_filtered, aes(x = factor(Q10_4_Flower), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Flower Garden", x = "Flower Garden", y = "Bio Attitude Score")
B10.5 <- ggplot(data_filtered, aes(x = factor(Q10_5_Fruit), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fruit Garden", x = "Fruit Garden", y = "Bio Attitude Score")
B10.6 <- ggplot(data_filtered, aes(x = factor(Q10_6_Water), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pool, Pond, Etc.", x = "Pool, Pond, Etc.", y = "Bio Attitude Score")
B10.7 <- ggplot(data_filtered, aes(x = factor(Q10_7_Veg), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Veg Garden", x = "Veg Garden", y = "Bio Attitude Score")
B10.8 <- ggplot(data_filtered, aes(x = factor(Q10_8_Wind), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Windchimes", x = "Windchimes", y = "Bio Attitude Score")
B10.9 <- ggplot(data_filtered, aes(x = factor(Q10_9_Woodpile), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Woodpiles", x = "Woodpiles", y = "Bio Attitude Score")
B10.10 <- ggplot(data_filtered, aes(x = factor(Q10_10_Other), y = attitude_Bio)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Item", x = "Other Item", y = "Bio Attitude Score")
grid.arrange(B9.1, B9.2, B9.3, B9.4, B9.5, B9.6, B9.7, B9.8, B9.9, B9.10, B9.11, B9.12, B10.1, B10.2, B10.3, B10.4, B10.5, B10.6, B10.7, B10.8, B10.9, B10.10, ncol=5, nrow=5)



S9.1 <- ggplot(data_filtered, aes(x = factor(Q9_1_CatOut), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Allowing Cats Outdoors", x = "Allowed Cats Outdoors", y = "Stew Attitude Score")
S9.2 <- ggplot(data_filtered, aes(x = factor(Q9_2_TreeVeg), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Cleared Tree/Veg", x = "Cleared Tree/Veg", y = "Stew Attitude Score")
S9.3 <- ggplot(data_filtered, aes(x = factor(Q9_3_Exter), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Exterminated", x = "Exterminated", y = "Stew Attitude Score")
S9.4 <- ggplot(data_filtered, aes(x = factor(Q9_4_Trap), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Trapped", x = "Trapped", y = "Stew Attitude Score")
S9.5 <- ggplot(data_filtered, aes(x = factor(Q9_5_Lights), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Lights at Night", x = "Lights at Night", y = "Stew Attitude Score")
S9.6 <- ggplot(data_filtered, aes(x = factor(Q9_6_PetFood), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pet Food Outside", x = "Pet Food Outside", y = "Stew Attitude Score")
S9.7 <- ggplot(data_filtered, aes(x = factor(Q9_7_DogOff), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Dogs Offleash", x = "Dogs Offleash", y = "Stew Attitude Score")
S9.8 <- ggplot(data_filtered, aes(x = factor(Q9_8_LockTrash), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Locked Trash", x = "Locked Trash", y = "Stew Attitude Score")
S9.9 <- ggplot(data_filtered, aes(x = factor(Q9_9_Prune), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pruned", x = "Pruned", y = "Stew Attitude Score")
S9.10 <- ggplot(data_filtered, aes(x = factor(Q9_10_PestHerb), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Applied Pest/Herbicide", x = "Applied Pest/Herbicide", y = "Stew Attitude Score")
S9.11 <- ggplot(data_filtered, aes(x = factor(Q9_11_CreateHab), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Created Habitat", x = "Created Habitat", y = "Stew Attitude Score")
S9.12 <- ggplot(data_filtered, aes(x = factor(Q9_12_Other), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Activity", x = "Other Activity", y = "Stew Attitude Score")
S10.1 <- ggplot(data_filtered, aes(x = factor(Q10_1_Bbath), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Bath", x = "Bird Bath", y = "Stew Attitude Score")
S10.2 <- ggplot(data_filtered, aes(x = factor(Q10_2_Bfeed), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Feeder", x = "Bird Feeder", y = "Stew Attitude Score")
S10.3 <- ggplot(data_filtered, aes(x = factor(Q10_3_Fence), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fence", x = "Fence", y = "Stew Attitude Score")
S10.4 <- ggplot(data_filtered, aes(x = factor(Q10_4_Flower), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Flower Garden", x = "Flower Garden", y = "Stew Attitude Score")
S10.5 <- ggplot(data_filtered, aes(x = factor(Q10_5_Fruit), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fruit Garden", x = "Fruit Garden", y = "Stew Attitude Score")
S10.6 <- ggplot(data_filtered, aes(x = factor(Q10_6_Water), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pool, Pond, Etc.", x = "Pool, Pond, Etc.", y = "Stew Attitude Score")
S10.7 <- ggplot(data_filtered, aes(x = factor(Q10_7_Veg), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Veg Garden", x = "Veg Garden", y = "Stew Attitude Score")
S10.8 <- ggplot(data_filtered, aes(x = factor(Q10_8_Wind), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Windchimes", x = "Windchimes", y = "Stew Attitude Score")
S10.9 <- ggplot(data_filtered, aes(x = factor(Q10_9_Woodpile), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Woodpiles", x = "Woodpiles", y = "Stew Attitude Score")
S10.10 <- ggplot(data_filtered, aes(x = factor(Q10_10_Other), y = attitude_Stew)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Item", x = "Other Item", y = "Stew Attitude Score")
grid.arrange(S9.1, S9.2, S9.3, S9.4, S9.5, S9.6, S9.7, S9.8, S9.9, S9.10, S9.11, S9.12, S10.1, S10.2, S10.3, S10.4, S10.5, S10.6, S10.7, S10.8, S10.9, S10.10, ncol=5, nrow=5)



U9.1 <- ggplot(data_filtered, aes(x = factor(Q9_1_CatOut), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Allowing Cats Outdoors", x = "Allowed Cats Outdoors", y = "Util Attitude Score")
U9.2 <- ggplot(data_filtered, aes(x = factor(Q9_2_TreeVeg), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Cleared Tree/Veg", x = "Cleared Tree/Veg", y = "Util Attitude Score")
U9.3 <- ggplot(data_filtered, aes(x = factor(Q9_3_Exter), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Exterminated", x = "Exterminated", y = "Util Attitude Score")
U9.4 <- ggplot(data_filtered, aes(x = factor(Q9_4_Trap), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Trapped", x = "Trapped", y = "Util Attitude Score")
U9.5 <- ggplot(data_filtered, aes(x = factor(Q9_5_Lights), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Lights at Night", x = "Lights at Night", y = "Util Attitude Score")
U9.6 <- ggplot(data_filtered, aes(x = factor(Q9_6_PetFood), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pet Food Outside", x = "Pet Food Outside", y = "Util Attitude Score")
U9.7 <- ggplot(data_filtered, aes(x = factor(Q9_7_DogOff), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Dogs Offleash", x = "Dogs Offleash", y = "Util Attitude Score")
U9.8 <- ggplot(data_filtered, aes(x = factor(Q9_8_LockTrash), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Locked Trash", x = "Locked Trash", y = "Util Attitude Score")
U9.9 <- ggplot(data_filtered, aes(x = factor(Q9_9_Prune), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pruned", x = "Pruned", y = "Util Attitude Score")
U9.10 <- ggplot(data_filtered, aes(x = factor(Q9_10_PestHerb), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Applied Pest/Herbicide", x = "Applied Pest/Herbicide", y = "Util Attitude Score")
U9.11 <- ggplot(data_filtered, aes(x = factor(Q9_11_CreateHab), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Created Habitat", x = "Created Habitat", y = "Util Attitude Score")
U9.12 <- ggplot(data_filtered, aes(x = factor(Q9_12_Other), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Activity", x = "Other Activity", y = "Util Attitude Score")
U10.1 <- ggplot(data_filtered, aes(x = factor(Q10_1_Bbath), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Bath", x = "Bird Bath", y = "Util Attitude Score")
U10.2 <- ggplot(data_filtered, aes(x = factor(Q10_2_Bfeed), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Bird Feeder", x = "Bird Feeder", y = "Util Attitude Score")
U10.3 <- ggplot(data_filtered, aes(x = factor(Q10_3_Fence), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fence", x = "Fence", y = "Util Attitude Score")
U10.4 <- ggplot(data_filtered, aes(x = factor(Q10_4_Flower), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Flower Garden", x = "Flower Garden", y = "Util Attitude Score")
U10.5 <- ggplot(data_filtered, aes(x = factor(Q10_5_Fruit), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Fruit Garden", x = "Fruit Garden", y = "Util Attitude Score")
U10.6 <- ggplot(data_filtered, aes(x = factor(Q10_6_Water), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Pool, Pond, Etc.", x = "Pool, Pond, Etc.", y = "Util Attitude Score")
U10.7 <- ggplot(data_filtered, aes(x = factor(Q10_7_Veg), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Veg Garden", x = "Veg Garden", y = "Util Attitude Score")
U10.8 <- ggplot(data_filtered, aes(x = factor(Q10_8_Wind), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Windchimes", x = "Windchimes", y = "Util Attitude Score")
U10.9 <- ggplot(data_filtered, aes(x = factor(Q10_9_Woodpile), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Woodpiles", x = "Woodpiles", y = "Util Attitude Score")
U10.10 <- ggplot(data_filtered, aes(x = factor(Q10_10_Other), y = attitude_Util)) +
  geom_boxplot() +
  labs(title = "Attitude vs Other Item", x = "Other Item", y = "Util Attitude Score")
grid.arrange(U9.1, U9.2, U9.3, U9.4, U9.5, U9.6, U9.7, U9.8, U9.9, U9.10, U9.11, U9.12, U10.1, U10.2, U10.3, U10.4, U10.5, U10.6, U10.7, U10.8, U10.9, U10.10, ncol=5, nrow=5)

#### end ####



#### Checking BASE Data Normality + Distributions ####

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)


base_data <- SurveyResults_AND_Demographics 
base_data[base_data == -99] <- NA

# Identify variable types
num_vars <- sapply(base_data, is.numeric)  # Numeric variables
cat_vars <- sapply(base_data, is.factor)   # Categorical variables

# 1. Summary Statistics
print("Summary Statistics for Numerical Variables:")
summary(base_data[, num_vars])

print("Summary Statistics for Categorical Variables:")
summary(base_data[, cat_vars])

# 2. Normality Test (Shapiro-Wilk) for Numerical Variables
shapiro_results <- sapply(base_data[, num_vars], function(x) {
  if (length(unique(x)) > 3) { # Shapiro test requires at least 4 unique values
    shapiro.test(x)$p.value
  } else {
    NA
  }
})
shapiro_results <- data.frame(Variable = names(shapiro_results), P_Value = shapiro_results)

print("Shapiro-Wilk Normality Test Results:")
print(shapiro_results)

# 3. Visualizing Distributions (Histograms & QQ plots)

par(mfrow = c(2,2)) # 2x2 plotting grid

for (var in names(base_data[, num_vars])) {
  hist(base_data[[var]], main = paste("Histogram of", var), xlab = var, col = "lightblue", border = "black")
  qqnorm(base_data[[var]], main = paste("QQ Plot of", var))
  qqline(base_data[[var]], col = "red")
}
par(mfrow = c(1,1)) # Reset plotting

# 4. Binomial Proportion Test for Binary Variables
binary_vars <- names(base_data[, cat_vars])  # Select categorical variables

binom_test_results <- sapply(binary_vars, function(var) {
  if (length(unique(base_data[[var]])) == 2) {  # Only for binary variables
    binom.test(table(base_data[[var]]), p = 0.5)$p.value  # Assuming 50% expected probability
  } else {
    NA
  }
})
binom_test_results <- data.frame(Variable = names(binom_test_results), P_Value = binom_test_results)

print("Binomial Proportion Test Results (Checking Expected 50% Proportion):")
print(binom_test_results)

#### end ####