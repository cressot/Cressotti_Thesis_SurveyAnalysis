#Cronbach's Alpha 

install.packages("psych")
library(psych)
install.packages("dplyr")  # Run this only if dplyr is not installed
library(dplyr)


#### Version 2 ####
str(R_Survey_Attitudes_CronbachA)

Cronbach_Numeric <- R_Survey_Attitudes_CronbachA %>% mutate(across(everything(), as.numeric))  # Convert all columns to numeric
str(Cronbach_Numeric)
Cronbach_Numeric <- Cronbach_Numeric[, colSums(is.na(Cronbach_Numeric)) < nrow(Cronbach_Numeric)]
str(Cronbach_Numeric)


Cronbach_Numeric <- na.omit(Cronbach_Numeric) #temporarily remove rows with NA values from the dataset when performing calculations without deleting columns; 
#This will remove any row that contains NA values in any column for that operation, but the dataset will still contain all of the columns


# Utilitarian Index
alpha(Cronbach_Numeric[, c("Q6_1_CoyTh", "Q6_4_CoyThCat", "Q6_6_CoyFM", "Q7_2_CatRod", 
               "Q7_5_CatFM_R", "Q8_4_GenECat", "Q8_7_GenUY", "Q12_3", 
               "Q12_4", "Q12_8", "Q12_10")])
# Recommends reverse coding Q12_3 Q12_4 Q12_8 Q12_10
Cronbach_Numeric <- Cronbach_Numeric %>%
  mutate(across(c(Q12_3, Q12_4, Q12_8, Q12_10), ~ -1 * .))
summary(Cronbach_Numeric[, c("Q12_3", "Q12_4", "Q12_8", "Q12_10")])
#Run it again:
alpha(Cronbach_Numeric[, c("Q6_1_CoyTh", "Q6_4_CoyThCat", "Q6_6_CoyFM", "Q7_2_CatRod", 
                           "Q7_5_CatFM_R", "Q8_4_GenECat", "Q8_7_GenUY", "Q12_3", 
                           "Q12_4", "Q12_8", "Q12_10")])


# Biocentric Index
alpha(Cronbach_Numeric[, c("Q6_2_CoyIH", "Q6_5_CoyEF", "Q6_7_CoyBeau", "Q7_1_CatThB", 
               "Q7_3_CatIH", "Q7_6_CatBeau", "Q8_1_GenN", "Q8_2_GenGS", 
               "Q8_5_GenVeg", "Q12_1", "Q12_5", "Q12_9", "Q12_12")])
# Recommends reverse coding Q12_1 Q12_5 Q12_9 Q12_12
Cronbach_Numeric <- Cronbach_Numeric %>%
  mutate(across(c(Q12_1, Q12_5, Q12_9, Q12_12), ~ -1 * .))
summary(Cronbach_Numeric[, c("Q12_1", "Q12_5", "Q12_9", "Q12_12")])
#Run again
alpha(Cronbach_Numeric[, c("Q6_2_CoyIH", "Q6_5_CoyEF", "Q6_7_CoyBeau", "Q7_1_CatThB", 
                           "Q7_3_CatIH", "Q7_6_CatBeau", "Q8_1_GenN", "Q8_2_GenGS", 
                           "Q8_5_GenVeg", "Q12_1", "Q12_5", "Q12_9", "Q12_12")])


# Stewardship Index
alpha(Cronbach_Numeric[, c("Q6_3_CoyIE", "Q6_8_CoyKO", "Q7_4_CatWO", "Q7_7_CatKO", 
               "Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2", "Q12_6", 
               "Q12_7", "Q12_11", "Q12_13")])
# Recommends reverse coding Q6_3_CoyIE Q7_4_CatWO Q7_7_CatKO Q8_3_GenECoy Q8_6_GenYard
Cronbach_Numeric <- Cronbach_Numeric %>%
  mutate(across(c("Q6_3_CoyIE", "Q7_4_CatWO", "Q7_7_CatKO", "Q8_3_GenECoy", "Q8_6_GenYard"), ~ -1 * .))
#Run again
alpha(Cronbach_Numeric[, c("Q6_3_CoyIE", "Q6_8_CoyKO", "Q7_4_CatWO", "Q7_7_CatKO", 
                           "Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2", "Q12_6", 
                           "Q12_7", "Q12_11", "Q12_13")])



#Second try with different variables in the indexes:
Cronbach_Numeric_V2 <- R_Survey_Attitudes_CronbachA %>% mutate(across(everything(), as.numeric))  # Convert all columns to numeric
str(Cronbach_Numeric_V2)
Cronbach_Numeric_V2 <- Cronbach_Numeric_V2[, colSums(is.na(Cronbach_Numeric_V2)) < nrow(Cronbach_Numeric_V2)]
str(Cronbach_Numeric_V2)

Cronbach_Numeric_V2_clean <- na.omit(Cronbach_Numeric_V2) #temporarily remove rows with NA values from the dataset when performing calculations without deleting columns; 
#This will remove any row that contains NA values in any column for that operation, but the dataset will still contain all of the columns

colnames(Cronbach_Numeric_V2_clean)


# Utilitarian Index
alpha(Cronbach_Numeric_V2_clean[, c("Q6_1_CoyTh", "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_8_CoyKO", 
                           "Q7_4_CatWO", "Q7_5_CatFM_R", "Q7_7_CatKO", "Q8_4_GenECat", "Q8_7_GenUY", "Q12_3", 
                           "Q12_4", "Q12_8", "Q12_10")])
# Reverse code the selected items
Cronbach_Numeric_V2_clean <- Cronbach_Numeric_V2_clean %>%
  mutate(
    Q7_4_CatWO = 2 - Q7_4_CatWO,
    Q7_5_CatFM_R = 2 - Q7_5_CatFM_R,
    Q7_7_CatKO = 2 - Q7_7_CatKO,
    Q12_8 = 2 - Q12_8,
    Q12_10 = 2 - Q12_10
  )


# Biocentric Index
alpha(Cronbach_Numeric_V2_clean[, c("Q6_2_CoyIH", "Q6_3_CoyIE", "Q7_1_CatThB", "Q7_3_CatIH", 
                           "Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg", "Q12_1", "Q12_5", "Q12_9", "Q12_12")])
# Stewardship Index
alpha(Cronbach_Numeric_V2_clean[, c("Q6_4_CoyThCat", "Q6_7_CoyBeau", "Q7_2_CatRod", "Q7_6_CatBeau", 
                           "Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2", "Q12_6", 
                           "Q12_7", "Q12_11", "Q12_13")])
# Reverse code the selected items
Cronbach_Numeric_V2_clean <- Cronbach_Numeric_V2_clean %>%
  mutate(
    Q6_4_CoyThCat = 2 - Q6_4_CoyThCat,
  )
#### end ####


#### Version 3 ####

SurveyResults_AND_Demographics <- read.csv("~/Desktop/Survey Chapter/Data/Survey Stats + Analysis/SurveyResults_AND_Demographics.csv")

str(SurveyResults_AND_Demographics)
SurveyResults_AND_Demographics <- na.omit(SurveyResults_AND_Demographics) #temporarily remove rows with NA values from the dataset when performing calculations without deleting columns; 
#This will remove any row that contains NA values in any column for that operation, but the dataset will still contain all of the columns


# Coyote Index - Good
alpha(SurveyResults_AND_Demographics[, c("Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", "Q6_7_CoyBeau")])

# Coyote Index - Bad
alpha(SurveyResults_AND_Demographics[, c("Q6_1_CoyTh","Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_8_CoyKO")])

# Coyote Index - All
alpha(SurveyResults_AND_Demographics[, c("Q6_1_CoyTh", "Q6_2_CoyIH", "Q6_3_CoyIE", "Q6_4_CoyThCat", 
                           "Q6_5_CoyEF", "Q6_6_CoyFM", "Q6_7_CoyBeau", "Q6_8_CoyKO")])


# Cat Index - Good
alpha(SurveyResults_AND_Demographics[, c("Q7_2_CatRod", "Q7_3_CatIH", "Q7_6_CatBeau")])

# Cat Index - Bad
alpha(SurveyResults_AND_Demographics[, c("Q7_1_CatThB","Q7_4_CatWO", "Q7_5_CatFM", "Q7_7_CatKO")])

# Cat Index - All
alpha(SurveyResults_AND_Demographics[, c("Q7_1_CatThB", "Q7_2_CatRod", "Q7_3_CatIH", "Q7_4_CatWO", 
                           "Q7_5_CatFM", "Q7_6_CatBeau", "Q7_7_CatKO")])




#Attitude Index but Less varibles (removed based on face valu) 
colnames(SurveyResults_AND_Demographics)

#With Q8
# Utilitarian Index
alpha(SurveyResults_AND_Demographics[, c("Q8_4_GenECat", "Q8_7_GenUY", "Q12_3_WUse", "Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")])
#Reverse Q8_4_GenECat Q8_7_GenUY
SurveyResults_AND_Demographics <- SurveyResults_AND_Demographics %>%
  mutate(across(c(Q8_4_GenECat, Q8_7_GenUY), ~ -1 * .))
summary(SurveyResults_AND_Demographics[, c("Q8_4_GenECat", "Q8_7_GenUY")])
alpha(SurveyResults_AND_Demographics[, c("Q8_4_GenECat", "Q8_7_GenUY", "Q12_3_WUse", "Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")])


# Biocentric Index
alpha(SurveyResults_AND_Demographics[, c("Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg", "Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")])
#Reverse Q8_1_GenN Q8_2_GenGS Q8_5_GenVeg
SurveyResults_AND_Demographics <- SurveyResults_AND_Demographics %>%
  mutate(across(c(Q8_1_GenN, Q8_2_GenGS, Q8_5_GenVeg), ~ -1 * .))
summary(SurveyResults_AND_Demographics[, c("Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg")])
alpha(SurveyResults_AND_Demographics[, c("Q8_1_GenN", "Q8_2_GenGS", "Q8_5_GenVeg", "Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")])


# Stewardship Index
alpha(SurveyResults_AND_Demographics[, c("Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")])
#Reverse Q8_3_GenECoy Q8_6_GenYard
SurveyResults_AND_Demographics <- SurveyResults_AND_Demographics %>%
  mutate(across(c(Q8_3_GenECoy, Q8_6_GenYard), ~ -1 * .))
summary(SurveyResults_AND_Demographics[, c("Q8_3_GenECoy", "Q8_6_GenYard")])
alpha(SurveyResults_AND_Demographics[, c("Q8_3_GenECoy", "Q8_6_GenYard", "Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")])



#Without Q8
# Utilitarian Index
alpha(SurveyResults_AND_Demographics[, c("Q12_3_WUse", "Q12_4_WControl", "Q12_8_WAvoid", "Q12_10_WIgnore")])

# Biocentric Index
alpha(SurveyResults_AND_Demographics[, c("Q12_1_WLove", "Q12_5_WEmbrace", "Q12_9_WCare", "Q12_12_WAdmire")])

# Stewardship Index
alpha(SurveyResults_AND_Demographics[, c("Q12_2_WProtect", "Q12_6_WExplore", "Q12_7_WStudy", "Q12_11_WUnderst", "Q12_13_WRespect")])

#### end ####