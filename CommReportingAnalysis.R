

UWIN_CoyBirdCat <- read.csv("~/Desktop/UWIN_CoyBirdCat.csv")
PUCP_Data <- read.csv("~/Desktop/PUCP_Data.csv")
CommReport_Addresses <- read.csv("~/Desktop/CommReport_Addresses.csv")
CommReport_SurveyReponses <- read.csv("~/Desktop/CommReport_SurveyReponses.csv")

#### Joining Addresses and Survey Responses ####
library(dplyr)

# Ensure Passcode is character in both datasets
CommReport_Addresses <- CommReport_Addresses %>%
  mutate(Passcode = as.character(Passcode))

CommReport_SurveyReponses <- CommReport_SurveyReponses %>%
  mutate(Passcode = as.character(Passcode))

# Now perform the join
Survey_Data <- CommReport_Addresses %>%
  inner_join(CommReport_SurveyReponses, by = "Passcode")
head(Survey_Data)

Survey_Data[Survey_Data == -99] <- NA
#### end ####


#### Cleaning PUCP Dataset ####
library(dplyr)
library(lubridate)

#Extracting Years 2020 - 2023 from PUCP
PUCP_Data$Date <- mdy(PUCP_Data$Date)  # Convert date column to Date format if needed
PUCP_Data$Year <- year(PUCP_Data$Date) # Extract year
PUCP_filtered <- PUCP_Data %>%
  filter(Year >= 2020 & Year <= 2023)
table(PUCP_filtered$year)  # Shows count of observations per year
head(PUCP_filtered)        # View first few rows


# Reducing # rows in PUCP to be w/in 5 miles of lat/long in Survey
library(sf)
library(dplyr)

sum(is.na(Survey_agg$Latitude))  # Count missing Latitudes
sum(is.na(Survey_agg$Longitude)) # Count missing Longitudes
Survey_agg <- Survey_agg[!is.na(Survey_agg$Latitude) & !is.na(Survey_agg$Longitude), ]

sum(is.na(PUCP_agg$Latitude)) 
sum(is.na(PUCP_agg$Longitude))
str(PUCP_agg$Latitude)
str(PUCP_agg$Longitude)
PUCP_agg <- PUCP_agg %>%
  mutate(Latitude = as.numeric(gsub("[^0-9.-]", "", Latitude)),
         Longitude = as.numeric(gsub("[^0-9.-]", "", Longitude)))
PUCP_agg <- PUCP_agg[!is.na(PUCP_agg$Latitude) & !is.na(PUCP_agg$Longitude), ]


Survey_sf <- st_as_sf(Survey_agg, coords = c("Longitude", "Latitude"), crs = 4326) # Convert Survey_agg to sf object
Survey_buffer <- st_buffer(Survey_sf, dist = units::set_units(804.67, "m"))   # <-- updated 0.5-mile buffer

PUCP_sf <- st_as_sf(PUCP_agg, coords = c("Longitude", "Latitude"), crs = 4326) # Convert PUCP_agg to sf object
PUCP_filtered <- PUCP_agg[rowSums(st_within(PUCP_sf, Survey_buffer, sparse = FALSE)) > 0, ] # Filter PUCP_agg to keep only points within 0.5 miles of Survey_sf

write.csv(PUCP_filtered, "PUCP_agg_Filtered.csv", row.names = FALSE)

#### end ####


#### Aggregating and Merging ####

#Summing Totals for UWIN data
UWIN_CoyBirdCat <- UWIN_CoyBirdCat %>%
  rowwise() %>%  # Ensures summation is row-based
  mutate(Total_Detections = sum(c_across(starts_with("Day_")), na.rm = TRUE)) %>%
  ungroup()

# Since Q1_CoySH (coyote sightings) and Q2_CatSH (cat sightings) are ordered frequency values, you can average them per location
Survey_agg <- Survey_Data %>%
  group_by(latitude.x, longitude.x) %>%
  summarise(mean_coyote_sightings = mean(Q1_CoySH, na.rm = TRUE),
            count_responses = n())  # Number of responses per location
Survey_agg <- Survey_agg %>%
  rename(Latitude = latitude.x, Longitude = longitude.x)

#Since Q1_CoySH (coyote sightings) and Q2_CatSH (cat sightings) are ordered frequency values, you can average them per location
PUCP_agg <- PUCP_filtered %>%
  group_by(Latitude, Longitude) %>%
  summarise(total_coyote_reports = n())

#Since this dataset is binary per 40-day season, we count how many seasons had at least one detection per location
UWIN_agg <- UWIN_CoyBirdCat %>%
  filter(Species == "Coyote") %>%  # Keep only Coyote rows
  group_by(Latitude, Longitude) %>%
  summarise(
    seasons_with_coyote = sum(Total_Detections > 0, na.rm = TRUE),  # Count seasons with ≥1 detection
    total_seasons = n(),  # Total seasons sampled
    detection_rate = seasons_with_coyote / total_seasons  # Proportion of seasons with detections
  )

write.csv(Survey_agg, "Survey_agg.csv", row.names = FALSE)
write.csv(PUCP_agg, "PUCP_agg.csv", row.names = FALSE)
write.csv(UWIN_agg, "UWIN_agg.csv", row.names = FALSE)
getwd()

#### end ####

#### Coyotes 0.5 Version 2 ####
library(dplyr)
library(sf)
library(units)
library(stats)
survey <- read.csv("Survey_agg.csv")
pucp <- read.csv("PUCP_agg.csv")
uwin <- read.csv("UWIN_agg.csv")

# Remove rows with missing coordinates BEFORE converting to sf
survey <- survey %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Convert to sf objects
survey_sf <- st_as_sf(survey, coords = c("Longitude", "Latitude"), crs = 4326)
pucp_sf <- st_as_sf(pucp, coords = c("Longitude", "Latitude"), crs = 4326)
uwin_sf <- st_as_sf(uwin, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract original coordinates from UWIN points
uwin_coords <- uwin_sf %>%
  mutate(Lat = st_coordinates(.)[, "Y"],
         Lon = st_coordinates(.)[, "X"])

# Buffer UWIN camera sites by 0.5 miles (~804.67 meters)
uwin_buffer <- st_buffer(uwin_coords, dist = set_units(804.67, "m"))

# Spatial joins: Find PUCP and Survey data within 0.5 miles of UWIN points
pucp_join <- st_join(uwin_buffer, pucp_sf, join = st_is_within_distance, dist = 804.67) %>%
  st_drop_geometry() %>%
  group_by(Lat, Lon) %>%
  summarise(PUCP_coyotes_per_ha_per_yr = mean(total_coyote_reports, na.rm = TRUE))

survey_join <- st_join(uwin_buffer, survey_sf, join = st_is_within_distance, dist = 804.67) %>%
  st_drop_geometry() %>%
  group_by(Lat, Lon) %>%
  summarise(Survey_coyotes_per_ha_per_yr = mean(mean_coyote_sightings, na.rm = TRUE))

# Prepare UWIN values with coordinates
uwin_summary <- dplyr::select(uwin_coords, Lat, Lon, UWIN_coyotes_per_ha_per_yr = detection_rate)

# Merge all datasets by coordinates
combined <- uwin_summary %>%
  left_join(pucp_join, by = c("Lat", "Lon")) %>%
  left_join(survey_join, by = c("Lat", "Lon")) %>%
  na.omit()  # Drop rows with any missing values


# First, drop geometry column if it exists
combined_clean <- combined %>% 
  st_drop_geometry() %>% 
  mutate(across(everything(), as.numeric))

# Now run correlation tests
cor_pearson <- cor(combined_clean[, 3:5], method = "pearson")
cor_spearman <- cor(combined_clean[, 3:5], method = "spearman")
cor_kendall <- cor(combined_clean[, 3:5], method = "kendall")

# Run Welch t-tests (two-sample tests assuming unequal variance)
t_uwin_vs_pucp <- t.test(combined_clean$UWIN_coyotes_per_ha_per_yr, combined_clean$PUCP_coyotes_per_ha_per_yr)
t_uwin_vs_survey <- t.test(combined_clean$UWIN_coyotes_per_ha_per_yr, combined_clean$Survey_coyotes_per_ha_per_yr)
t_pucp_vs_survey <- t.test(combined_clean$PUCP_coyotes_per_ha_per_yr, combined_clean$Survey_coyotes_per_ha_per_yr)

# Print results
cat("\n--- Pearson Correlation ---\n")
print(cor_pearson)

cat("\n--- Spearman Correlation ---\n")
print(cor_spearman)

cat("\n--- Kendall Correlation ---\n")
print(cor_kendall)

cat("\n--- Welch T-Test: UWIN vs PUCP ---\n")
print(t_uwin_vs_pucp)

cat("\n--- Welch T-Test: UWIN vs Survey ---\n")
print(t_uwin_vs_survey)

cat("\n--- Welch T-Test: PUCP vs Survey ---\n")
print(t_pucp_vs_survey)
#### end ####

#### Coyote 1mile Version 2 ####
library(dplyr)
library(sf)
library(units)
library(stats)
survey <- read.csv("Survey_agg.csv")
pucp <- read.csv("PUCP_agg.csv")
uwin <- read.csv("UWIN_agg.csv")

# Convert to sf objects
survey_sf <- st_as_sf(survey, coords = c("Longitude", "Latitude"), crs = 4326)
pucp_sf <- st_as_sf(pucp, coords = c("Longitude", "Latitude"), crs = 4326)
uwin_sf <- st_as_sf(uwin, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract original coordinates from UWIN points
uwin_coords <- uwin_sf %>%
  mutate(Lat = st_coordinates(.)[, "Y"],
         Lon = st_coordinates(.)[, "X"])

# Buffer UWIN camera sites by 0.5 miles (~804.67 meters)
uwin_buffer_1mile <- st_buffer(uwin_coords, dist = set_units(1609.34, "m"))

# Spatial joins: Find PUCP and Survey data within 0.5 miles of UWIN points
pucp_join_1mile <- st_join(uwin_buffer_1mile, pucp_sf, join = st_is_within_distance, dist = 1609.34) %>%
  st_drop_geometry() %>%
  group_by(Lat, Lon) %>%
  summarise(PUCP_coyotes_per_ha_per_yr = mean(total_coyote_reports, na.rm = TRUE))

survey_join_1mile <- st_join(uwin_buffer, survey_sf, join = st_is_within_distance, dist = 1609.34) %>%
  st_drop_geometry() %>%
  group_by(Lat, Lon) %>%
  summarise(Survey_coyotes_per_ha_per_yr = mean(mean_coyote_sightings, na.rm = TRUE))

# Prepare UWIN values with coordinates
uwin_summary_1mile <- dplyr::select(uwin_coords, Lat, Lon, UWIN_coyotes_per_ha_per_yr = detection_rate)

# Merge all datasets by coordinates
combined_1mile <- uwin_summary %>%
  left_join(pucp_join, by = c("Lat", "Lon")) %>%
  left_join(survey_join, by = c("Lat", "Lon")) %>%
  na.omit()  # Drop rows with any missing values


# First, drop geometry column if it exists
combined_clean <- combined %>% 
  st_drop_geometry() %>% 
  mutate(across(everything(), as.numeric))

# Run correlation tests
cor_pearson_1mile <- cor(combined_clean[, 3:5], method = "pearson")
cor_spearman_1mile <- cor(combined_clean[, 3:5], method = "spearman")
cor_kendall_1mile <- cor(combined_clean[, 3:5], method = "kendall")

# Run Welch t-tests (two-sample tests assuming unequal variance)
t_uwin_vs_pucp_1mile <- t.test(combined_clean$UWIN_coyotes_per_ha_per_yr, combined_clean$PUCP_coyotes_per_ha_per_yr)
t_uwin_vs_survey_1mile <- t.test(combined_clean$UWIN_coyotes_per_ha_per_yr, combined_clean$Survey_coyotes_per_ha_per_yr)
t_pucp_vs_survey_1mile <- t.test(combined_clean$PUCP_coyotes_per_ha_per_yr, combined_clean$Survey_coyotes_per_ha_per_yr)

# Print results
cat("\n--- Pearson Correlation ---\n")
print(cor_pearson_1mile)

cat("\n--- Spearman Correlation ---\n")
print(cor_spearman_1mile)

cat("\n--- Kendall Correlation ---\n")
print(cor_kendall_1mile)

cat("\n--- Welch T-Test: UWIN vs PUCP ---\n")
print(t_uwin_vs_pucp_1mile)

cat("\n--- Welch T-Test: UWIN vs Survey ---\n")
print(t_uwin_vs_survey_1mile)

cat("\n--- Welch T-Test: PUCP vs Survey ---\n")
print(t_pucp_vs_survey_1mile)

#### end ####


#### Cats 0.5 mile Version 2 ####
library(dplyr)
library(sf)
library(units)
library(readr)

#### Load Data
UWIN_CoyBirdCat <- read_csv("~/Desktop/UWIN_CoyBirdCat.csv")
CommReport_Addresses <- read_csv("~/Desktop/CommReport_Addresses.csv")
CommReport_SurveyReponses <- read_csv("~/Desktop/CommReport_SurveyReponses.csv")

#### Merge Survey Address with Responses
CommReport_Addresses <- CommReport_Addresses %>% mutate(Passcode = as.character(Passcode))
CommReport_SurveyReponses <- CommReport_SurveyReponses %>% mutate(Passcode = as.character(Passcode))

Survey_Data <- inner_join(CommReport_Addresses, CommReport_SurveyReponses, by = "Passcode") %>%
  mutate(across(everything(), ~ na_if(., -99)))  # Replace -99 with NA

#### Aggregate Survey Data for Cats
Survey_Data <- Survey_Data %>%
  rename(Latitude = latitude.x, Longitude = longitude.x)
Survey_agg_cat <- Survey_Data %>%
  group_by(Latitude, Longitude) %>%
  summarise(mean_cat_sightings = mean(Q2_CatSH, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

#### Prepare UWIN Dataset for Cats
UWIN_Cats <- UWIN_CoyBirdCat %>%
  filter(Species == "Domestic cat") %>%
  rowwise() %>%
  mutate(Total_Detections = sum(c_across(starts_with("Day_")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    seasons_with_cat = sum(Total_Detections > 0, na.rm = TRUE),
    total_seasons = n(),
    detection_rate = seasons_with_cat / total_seasons,
    .groups = 'drop'
  )

#### Convert to Spatial Data
survey_sf <- st_as_sf(Survey_agg_cat, coords = c("Longitude", "Latitude"), crs = 4326)
uwin_sf <- st_as_sf(UWIN_Cats, coords = c("Longitude", "Latitude"), crs = 4326)

#### Extract Coordinates for UWIN and Buffer by 0.5 Mile
uwin_coords <- uwin_sf %>%
  mutate(
    Lat = st_coordinates(.)[, "Y"],
    Lon = st_coordinates(.)[, "X"]
  )

uwin_buffer <- st_buffer(uwin_coords, dist = set_units(804.67, "m"))

#### Join Survey Observations Within 0.5 Miles of UWIN Sites
survey_join <- st_join(uwin_buffer, survey_sf, join = st_is_within_distance, dist = 804.67) %>%
  st_drop_geometry() %>%
  group_by(Lat, Lon) %>%
  summarise(Survey_cats_per_ha_per_yr = mean(mean_cat_sightings, na.rm = TRUE), .groups = 'drop')

#### Prepare Final Combined Dataset
# Prepare UWIN values with coordinates
uwin_summary <- dplyr::select(uwin_coords, Lat, Lon, UWIN_cats_per_ha_per_yr = detection_rate)

combined_cats <- uwin_summary %>%
  left_join(survey_join, by = c("Lat", "Lon")) %>%
  na.omit()

# First, drop geometry column if it exists
combined_cats_clean <- combined %>% 
  st_drop_geometry() %>% 
  mutate(across(everything(), as.numeric))

#### Statistical Tests
cor_pearson_cats <- cor(combined_cats_clean[, 3:4], method = "pearson")
cor_spearman_cats <- cor(combined_cats_clean[, 3:4], method = "spearman")
cor_kendall_cats <- cor(combined_cats_clean[, 3:4], method = "kendall")


colnames(combined_cats_clean)
str(combined_cats_clean)
summary(combined_cats_clean$UWIN_cats_per_ha_per_yr)
summary(combined_cats_clean$Survey_cats_per_ha_per_yr)


t_uwin_vs_survey_cats <- t.test(combined_cats_clean$UWIN_cats_per_ha_per_yr, combined_cats_clean$Survey_cats_per_ha_per_yr)

#### Print Results
cat("\n--- Pearson Correlation (Cats) ---\n")
print(cor_pearson_cats)

cat("\n--- Spearman Correlation (Cats) ---\n")
print(cor_spearman_cats)

cat("\n--- Kendall Correlation (Cats) ---\n")
print(cor_kendall_cats)

cat("\n--- Welch T-Test: UWIN vs Survey (Cats) ---\n")
print(t_uwin_vs_survey_cats)


#### end ####



#### Load Up - Demographics ####
CommReport_Addresses <- read.csv("~/Desktop/CommReport_Addresses.csv")
CommReport_SurveyReponses <- read.csv("~/Desktop/CommReport_SurveyReponses.csv")
SurveyResults_AND_Demographics <- read.csv("~/Desktop/Survey Chapter/Data/Survey Stats + Analysis/SurveyResults_AND_Demographics.csv")

CommReport_Addresses <- CommReport_Addresses %>%
  mutate(Passcode = as.character(Passcode))
CommReport_SurveyReponses <- CommReport_SurveyReponses %>%
  mutate(Passcode = as.character(Passcode))
Survey_Data <- CommReport_Addresses %>%
  inner_join(CommReport_SurveyReponses, by = "Passcode")
Survey_Data[Survey_Data == -99] <- NA

full_data <- SurveyResults_AND_Demographics
full_data[full_data == -99] <- NA
full_data[full_data == 99] <- NA

library(dplyr)
library(tidyr)
library(readr)

class(full_data)
class(Survey_Data)

full_data$Passcode <- as.character(full_data$Passcode)
Survey_Data$Passcode <- as.character(addresses$Survey_Data)

# Merge all columns from both datasets using a full left join
merged_full <- left_join(full_data, Survey_Data, by = "Passcode")

# Check duplicates in each dataset
table(duplicated(full_data$Passcode))
table(duplicated(Survey_Data$Passcode))

# See which Passcodes are duplicated
full_data %>% filter(duplicated(Passcode) | duplicated(Passcode, fromLast = TRUE))
Survey_Data %>% filter(duplicated(Passcode) | duplicated(Passcode, fromLast = TRUE))


write_xlsx(full_data, "Full_Data.xlsx")
write_xlsx(Survey_Data, "Survey_Data.xlsx")

install.packages("writexl")
library(writexl)
write_xlsx(merged_data, "Merged_Full_Data.xlsx")

#### end ####



#### Demographics ####
uwin_data <- read.csv("~/Desktop/UWIN_CoyBirdCat.csv")
pucp_data <- read.csv("~/Desktop/PUCP_Data.csv")
library(dplyr)
library(sf)
library(broom)

merged_full <- merged_full %>%
  filter(!is.na(longitude.x) & !is.na(latitude.x))
pucp_data <- pucp_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
uwin_data <- uwin_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Count how many are NA
sum(is.na(pucp_data$Latitude))
sum(is.na(pucp_data$Longitude))

# View rows with missing or malformed coordinates
pucp_data %>% filter(is.na(Latitude) | is.na(Longitude))
pucp_data <- pucp_data %>%
  mutate(
    Latitude = as.numeric(gsub("[^0-9.-]", "", Latitude)),
    Longitude = as.numeric(gsub("[^0-9.-]", "", Longitude))
  )
pucp_data <- pucp_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Step 1: Convert to spatial objects
pucp_sf <- st_as_sf(pucp_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
survey_sf <- st_as_sf(merged_full, coords = c("longitude.x", "latitude.x"), crs = 4326, remove = FALSE)
uwin_sf <- st_as_sf(uwin_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Step 2: Create 1-mile buffer (1609.34 meters)
survey_buffer <- st_buffer(survey_sf, dist = units::set_units(1609.34, "m"))

# Step 3: Count PUCP reports within 1-mile of each survey point
survey_sf$pucp_count <- lengths(st_intersects(survey_buffer, pucp_sf))

# Step 4: Average UWIN detection (Total) within 1-mile of each survey point
survey_sf$uwin_coy <- NA
survey_sf$uwin_cat <- NA

for (i in 1:nrow(survey_sf)) {
  buffer_i <- st_buffer(survey_sf[i, ], dist = units::set_units(1609.34, "m"))
  nearby_uwin <- uwin_sf[st_within(uwin_sf, buffer_i, sparse = FALSE), ]
  
  # Separate by species
  survey_sf$uwin_coy[i] <- mean(nearby_uwin$Total[nearby_uwin$Species == "Coyote"], na.rm = TRUE)
  survey_sf$uwin_cat[i] <- mean(nearby_uwin$Total[nearby_uwin$Species == "Domestic cat"], na.rm = TRUE)
}

# Step 5: Correlate each demographic with Q1, Q2, PUCP, and UWIN detections
demographics <- c("LOW_INC_P", "INC_50_P", "INC_125_P", "INC_200_P",
                  "ED_LT_HS_P", "ED_HS_P", "ED_BACH_P", "ED_GRAD_P",
                  "AGE_18_P", "AGE_35_P", "AGE_65_P",
                  "POC_P", "WHITE_A_P", "MULTI_NH_P", "BLACK_NH_P",
                  "HISPANIC _P", "ASIAN_NH_P", "AIAN_NH_P", "NHPI_NH_P", "OTHER_NH_P")

targets <- c("Q1_CoySH.x", "Q2_CatSH.x", "pucp_count", "uwin_coy", "uwin_cat")

# Spearman
spearman_results <- list()

for (demo in demographics) {
  for (target in targets) {
    if (all(c(demo, target) %in% names(survey_sf))) {
      df <- survey_sf[, c(demo, target)] %>% na.omit()
      if (nrow(df) > 2) {
        test <- cor.test(df[[demo]], df[[target]], method = "spearman")
        spearman_results[[paste(demo, target, sep = "_vs_")]] <- data.frame(
          Demographic = demo,
          Target = target,
          Correlation = round(test$estimate, 3),
          P_Value = round(test$p.value, 4),
          CI_Lower = NA,  # Spearman doesn't provide confidence intervals by default
          CI_Upper = NA,
          N = nrow(df),
          Method = "Spearman"
        )
      }
    }
  }
}

# Combine into a dataframe
spearman_cor_df <- bind_rows(spearman_results)
View(spearman_cor_df)
write.csv(spearman_cor_df, "Spearman_Correlations_Wildlife_vs_Demographics.csv", row.names = FALSE)

summary(spearman_cor_df)



# Visualizing
library(ggplot2)
library(dplyr)
library(readr)

# Read in your Spearman correlation results
spearman_df <- read_csv("Spearman_Correlations_Wildlife_vs_Demographics.csv")

# Filter to significant results (p < 0.05)
sig_spearman <- spearman_df %>%
  filter(P_Value < 0.05)

# Sort by absolute correlation strength and select Top 20
sig_top20 <- sig_spearman %>%
  arrange(desc(abs(Correlation))) %>%
  slice_head(n = 20)

# Create a clean barplot
ggplot(sig_top20, aes(x = reorder(Demographic, Correlation), y = Correlation, fill = Target)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 20 Significant Spearman Correlations",
    subtitle = "Demographics vs Wildlife Detections",
    x = "Demographic Variable",
    y = "Spearman Correlation (ρ)",
    fill = "Detection Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )






label_key <- c(
  "ED_BACH_P" = "Bachelor's Degree (%)",
  "BLACK_NH_P" = "Black or African American (%)",
  "ASIAN_NH_P" = "Asian (%)",
  "ED_GRAD_P" = "Graduate Degree (%)",
  "MULTI_NH_P" = "Multiracial (%)",
  "OTHER_NH_P" = "Other Race (%)",
  "AGE_18_P" = "Age 18–24 (%)",
  "POC_P" = "People of Color (%)",
  "AGE_35_P" = "Age 35–64 (%)",
  "INC_125_P" = "Income ≥ $125k (%)",
  "AGE_65_P" = "Age 65+ (%)",
  "INC_50_P" = "Income < $50k (%)",
  "AIAN_NH_P" = "American Indian or Alaska Native (%)",
  "ED_HS_P" = "High School Only (%)",
  "ED_LT_HS_P" = "Less than High School (%)"
)

# Apply readable labels to x-axis
sig_top20$DemographicReadable <- label_key[sig_top20$Demographic]

# Also relabel Detection Type in legend
sig_top20$DetectionLabel <- recode(sig_top20$Target,
                                   "pucp_count" = "PUCP Reports",
                                   "Q1_CoySH.x" = "Survey: Coyote Sightings",
                                   "Q2_CatSH.x" = "Survey: Cat Sightings"
)

ggplot(sig_top20, aes(x = reorder(DemographicReadable, Correlation), y = Correlation, fill = DetectionLabel)) +
  geom_col() +
  geom_text(aes(label = Label),
            hjust = ifelse(sig_top20$Correlation > 0, -0.2, 1.2),
            color = "black", size = 3.5) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 20 Significant Spearman Correlations",
    subtitle = "Demographic Predictors of Wildlife Detections\n(N = 57 combinations tested)",
    x = "Demographic Group",
    y = "Spearman Correlation (ρ)",
    fill = "Detection Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


# Prepare data for heatmap
# Use base R selection instead of dplyr::select
heatmap_data <- sig_spearman[, c("Demographic", "Target", "Correlation")]

# Plot the heatmap
ggplot(heatmap_data, aes(x = Target, y = Demographic, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Spearman ρ") +
  labs(
    title = "Heatmap of Significant Spearman Correlations",
    subtitle = "Demographics vs Wildlife Detections",
    x = "Wildlife Detection Variable",
    y = "Demographic Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )


#Tried Regression
# Load libraries
library(dplyr)
library(broom)
library(MASS)  # For Negative Binomial
library(sf)

# STEP 1: Prepare Dataset
# Drop spatial geometry to remove 'geometry' column
survey_df <- st_drop_geometry(survey_sf)

# Make sure uwin_coy and uwin_cat are numeric
survey_df$uwin_coy <- as.numeric(survey_df$uwin_coy)
survey_df$uwin_cat <- as.numeric(survey_df$uwin_cat)

# Quick check
table(survey_df$uwin_coy, useNA = "always")
table(survey_df$uwin_cat, useNA = "always")

# STEP 2: Define Predictor Variables
demographics <- c("LOW_INC_P", "INC_50_P", "INC_125_P", "INC_200_P",
                  "ED_LT_HS_P", "ED_HS_P", "ED_BACH_P", "ED_GRAD_P",
                  "AGE_18_P", "AGE_35_P", "AGE_65_P",
                  "POC_P", "WHITE_A_P", "MULTI_NH_P", "BLACK_NH_P",
                  "HISPANIC_P", "ASIAN_NH_P", "AIAN_NH_P", "NHPI_NH_P", "OTHER_NH_P")

# STEP 3: Run Models

# 3.1 Linear Regression: Survey Coyote sightings
lm_coyote <- lm(Q1_CoySH.x ~ ., data = survey_df[, c("Q1_CoySH.x", demographics)], na.action = na.exclude)

# 3.2 Linear Regression: Survey Cat sightings
lm_cat <- lm(Q2_CatSH.x ~ ., data = survey_df[, c("Q2_CatSH.x", demographics)], na.action = na.exclude)

# 3.3 Logistic Regression: UWIN Data (Skip due to no valid data)
if (all(is.na(survey_df$uwin_coy))) {
  message("⚠️ UWIN coyote data has no valid detections (all NaN). Logistic regression skipped.")
} else {
  logit_coyote <- glm(uwin_coy ~ ., 
                      data = survey_df[!is.na(survey_df$uwin_coy), c("uwin_coy", demographics)], 
                      family = "binomial")
}

if (all(is.na(survey_df$uwin_cat))) {
  message("⚠️ UWIN cat data has no valid detections (all NaN). Logistic regression skipped.")
} else {
  logit_cat <- glm(uwin_cat ~ ., 
                   data = survey_df[!is.na(survey_df$uwin_cat), c("uwin_cat", demographics)], 
                   family = "binomial")
}

# 3.4 Poisson Regression: PUCP Citizen Science Reports
poisson_model <- glm(pucp_count ~ ., data = survey_df[, c("pucp_count", demographics)], family = "poisson")

# Check overdispersion
overdispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual

# If overdispersed, switch to Negative Binomial
if (overdispersion_ratio > 1.5) {
  message("⚠️ Overdispersion detected (ratio = ", round(overdispersion_ratio, 2), "). Switching to Negative Binomial model.")
  nb_model <- glm.nb(pucp_count ~ ., data = survey_df[, c("pucp_count", demographics)])
} else {
  message("✅ No major overdispersion. Using Poisson model.")
  nb_model <- poisson_model
}

# STEP 4: Summarize Results

summary(lm_coyote)
summary(lm_cat)
summary(nb_model)

# Tidy outputs
lm_coyote_summary <- tidy(lm_coyote)
lm_cat_summary <- tidy(lm_cat)
nb_model_summary <- tidy(nb_model)

# View (optional if running interactively)
View(lm_coyote_summary)
View(lm_cat_summary)
View(nb_model_summary)

# STEP 5: Export to CSV
write.csv(lm_coyote_summary, "Linear_Coyote_Survey_Model.csv", row.names = FALSE)
write.csv(lm_cat_summary, "Linear_Cat_Survey_Model.csv", row.names = FALSE)
write.csv(nb_model_summary, "PUCP_Coyote_Count_Model.csv", row.names = FALSE)




# Tried Pearsons -> NO 
results <- list()

for (demo in demographics) {
  for (target in targets) {
    if (all(c(demo, target) %in% names(survey_sf))) {
      df <- survey_sf[, c(demo, target)] %>% na.omit()
      if (nrow(df) > 2) {
        test <- cor.test(df[[demo]], df[[target]], method = "pearson")
        results[[paste(demo, target, sep = "_vs_")]] <- data.frame(
          Demographic = demo,
          Target = target,
          Correlation = round(test$estimate, 3),
          P_Value = round(test$p.value, 4),
          CI_Lower = round(test$conf.int[1], 3),
          CI_Upper = round(test$conf.int[2], 3),
          N = nrow(df)
        )
      }
    }
  }
}

# Combine and view
cor_results <- bind_rows(results)
View(cor_results)

write.csv(cor_results, "Wildlife_Detection_vs_Demographics.csv", row.names = FALSE)



#### end ####






# Load required libraries
install.packages("ggeffects")
library(ggplot2)
library(ggeffects)
library(MASS)
library(dplyr)

colnames(Survey_Data)
colnames(survey_sf)

# --- Model 1: Linear Regression for Survey-Based Coyote Sightings ---
lm_coyote <- lm(Q1_CoySH.x ~ ED_GRAD_P, data = survey_sf)

# --- Model 2: Negative Binomial Regression for PUCP Reports ---
nb_model <- glm.nb(pucp_count ~ ED_GRAD_P, data = survey_sf)

# --- Generate Predicted Values ---
pred_lm <- ggpredict(lm_coyote, terms = "ED_GRAD_P")
pred_nb <- ggpredict(nb_model, terms = "ED_GRAD_P")

# --- Plot the Predictions ---
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Graduate Degree (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents with Graduate Degrees (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)



lm_coyote <- lm(Q1_CoySH.x ~ LOW_INC_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ LOW_INC_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "LOW_INC_P")
pred_nb <- ggpredict(nb_model, terms = "LOW_INC_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Low Income (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents with Low Incomes (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)


lm_coyote <- lm(Q1_CoySH.x ~ POC_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ POC_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "POC_P")
pred_nb <- ggpredict(nb_model, terms = "POC_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by People of Color (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents Identify as a Person of Color (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)


lm_coyote <- lm(Q1_CoySH.x ~ MALE_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ MALE_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "MALE_P")
pred_nb <- ggpredict(nb_model, terms = "MALE_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Human Males (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents Registered as Males (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)


lm_coyote <- lm(Q1_CoySH.x ~ FEMALE_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ FEMALE_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "FEMALE_P")
pred_nb <- ggpredict(nb_model, terms = "FEMALE_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Human Females (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents Registered as Females (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)


lm_coyote <- lm(Q1_CoySH.x ~ AGE_18_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ AGE_18_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "AGE_18_P")
pred_nb <- ggpredict(nb_model, terms = "AGE_18_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Residents of 18+ Years Old (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents 18 Years or Older (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)

colnames(survey_sf)
lm_coyote <- lm(Q1_CoySH.x ~ AGE_35_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ AGE_35_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "AGE_35_P")
pred_nb <- ggpredict(nb_model, terms = "AGE_35_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Residents of 35+ Years Old (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents 35 Years or Older (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)

lm_coyote <- lm(Q1_CoySH.x ~ AGE_65_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ AGE_65_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "AGE_65_P")
pred_nb <- ggpredict(nb_model, terms = "AGE_65_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Residents of 65 Years Old (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents 65 Years or Older (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)

lm_coyote <- lm(Q1_CoySH.x ~ AGE_75_P, data = survey_sf)
nb_model <- glm.nb(pucp_count ~ AGE_75_P, data = survey_sf)
pred_lm <- ggpredict(lm_coyote, terms = "AGE_75_P")
pred_nb <- ggpredict(nb_model, terms = "AGE_75_P")
ggplot() +
  geom_line(data = pred_lm, aes(x = x, y = predicted), 
            color = "steelblue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_nb, aes(x = x, y = predicted), 
            color = "darkred", size = 1.2) +
  labs(
    title = "Predicted Coyote Reports by Residents of 75+ Years Old (%)",
    subtitle = "Dashed blue = Survey Sightings | Solid red = PUCP Reports",
    x = "Proportion of Residents 75 Years or Older (%)",
    y = "Predicted Coyote Reports"
  ) +
  theme_minimal(base_size = 14)








##### Coyotes Version 1 ####
UWIN_Intersect_Table <- read.csv("~/Downloads/UWIN_Intersect_Table.csv")
PUCP_Intersect_Table <- read.csv("~/Downloads/PUCP_Intersect_Table.csv")
Survey_Intersect_Table <- read.csv("~/Downloads/Survey_Intersect_Table.csv")
uwin <- UWIN_Intersect_Table
pucp <- PUCP_Intersect_Table
survey <- Survey_Intersect_Table


pucp$PUCP_coyotes_per_ha_per_yr <- pucp$total_coyote_reports_ / 518 / 4 # PUCP: total_coyote_reports_ from 2020–2023 (4 years)
uwin$UWIN_coyotes_per_ha_per_yr <- uwin$TotalCoyotes / 518 / 4 # UWIN: TotalCoyotes from 2020–2023 (4 years)
survey$Survey_coyotes_per_ha_per_yr <- survey$mean_coyote_sightings / 518 # Survey: mean_coyote_sightings in the past year


# Merge all three datasets by Camera_Site_ID
combined <- Reduce(function(x, y) merge(x, y, by = "Camera_Site_ID", all = TRUE),
                   list(uwin[, c("Camera_Site_ID", "UWIN_coyotes_per_ha_per_yr")],
                        pucp[, c("Camera_Site_ID", "PUCP_coyotes_per_ha_per_yr")],
                        survey[, c("Camera_Site_ID", "Survey_coyotes_per_ha_per_yr")]))

cor(combined[, c("UWIN_coyotes_per_ha_per_yr", "PUCP_coyotes_per_ha_per_yr", "Survey_coyotes_per_ha_per_yr")], use = "complete.obs")

plot(combined$UWIN_coyotes_per_ha_per_yr, combined$PUCP_coyotes_per_ha_per_yr,
     xlab = "UWIN (camera trap)", ylab = "PUCP (citizen science)",
     main = "Camera Trap vs Citizen Science",
     pch = 19)
abline(lm(PUCP_coyotes_per_ha_per_yr ~ UWIN_coyotes_per_ha_per_yr, data = combined), col = "blue")



#Version 2 Using Detection Rate VS TotalCoy
uwin$UWIN_coyotes_per_ha_per_yr_detec <- uwin$detection_rate_ / 518 / 4 # UWIN: TotalCoyotes from 2020–2023 (4 years)
combined_V2 <- Reduce(function(x, y) merge(x, y, by = "Camera_Site_ID", all = TRUE),
                      list(uwin[, c("Camera_Site_ID", "UWIN_coyotes_per_ha_per_yr_detec")],
                           pucp[, c("Camera_Site_ID", "PUCP_coyotes_per_ha_per_yr")],
                           survey[, c("Camera_Site_ID", "Survey_coyotes_per_ha_per_yr")]))
cor(combined_V2[, c("UWIN_coyotes_per_ha_per_yr_detec", "PUCP_coyotes_per_ha_per_yr", "Survey_coyotes_per_ha_per_yr")], use = "complete.obs")

cor_matrix <- cor(full_data[, c("Utilitarian_Index", "Biocentric_Index", "Stewardship_Index", 
                                "total_good.y", "total_bad.y", "final_score.y")], 
                  use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, col = colorRampPalette(c("blue", "white", "red"))(200))


#### end ####

##### Cats Version 1 ####
UWIN_CoyBirdCat <- UWIN_CoyBirdCat %>%
  rowwise() %>%  # Ensures summation is row-based
  mutate(Total_Detections = sum(c_across(starts_with("Day_")), na.rm = TRUE)) %>%
  ungroup()

Survey_Cat_agg <- Survey_Data %>%
  group_by(latitude.x, longitude.x) %>%
  summarise(mean_cat_sightings = mean(Q2_CatSH, na.rm = TRUE),
            count_responses = n())  # Number of responses per location
Survey_Cat_agg <- Survey_Cat_agg %>%
  rename(Latitude = latitude.x, Longitude = longitude.x)

#Since this dataset is binary per 40-day season, we count how many seasons had at least one detection per location
UWIN_Cat_agg <- UWIN_CoyBirdCat %>%
  filter(Species == "Domestic cat") %>%  
  group_by(Latitude, Longitude) %>%
  summarise(
    seasons_with_cats = sum(Total_Detections > 0, na.rm = TRUE),  # Count seasons with ≥1 detection
    total_seasons = n(),  # Total seasons sampled
    detection_rate = seasons_with_cats / total_seasons  # Proportion of seasons with detections
  )
write.csv(Survey_Cat_agg, "Survey_Cat_agg.csv", row.names = FALSE)
write.csv(UWIN_Cat_agg, "UWIN_Cat_agg.csv", row.names = FALSE)



UWIN_Cat_Intercept_Table <- read.csv("~/Downloads/UWIN_Cat_Intercept_Table.csv")
Survey_Cat_Intercept_Table <- read.csv("~/Downloads/Survey_Cat_Intercept_Table.csv")
uwin_cats <- UWIN_Cat_Intercept_Table
survey_cats <- Survey_Cat_Intercept_Table

uwin_cats$UWIN_cats_per_ha_per_yr <- uwin_cats$detection_rate / 518 / 4 
survey_cats$Survey_cats_per_ha_per_yr <- survey_cats$mean_cat_sightings / 518 
combined <- Reduce(function(x, y) merge(x, y, by = "Camera_Site_ID", all = TRUE),
                   list(uwin_cats[, c("Camera_Site_ID", "UWIN_cats_per_ha_per_yr")],
                        survey_cats[, c("Camera_Site_ID", "Survey_cats_per_ha_per_yr")]))
cor(combined[, c("UWIN_cats_per_ha_per_yr", "Survey_cats_per_ha_per_yr")], use = "complete.obs")

plot(combined$UWIN_cats_per_ha_per_yr, combined$Survey_cats_per_ha_per_yr,
     xlab = "UWIN (camera trap)", ylab = "Survey Responses",
     main = "Camera Trap vs Survey Responses",
     pch = 19)
abline(lm(Survey_cats_per_ha_per_yr ~ UWIN_cats_per_ha_per_yr, data = combined), col = "blue")

#### end ####

##### Birds ####
^ Do same as Coyotes BUT only use eBird data and UWIN data

library(dplyr)
library(readr)

all_species <- c(washington$COMMON.NAME, clackamas$COMMON.NAME, multnomah$COMMON.NAME)
unique_species_all <- sort(unique(unique(all_species)))
print(unique_species_all)

eBird_MultnomahCounty <- read.csv("~/Downloads/eBird_MultnomahCounty.csv")
eBird_WashingtonCounty <- read.csv("~/Downloads/eBird_WashingtonCounty.csv")
eBird_ClackamasCounty <- read.csv("~/Downloads/eBird_ClackamasCounty.csv")
washington <- eBird_WashingtonCounty
clackamas <- eBird_ClackamasCounty
multnomah <- eBird_MultnomahCounty

combined_ebird <- bind_rows(washington, clackamas, multnomah) # Step 2: Combine all three datasets
thesis_species <- c(
  "American Crow", "American Kestrel", "American Goldfinch", "American Robin",
  "Anna's Hummingbird", "Anas sp.", "Bald Eagle", "Barred Owl", "Bewick's Wren",
  "bluebird sp.", "Black-capped Chickadee", "Black-headed Grosbeak", "Brown Creeper",
  "California Quail", "California Scrub-Jay", "Canada Goose", "Chestnut-backed Chickadee",
  "Chipping Sparrow", "Common Yellowthroat", "Cooper's Hawk", "Dark-eyed Junco",
  "Downy Woodpecker", "Downy/Hairy Woodpecker", "Eurasian Collared-Dove", "European Starling",
  "Golden-crowned Sparrow", "Great Blue Heron", "Great Egret", "Great Horned Owl",
  "Hairy Woodpecker", "Hermit Thrush", "hummingbird sp.", "Mallard", "Mourning Dove",
  "Nashville Warbler", "Northern Flicker", "Orange-crowned Warbler", "owl sp.",
  "Pacific Wren", "Red-winged Blackbird", "Ruby-crowned Kinglet", "Song Sparrow",
  "Sora", "Spotted Towhee", "Steller's Jay", "Swainson's Thrush", "Varied Thrush",
  "Western Screech-Owl", "White-crowned Sparrow", "Wilson's Snipe", "Wilson's Warbler",
  "Yellow-rumped Warbler", "Wood Duck", "hawk sp.", "Cedar Waxwing"
)
filtered_ebird <- combined_ebird %>%
  filter(`COMMON.NAME` %in% thesis_species) # Step 4: Filter the combined dataset for only those species
print(head(filtered_ebird))


#Comparing to UWIN 
install.packages(c("sf", "dplyr", "units"))
library(sf)
library(dplyr)
library(units)

bird_only_uwin <- UWIN_CoyBirdCat %>%
  filter(!(Species %in% c("Coyote", "Domestic cat")))

# Convert eBird to sf
ebird_sf <- filtered_ebird %>%
  rename(Latitude = LATITUDE, Longitude = LONGITUDE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Convert UWIN (bird-only) to sf
uwin_sf <- bird_only_uwin %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to a projection suitable for distance (meters)
ebird_sf_proj <- st_transform(ebird_sf, 32610)  # UTM Zone 10N (covers Oregon)
uwin_sf_proj  <- st_transform(uwin_sf, 32610)
buffer_distance <- set_units(2, miles)  # 2-mile buffer
uwin_buffered <- st_buffer(uwin_sf_proj, dist = buffer_distance)




install.packages("RANN")
library(dplyr)
library(sf)
library(RANN)

# Convert to sf and transform to projected CRS
ebird_sf <- filtered_ebird %>%
  rename(Latitude = LATITUDE, Longitude = LONGITUDE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32610)

uwin_sf <- bird_only_uwin %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32610)

# Extract coordinates
ebird_coords <- st_coordinates(ebird_sf)
uwin_coords  <- st_coordinates(uwin_sf)


library(RANN)

# Set a smaller k (e.g., check only 10 nearest UWIN sites)
k_nearest <- 10
search_radius <- 3218.69  # 2 miles in meters

nn_result <- nn2(
  data = uwin_coords,
  query = ebird_coords,
  k = k_nearest,
  searchtype = "radius",
  radius = search_radius
)




# Create pairings: which eBird points fall within 2 miles of which UWIN sites
matched_idx <- which(nn_result$nn.dists < 3218.69, arr.ind = TRUE)

ebird_matches <- data.frame(
  ebird_row = matched_idx[, 1],
  uwin_row = nn_result$nn.idx[matched_idx],
  distance_m = nn_result$nn.dists[matched_idx]
)

# Add back Site info
ebird_matches$Site <- uwin_sf$Site[ebird_matches$uwin_row]

# Count eBird birds near each UWIN site
ebird_counts <- ebird_matches %>%
  group_by(Site) %>%
  summarise(eBird_bird_count = n(), .groups = "drop")

# UWIN bird counts per site
uwin_counts <- bird_only_uwin %>%
  group_by(Site) %>%
  summarise(UWIN_bird_count = n(), .groups = "drop")

# Join and correlate
comparison_df <- inner_join(ebird_counts, uwin_counts, by = "Site")

# Correlation
cor_result <- cor.test(comparison_df$eBird_bird_count, comparison_df$UWIN_bird_count)
print(cor_result)

# Plot
library(ggplot2)

ggplot(comparison_df, aes(x = eBird_bird_count, y = UWIN_bird_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "eBird vs UWIN Bird Counts (within 2-mile radius)",
       x = "eBird Bird Count (within 2 miles)",
       y = "UWIN Bird Count") +
  theme_minimal()












ebird_summary <- filtered_ebird %>%
  group_by(SAMPLING.EVENT.IDENTIFIER, COMMON.NAME) %>%
  summarise(eBird_count = n(), .groups = "drop") # Step 2a: Aggregate eBird data (e.g., by species count per site)

uwin_summary <- bird_only_uwin %>%
  group_by(Camera_Location, Species) %>%  # Adjust column names as needed
  summarise(UWIN_count = n(), .groups = "drop") # Step 2b: Aggregate UWIN bird data (e.g., species count per camera site)





^Then compare to the PUCP data


#### end ####







# Changing to numeric
Survey_agg <- Survey_agg %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

PUCP_agg <- PUCP_agg %>%
  mutate(
    Latitude = as.numeric(gsub("[^0-9.-]", "", Latitude)),
    Longitude = as.numeric(gsub("[^0-9.-]", "", Longitude))
  )
PUCP_agg %>%
  filter(is.na(Latitude) | is.na(Longitude)) %>%
  select(Latitude, Longitude) # Check rows where Latitude or Longitude is NA
PUCP_agg <- PUCP_agg %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

UWIN_agg <- UWIN_agg %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
colnames(Survey_agg)
colnames(PUCP_agg)
colnames(UWIN_agg)