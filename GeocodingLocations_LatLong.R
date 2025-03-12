
install.packages("tidyverse")  # If not installed
library(tidyverse)
install.packages("dplyr")  # If not installed
library(dplyr)
install.packages("tidygeocoder")  # If not installed
library(tidygeocoder)



install.packages("dplyr")  # If not installed
library(dplyr)

df <- Survey_Address_GeoCode
# Combine address components into a single column
df <- df %>%
  mutate(Full_Address = paste(ADDRESS_NU, STREET_N_1, JURISDICTI, STATE_ABBR, sep = ", "))
head(df)



# Geocode using OpenStreetMap (Nominatim)
df <- df %>%
  geocode(Full_Address, method = "osm", long = longitude, lat = latitude)

print(df)

write.csv(df, "/Users/cressot/Desktop/Survey_Addresses_LatLong.csv", row.names = FALSE)
