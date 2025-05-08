# Amanda Batlle-Morera (a.batlle@creaf.uab.cat)

# 1. DATA PRE-PROCESSING: 
# Functions to convert input dataset into a format to enter a data quality and filtering workflow

# Install libraries
# Require packages 
requiredPackages <- c("readr", "dplyr", "tidyr", "lubridate", "sf", "ggplot2" )
# Check and install only missing packages
install.packages(setdiff(requiredPackages, rownames(installed.packages())), depenencies = TRUE)
# Load these packages 
lapply(requiredPackages, library, character.only = TRUE)

# FUNCTION CREATE THESAURUS ________________________________________________####

# Write a thesaurus for variable VAR
Thesaurus_var <- function (df, VAR) {
  df <- df%>%
    mutate(
      x=C_X,
      y=C_Y
    )
  # Convert Thesaurus in a sf object: 
  df_sf <- st_as_sf(df,  coords = c("x", "y"), crs = 25831)
  all_stations_sf <- df_sf[!duplicated(df_sf$CODE), c("CODE", "NAME_STATION", "ALT", "source", "C_X", "C_Y", "geometry")]
  
  write.csv(all_stations_sf, paste0("Thesaurus_", VAR, ".csv"))
  return (all_stations_sf)
  
}