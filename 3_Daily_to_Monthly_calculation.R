# Amanda Batlle-Morera (a.batlle@creaf.uab.cat)

# 1. CONVERTING DAILY PRECIPITATION DATA TO MONTHLY DATA.

# Install libraries
# Require packages 
requiredPackages <- c("readr", "dplyr", "tidyr", "lubridate", "sf", "ggplot2" )
# Check and install only missing packages
install.packages(setdiff(requiredPackages, rownames(installed.packages())), depenencies = TRUE)
# Load these packages 
lapply(requiredPackages, library, character.only = TRUE)


#### Calculating monthly accumulated PCP from daily data: ________________________________________####

# According to Calculation of Monthly and Annual 30-year Standard Normals (WMO, 1989) the recommendation for calculation monthly Total values is: 
#  "Precipitation Total—Totals shall be calculated for each month of each year from daily data. Monthly totals should be based on a full month's data." 

# Group by 'year' and 'month', and filter out months with more than 1 NAs
df_clean2 <- DATA_stacked2 %>%
  group_by(CODE_INM, YEAR, MONTH) %>%
  filter(sum(is.na(PL)) ==0) %>%
  ungroup()

na_rows <- DATA_stacked2 %>%
  filter(is.na(PL))

write_csv(df_clean2, "AEMET_SpanishLL_PrecipiationDaily_CompleteCases.csv") 

# Calculation Monthly Total PCP values _______________________________________________________####
df_monthly <- df_clean2 %>%
  group_by(CODE_INM, YEAR, MONTH,ALT, C_X, C_Y) %>%
  summarize (PL_mm = sum(PL, na.rm = TRUE),
             N_DATA = n()) %>%
  ungroup()

write_csv(df_monthly, "AEMET_SpanishLL_TotalMonthlyPrecipitationData_1.csv")


# Calculating MONTHLY MEAN values: "3/5 RULE" for temperature__________________________________________####

# According to Calculation of Monthly and Annual 30-year Standard Normals (WMO, 1989) the recommendation for calculation monthly MEAN values is: 
#   "3/5 rule": Monthly mean values should not be calculated if either of the following criteria are satified: 
#       – >3 consecutive daily values are missing;
#       – >5 daily values in total in a given month are missing. 
# There is a newer document WMO Guidelines on the Calculation of Climate Normals (WMO, 2017) that have a less strick rule (5/11 rule) but many countries decided to stick to the old rule to have consistency on their data series. 
# 5/3 rule standards, is often referenced in general climatological practices.


# Remove stations that have on a month >3 consecutive NA values. 

# Function to detect stations with more than 3 consecutive NA values
detect_consecutive_na <- function(tmean) {
  # Run-length encoding to detect consecutive NAs
  rle_result <- rle(is.na(tmean))
  # Check if any consecutive NA lengths are greater than 3
  any(rle_result$lengths[rle_result$values] > 3)
}

# #For debugging
# testfile <- "C:/Amanda_LocalProjects/local_ICISK/AEMET_ABM_2/TEMP/Data_processing/data_test.csv"
# df_subset <-  read_delim(testfile, delim= ";", show_col_types = FALSE)
# 
# Group by 'year' and 'month', and filter out months with more than 3 consecutive NAs
df_rule3_5 <- df_dailyoutlierout %>%
  group_by(CODE_INM, YEAR, MONTH) %>%
  # Remove months if they have more than 3 consecutive NAs or more than 5 total NAs
  filter(!detect_consecutive_na(TMEAN) | sum(is.na(TMEAN)) <=5) %>%
  ungroup()

