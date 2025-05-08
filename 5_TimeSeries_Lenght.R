# Amanda Batlle-Morera (a.batlle@creaf.uab.cat)

# 5. Filter time series by lenght



# Install libraries
# Require packages 
requiredPackages <- c("dplyr", "tidyr","ggplot2" ) # 
# Check and install only missing packages
install.packages(setdiff(requiredPackages, rownames(installed.packages())), depenencies = TRUE)
# Load these packages 
lapply(requiredPackages, library, character.only = TRUE)





#  FUNCTIONS for separate data sets by variable________________________________________________________________####

# Function to complete time series sequence with NA values. 

complete_date_sequence <- function(df, VAR, start_date, end_date, time_step) {
  # Create a sequence of all dates
  all_dates <- seq(from = as.Date(start_date), to = as.Date(end_date), by = time_step)
  
  # Fill in missing dates for each station code
  df_complete <- df %>%
    select(DATE, CODE, all_of(VAR)) %>%
    group_by(CODE) %>%
    complete(DATE = all_dates) %>%
    ungroup()
  
  return(df_complete)
}


PCP_data2 <- complete_date_sequence(PCP_data, "PCP", "2000-01-01", "2022-12-31", "1 day" ) 
MIN_data2 <- complete_date_sequence(MIN_data, "MIN", "2000-01-01", "2022-12-31", "1 day" ) 
MAX_data2 <- complete_date_sequence(MAX_data, "MAX", "2000-01-01", "2022-12-31", "1 day" ) 
SLR_data2 <- complete_date_sequence(SLR_data, "SLR", "2000-01-01", "2022-12-31", "1 day" ) 
WND_data2 <- complete_date_sequence(WND_data, "WND", "2000-01-01", "2022-12-31", "1 day" ) 
HMD_data2 <- complete_date_sequence(HMD_data, "HMD", "2000-01-01", "2022-12-31", "1 day" ) 


# Function to identify Short time series ####
# Fore separate datasets by variable: 
NApc <- function(df, VAR) {
  df_NApc <- df %>%
    group_by(CODE) %>%
    summarise(
      NApc = mean(is.na(.data[[VAR]])) * 100
    ) %>%
    ungroup()
  
  return(df_NApc)
}

PCP_NApc <- NApc(PCP_data2, "PCP")
MIN_NApc <- NApc(MIN_data2, "MIN")
MAX_NApc <- NApc(MAX_data2, "MAX")
SLR_NApc <- NApc(SLR_data2, "SLR")
WND_NApc <- NApc(WND_data2, "WND")
HMD_NApc <- NApc(HMD_data2, "HMD")

#Merge Variables for all stations
NApc <- PCP_NApc %>%
  full_join (MIN_NApc, by= c("CODE"), suffix = c(".PCP", ".MIN"))%>%
  full_join (MAX_NApc, by= c("CODE"), suffix = c("", ".MAX"))%>%
  full_join (SLR_NApc, by= c("CODE"), suffix = c("", ".SLR"))%>%
  full_join (WND_NApc, by= c("CODE"), suffix = c("", ".WND"))%>%
  full_join (HMD_NApc, by= c("CODE"), suffix = c("", ".HMD"))

write.csv(NApc, "NApc_stations.csv")

# For complete dataset with all variables: 
NApc_multivarible <- function(df) {
  df_NApc <- df %>%
    group_by(CODE) %>%
    summarise(
      NApc_PCP = mean(is.na(PCP)) * 100 ,
      NApc_MIN = mean(is.na(MIN)) * 100 ,
      NApc_MAX = mean(is.na(MAX)) * 100 ,
      NApc_SLR = mean(is.na(SLR)) * 100 ,
      NApc_WND = mean(is.na(WND)) * 100 ,
      NApc_HMD = mean(is.na(HMD)) * 100 
      
    ) %>%
    ungroup()
  
  return(df_NApc)
}

NApc <- NApc_multivarible (weather_data)

write.csv(NApc, "NApc_stations.csv")

# Function to delete stations with more than X% of NA values

Remove_short_time_series <- function (df, df_NApc, VAR, NApc_threshold) {
  # Subset stations over NApc threshold. 
  HighNA_stations <- df_NApc%>%
    filter(NApc>=NApc_threshold)%>%
    pull(CODE)
  # Filter out data from this stations: 
  df_filtered <- df%>%
    filter(!CODE %in% HighNA_stations)
  
  return (df_filtered)
}

PCP_data3 <- Remove_short_time_series(PCP_data2, PCP_NApc, "PCP", 70)
MIN_data3 <- Remove_short_time_series(MIN_data2, MIN_NApc, "MIN", 70)
MAX_data3 <- Remove_short_time_series(MAX_data2, MAX_NApc, "MAX", 70)
MAX_data3 <- MAX_data3 %>%
  filter(filter(!(CODE %in% )))


SLR_data3 <- Remove_short_time_series(SLR_data2, SLR_NApc, "SLR", 70)
WND_data3 <- Remove_short_time_series(WND_data2, WND_NApc, "WND", 70)
HMD_data3 <- Remove_short_time_series(HMD_data2, HMD_NApc, "HMD", 70)

PCP_data4 <- PCP_data3%>%left_join(thesaurus_PCP_GeoFilt, by = "CODE")
MIN_data4 <- MIN_data3%>%left_join(thesaurus_MIN_GeoFilt, by = "CODE")
MAX_data4 <- MAX_data3%>%left_join(thesaurus_MAX_GeoFilt, by = "CODE")
SLR_data4 <- SLR_data3%>%left_join(thesaurus_SLR_GeoFilt, by = "CODE")
WND_data4 <- WND_data3%>%left_join(thesaurus_WND_GeoFilt, by = "CODE")
HMD_data4 <- HMD_data3%>%left_join(thesaurus_HMD_GeoFilt, by = "CODE")


thesaurus_PCP_GeoLenghtFILT <-Thesaurus_var (PCP_data4, "PCP")
thesaurus_MIN_GeoLenghtFILT <-Thesaurus_var (MIN_data4, "MIN")
thesaurus_MAX_GeoLenghtFILT <-Thesaurus_var (MAX_data4, "MAX")
thesaurus_SLR_GeoLenghtFILT <-Thesaurus_var (SLR_data4, "SLR")
thesaurus_WND_GeoLenghtFILT <-Thesaurus_var (WND_data4, "WND")
thesaurus_HMD_GeoLenghtFILT <-Thesaurus_var (HMD_data4, "HMD")



# PLOTTING GRAPHs with thenumber of valid measure by date_________________________________________________________________________________________####

#PCP

df_inicial <- PCP_ROI_weather_data
df_geofilt <- PCP_data
df_lenghtfilt <- PCP_data3


Plot_station_number_filtering <- function (df_inicial, df_geofilt, df_lenghtfilt, VAR ) {
  # Plotting number of station for each month of the series
  Available_values_INI <- df_inicial%>%
    rename(VAR_col = all_of(VAR)) %>% 
    select (DATE, VAR_col) %>%
    group_by(DATE) %>%
    summarise(available_values_INICIAL = sum(!is.na(VAR_col)), .groups = "drop")
  
  
  Available_values_GEO <- df_geofilt %>%
    rename(VAR_col = all_of(VAR)) %>%          # Use all_of for string column names
    select (DATE, VAR_col) %>%
    group_by(DATE) %>%
    summarise(available_values_GEO =  sum(!is.na(VAR_col)), .groups = "drop") # Count number of valid values per DATE
  
  Available_values_GEO_SER <- df_lenghtfilt %>%
    rename(VAR_col = all_of(VAR)) %>%          # Use all_of for string column names
    select (DATE, VAR_col) %>%
    group_by(DATE) %>%
    summarise(available_values_GEO_SER =  sum(!is.na(VAR_col)), .groups = "drop") # Count number of valid values per DATE
  
  # Combine all into a single dataframe for plotting if needed
  df_plot <- Available_values_INI %>%
    left_join(Available_values_GEO, by = "DATE") %>%
    left_join(Available_values_GEO_SER, by = "DATE")
  
  write.csv(df_plot, paste0("Data_plots/Available_Stations_", VAR,".csv"), row.names = FALSE)
  
  df_plot <- pivot_longer ( df_plot,
                            cols = starts_with("available_values"),
                            names_to= "Filter_level",
                            values_to= "station_num")
  
  df_plot$Filter_level <- as.factor(df_plot$Filter_level)
  df_plot$Filter_level <- gsub("available_values", "filter", df_plot$Filter_level)
  
  p <- ggplot(df_plot, aes(x = DATE, y = station_num, color = Filter_level)) +
    geom_line() +
    labs(
      title = "Available values after geographic and time series filter",
      x = "Date",
      y = "Number of stations",
      color = "Legend"
    ) +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(c(min(df_plot$DATE), max(df_plot$DATE))) 
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  ggsave (paste0("Data_plots/Available_Stations_", VAR,".png"), plot=p, width = 30, height = 5, dpi = 200)
  
}

Plot_station_number_filtering (PCP_ROI_weather_data, PCP_data, PCP_data3, "PCP")
Plot_station_number_filtering (MIN_ROI_weather_data, MIN_data, MIN_data3, "MIN")
Plot_station_number_filtering (MAX_ROI_weather_data, MAX_data, MAX_data3, "MAX")
Plot_station_number_filtering (SLR_ROI_weather_data, SLR_data, SLR_data3, "SLR")
Plot_station_number_filtering (WND_ROI_weather_data, WND_data, WND_data3, "WND")
Plot_station_number_filtering (HMD_ROI_weather_data, HMD_data, HMD_data3, "HMD")


#Plot maps with stations distribution. 
install.packages("patchwork")  # only once
library(patchwork)

plot_stationlocations <- function(thesaurus, thesaurus_GeoFilt, thesaurus_GeoLenghtFILT, VAR) {
  p1 <- ggplot() +
    geom_sf(data = ROI_sf, fill = "white", color = "gray70") +
    geom_sf(data = thesaurus , color = "blue", size = 2) +
    coord_sf(crs = 25831, expand = FALSE) +
    theme_minimal() +
    ggtitle("Original Points")
  
  p2 <- ggplot() +
    geom_sf(data = ROI_sf, fill = "white", color = "gray70") +
    geom_sf(data = thesaurus_GeoFilt , color = "red", size = 2) +
    coord_sf(crs = 25831, expand = FALSE) +
    theme_minimal() +
    ggtitle("Geographic Filter")
  
  p3 <- ggplot() +
    geom_sf(data = ROI_sf, fill = "white", color = "gray70") +
    geom_sf(data = thesaurus_GeoLenghtFILT , color = "green", size = 2) +
    coord_sf(crs = 25831, expand = FALSE) +
    theme_minimal() +
    ggtitle("Geo + Length Filter")
  
  # Combine plots side by side with a main title
  final_plot <- (p1|p2|p3) +
    plot_annotation(title = paste("Station Filtering Process for", VAR))
  
  # Save the plot
  ggsave(
    filename = paste0("Data_plots/Available_StationsLocation_", VAR, ".png"),
    plot = final_plot,
    width = 30,
    height = 6,
    dpi = 200
  )
}

plot_stationlocations(thesaurus_PCP, thesaurus_PCP_GeoFilt, thesaurus_PCP_GeoLenghtFILT, "PCP") 
plot_stationlocations(thesaurus_MIN, thesaurus_MIN_GeoFilt, thesaurus_MIN_GeoLenghtFILT, "MIN") 
plot_stationlocations(thesaurus_MAX, thesaurus_MAX_GeoFilt, thesaurus_MAX_GeoLenghtFILT, "MAX") 
plot_stationlocations(thesaurus_SLR, thesaurus_SLR_GeoFilt, thesaurus_SLR_GeoLenghtFILT, "SLR") 
plot_stationlocations(thesaurus_WND, thesaurus_WND_GeoFilt, thesaurus_WND_GeoLenghtFILT, "WND") 
plot_stationlocations(thesaurus_HMD, thesaurus_HMD_GeoFilt, thesaurus_HMD_GeoLenghtFILT, "HMD") 


#  FUNCTIONS for data sets by all variables ________________________________________________________________####

# Function to complete time series sequence with NA values. 



weather_data2 <- weather_data

# Function to identify Short time series
NApc_multivariable <- function(df) {
  df_NApc <- df %>%
    group_by(CODE) %>%
    summarise(
      NApc_PCP = mean(is.na(PCP)) * 100,
      NApc_MIN = mean(is.na(MIN)) * 100,
      NApc_MAX = mean(is.na(MAX)) * 100,
      NApc_SLR = mean(is.na(SLR)) * 100,
      NApc_WND = mean(is.na(WND)) * 100,
      NApc_HMD = mean(is.na(HMD)) * 100
    ) %>%
    ungroup()
  
  return(df_NApc)
}

NApc_2 <- NApc_multivariable(weather_data2)

write.csv(NApc, "NApc2_stations.csv")


# PLOTTING GRAPHs with thenumber of valid measure by date_________________________________________________________________________________________####

#PCP

df_inicial <- ROI_weather_data
df_geofilt <- weather_data
df_lenghtfilt <- weather_data2

Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "PCP" ) 
Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "MIN" )
Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "MAX" )
Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "SLR" )
Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "WND" )
Plot_station_number_filtering (ROI_weather_data, weather_data, weather_data2, "HMD" )



#Plot maps with stations distribution. 
library(patchwork)
VAR <- c("PCP", "MIN","MAX", "SLR", "WND", "HMD")
Weather_data_sf <- st_as_sf(weather_data,  coords = c("C_X", "C_Y"), crs = 25831)
Weather_data2_sf <- st_as_sf(weather_data2,  coords = c("C_X", "C_Y"), crs = 25831)

for (v in VAR) {
  thesaurus_inicial <- ROI_weather_data %>%
    rename(VAR_col = all_of(v)) %>%          # Use all_of for string column names
    select(VAR_col, geometry) %>%
    group_by(geometry) %>%
    mutate(mean = mean(VAR_col, na.rm = TRUE)) %>%
    filter(!is.na(mean))
  
  thesaurus_GeoFilt <- Weather_data_sf %>%
    rename(VAR_col = all_of(v)) %>%          # Use all_of for string column names
    select(VAR_col, geometry) %>%
    group_by(geometry) %>%
    mutate(mean = mean(VAR_col, na.rm = TRUE)) %>%
    filter(!is.na(mean))
  
  thesaurus_GeoLenghtFILT <- Weather_data2_sf %>%
    rename(VAR_col = all_of(v)) %>%          # Use all_of for string column names
    select(VAR_col, geometry) %>%
    group_by(geometry) %>%
    mutate(mean = mean(VAR_col, na.rm = TRUE)) %>%
    filter(!is.na(mean))
  
  plot_stationlocations(thesaurus_inicial, thesaurus_GeoFilt, thesaurus_GeoLenghtFILT, v) 
  
}


