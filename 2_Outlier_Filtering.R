# Amanda Batlle-Morera (a.batlle@creaf.uab.cat)

# 2. Outlier filtering : 
# Functions to identify and remove outliers


# Install libraries
# Require packages 
requiredPackages <- c("readr", "dplyr", "tidyr", "lubridate", "sf", "ggplot2" )
# Check and install only missing packages
install.packages(setdiff(requiredPackages, rownames(installed.packages())), depenencies = TRUE)
# Load these packages 
lapply(requiredPackages, library, character.only = TRUE)


#Removing potential data issues: ___________________________________________________#### 

# HUMIDITY
    # Removing HMD values under 20 
    # Remove rows where geometries are empty or HMD is NA
    valid_data <- ROI_weather_data[!st_is_empty(ROI_weather_data) & !is.na(ROI_weather_data$HMD), ]
    
    # Check for cases where HMD < 20
    invalid_cases <- valid_data[valid_data$HMD < 20, ]
    
    # Print results
    if (nrow(invalid_cases) > 0) {
      print(invalid_cases)
    } else {
      print("No cases where HMD < 20.")
    }
    
    # Replace HMD values < 20 with NA
    ROI_weather_data <- ROI_weather_data %>%
      mutate(HMD = if_else(HMD < 20, NA_real_, HMD))
    
# TEMPERATURE 
    # Check for cases where MIN > MAX in valid rows
    # Remove rows where geometries are empty or have NA values
    valid_data <- ROI_weather_data[!st_is_empty(ROI_weather_data$geometry) & !is.na(ROI_weather_data$MIN) & !is.na(ROI_weather_data$MAX), ]
    
    # Now check for cases where MIN > MAX in valid rows
    invalid_cases <- valid_data[valid_data$MIN > valid_data$MAX, ]
    
    # If there are invalid cases, print them
    if (nrow(invalid_cases) > 0) {
      print(invalid_cases)
    } else {
      print("No cases where MIN > MAX.")
    }
    
# PRECIPITATION
    # Check for negative PCP values
    # Remove rows where geometries are empty or have NA values
    valid_data <- ROI_weather_data[!st_is_empty(ROI_weather_data$geometry) & !is.na(ROI_weather_data$PCP), ]
    
    negative_pcp <-  valid_data[ valid_data$PCP < 0, ]
    
    # If there are negative PCP values, print them
    if (nrow(negative_pcp) > 0) {
      print(negative_pcp)
    } else {
      print("No negative PCP values.")
    }
    
# SOLAR RADIATION
    # Check for negative SLR values
    # Remove rows where geometries are empty or have NA values
    valid_data <- ROI_weather_data[!st_is_empty(ROI_weather_data$geometry) & !is.na(ROI_weather_data$SLR), ]
    
    negative_slr <-  valid_data[ valid_data$SLR < 0, ]
    
    # If there are negative SLR values, print them
    if (nrow(negative_slr) > 0) {
      print(negative_slr)
    } else {
      print("No negative SLR values.")
    }

# WIND SPEED 
    # Check for negative WND values
    # Remove rows where geometries are empty or have NA values
    valid_data <- ROI_weather_data[!st_is_empty(ROI_weather_data$geometry) & !is.na(ROI_weather_data$WND), ]
    
    negative_wnd <-  valid_data[ valid_data$WND < 0, ]
    
    # If there are negative SLR values, print them
    if (nrow(negative_wnd) > 0) {
      print(negative_wnd)
    } else {
      print("No negative SLR values.")
    }
    
#FUNTION TO IDENTIFY OUTLIERS_________________________________________________####
    
  # This function will :
  #     1. calculate the monthly mean and sd for each station.
  #     2. Shortlist the values that are outside of the considered extreme values outside of the range of mean monthly value +- (N_SD *sd) 
  # Inputs: 
    # df: dataframe with daily data
    # VAR: variable to evaluate
    # N_SD: number of sd to consider extrem values
  # Outputs: 
    # df_outlier_id : dataframe with the short list of potencial outliers
    # csv file: short list of potencial outliers saved in paste0("Data_plots/Daily_outliers/", VAR,"/Daily_potencial_outliers_", VAR, ".csv")
    outlier_highlight <- function (df, VAR, N_SD) {
      df <- df %>%
        mutate(MONTH = month(DATE))  # This gives a numeric month (1–12)
      
      monthly_stats <- df %>%
        group_by(MONTH, CODE) %>%
        summarise(
          climMEAN= mean(.data[[VAR]], na.rm = TRUE),
          climSD = sd(.data[[VAR]], na.rm = TRUE)) %>%
        ungroup()
      
      monthly_stats_extrems <- monthly_stats%>%
        mutate(ext_inf = climMEAN-(N_SD*climSD),
               ext_sup = climMEAN+(N_SD*climSD)
        )
      
      df_outlier_id <- left_join(df, monthly_stats_extrems, by = c("CODE", "MONTH"))
      
      #Subset outliers
      df_outlier_id <- df_outlier_id %>%
        filter(.data[[VAR]] <= ext_inf | .data[[VAR]] >= ext_sup)
      
      if (nrow(df_outlier_id) == 0) {
        message(paste("No outliers detected for variable", VAR))
        return(NULL)
      }
      #Create folder
      output_dir <- paste0("Data_plots/Daily_outliers/", VAR)
      
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      write.csv(df_outlier_id , paste0("Data_plots/Daily_outliers/", VAR,"/Daily_potencial_outliers_", VAR, ".csv"))
      return (df_outlier_id)
    }

    
# Generate plots of the potencial outliers ___________________________________________####
    outlier_plot <- function (df,df_outlier_id, thesaurus, VAR ) {
      #Check for values that are too high and plot to compare with neibouring stations  
      #Based on Functions from # Eirini Trypidaki- e.trypidaki@creaf.uab.cat
      
      # Iterate over the rows of the dataframe
      
      for (i in 1:nrow(df_outlier_id)) {
        outlier_id <- df_outlier_id$CODE[i]
        outlier_station <- all_stations_sf[all_stations_sf$CODE == outlier_id, ] 
        buffer_distance <- 1000
        
        # Repeat until at least 3 stations are found within the buffer
        while (TRUE) {
          # Create a buffer around the outlier station
          outlier_buffer <- st_buffer(outlier_station, buffer_distance)
          
          # Find stations that are within the buffer
          within_indices <- st_within(all_stations_sf, outlier_buffer, sparse = FALSE)
          stations_within <- all_stations_sf[within_indices, ]
          
          # Add the list of stations within buffer to the result list
          stations_within_buffer <- stations_within$CODE
          
          # Check if at least 3 stations are found within the buffer
          if (nrow(stations_within) >= 5) {
            break  # Exit the loop
          } else {
            # Increase the buffer distance
            buffer_distance <- buffer_distance + 1000  # Increase by 1000 m
          }
        }
        
        # Filter df for shortlisted stations and the specific month
        target_date <- df_outlier_id$DATE[i]
        selected_df <- df %>%
          filter(CODE %in% stations_within_buffer) %>% # Filter stations
          filter(DATE >= (target_date - days(15)) & DATE <= (target_date + days(15))) %>% # Filter 15 days up and down of the potencial outlier 
          mutate(StationID_Altitude = paste(CODE, ",", ALT, "m")) # Get unique stations with their altitudes
        
        # Plot time series for each selected station
        p <- ggplot(selected_df, aes(x = DATE, y =.data[[VAR]], color = StationID_Altitude)) +
          geom_point() +  # Plot points
          geom_line() +   # Connect points with lines
          labs(title = paste("Potencial outlier:",outlier_id, target_date ),
               subtitle = paste(buffer_distance, "m buffer distance"),
               x = "Day", y = VAR,
               color = "StationID_Altitude") +
          theme_minimal()  # Minimal theme
        ggsave(paste0("Data_plots/Daily_outliers/", VAR, "/", outlier_id, "_", target_date, "_", VAR ,"DegreesC_DayOutlierInspection.png"), plot = p, width = 7, height = 3)
        
      }
    }
    
# FUNCTION TO REMOVE OUTLIERS ________________________________________________####
    # This function will remove manually confirmed outliers from the dataset.
    # Inputs:
      # df_outlier_id : dataframe with the short list of potencial outliers
      # df: dataframe with daily data
      # outliers: vector of index positions on the df_outlier_id that correspond to TRUE outliers and need to be removed

    # Outputs: 
      # df_filteroutlier : dataframe without outliers
    remove_outliers <- function (df_outlier_id , df, outliers) {
      for (i in outliers) {
        outlier_id <- df_outlier_id$CODE[i]
        target_date <- df_outlier_id$DATE[i]
        
        df_filteroutlier<- df %>%
          filter(!(CODE_INM==outlier_id & DATE==target_date))# Filter station
      }
      return(df_filteroutlier)
    } 
    
    
