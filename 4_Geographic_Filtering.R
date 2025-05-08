Amanda Batlle-Morera (a.batlle@creaf.uab.cat)

# 2. Geographic filtering 

# Note: Mira et al, 2017: Improving mean minimum and maximum month-to-month air temperature surfaces using satellite-derived land surface temperature.
#     - Applies a filter to eliminate stations that are close to each other.
#     - to avoid artifacts in the interpolation step due to the proximity of some stations, a criterion of minimum distance (1 km) was applied. 
#     - The criteria used to choose one of the stations closer than 1 km was based on:
#                     - Fecha Entrada: 01/10/24
#                       Asunto: Medidas de calidad para la red de estaciones meteorológicas
#                       Buenas tardes, Por lo general, si las estaciones están a 0 metros de distancia, es que están en el mismo jardín meteorológico, siendo una la manual y la otra la automática. Hay que elegir siempre la automática. Se distingue porque tiene como tipo A. Si son dos estaciones, pero no están a 0 metros de distancia o tienen nombres manifiestamente diferentes, elijase siempre la automática. En el caso de que las dos estaciones sean manuales, no hay información precisa de cuál es la más fiable. Consultar cada caso con maldanae@aemet.es 

#                     - the series that was longest and most robust.



# Install libraries
# Require packages 
requiredPackages <- c("readr", "sf", "tibble", "dplyr", "tidyr","ggplot2", "lubridate", "purrr" ) # "readr", "lubridate"
# Check and install only missing packages
install.packages(setdiff(requiredPackages, rownames(installed.packages())), depenencies = TRUE)
# Load these packages 
lapply(requiredPackages, library, character.only = TRUE)


# Functions for the Geographic filtering _________________________________________####
# CALCULATE DISTANCE BETWEEN STATIONS 
Distance_between_stations <- function (Thesaurus_sf, VAR, df) {
  
  # 1. Calculate pairwise distances between stations
  dist_matrix <- st_distance(Thesaurus_sf)
  
  # 2. Convert distance matrix to a dataframe and find pairs closer than 1 km
  dist_df <- as.data.frame(as.matrix(dist_matrix))  # Convert to a data frame
  names(dist_df) <- Thesaurus_sf$CODE  # Assign station IDs as column names
  dist_df$CODE <- Thesaurus_sf$CODE  # Add station IDs as row names
  
  
  #Create folder
  output_dir <- paste0("GeographicFilter/", VAR)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write_csv(dist_df, paste0(output_dir, "/DistanceMatrix_", VAR, ".csv"))
  
  # 3. Find station pairs with distance < 3000 m (excluding self-pairs)
  dist_df_long <- dist_df %>%
    pivot_longer(-CODE, names_to = "station2", values_to = "distance") %>%
    mutate(distance = as.numeric(distance)) %>% 
    filter(distance < 3000 & CODE != station2) %>% # Keep only unique combinations of station1 and station2# Exclude self-pairs, and 'close_stations' will contain the pairs of stations that are closer than 1 km
    rowwise() %>%
    mutate(
      station1 = pmin(CODE, station2),  # Always keep the smaller station code as station1
      station2 = pmax(CODE, station2)) %>%  # The larger code becomes station2
    ungroup() %>%
    distinct(station1, station2, .keep_all = TRUE) 
  
  # 5. add temporal data
  time_range1 <- Thesaurus_sf %>%
    st_drop_geometry() %>%
    select(CODE, NAME_STATION, ALT) %>%
    rename(station1 = CODE,
           NAME_STATION_station1 = NAME_STATION,
           ALT_station1 = ALT)
  
  time_range2 <- Thesaurus_sf %>%
    st_drop_geometry() %>%
    select(CODE, NAME_STATION, ALT) %>%
    rename(station2 = CODE,
           NAME_STATION_station2 = NAME_STATION,
           ALT_station2 = ALT)
  
  dist_df_long_2 <- dist_df_long %>%
    left_join(time_range1, by = "station1") %>%
    left_join(time_range2, by = "station2")
  
  write_csv(dist_df_long_2, paste0(output_dir, "/GeograStationNearbyEachOther_", VAR, ".csv"))
  
  # 5. Prepare data for plotting
  df_subset <- df %>%
    select(DATE, CODE, ALT, all_of(VAR))
  
  #6. Generate comparison plots
  for (i in 1:nrow(dist_df_long_2)) {
    station1 <- dist_df_long_2$station1[i]
    station2 <- dist_df_long_2$station2[i]
    
    # Filter df2 for the given station1 and station2
    df_filtered <- df_subset %>%
      filter(CODE%in% c(station1, station2))
    
    
    # Plot TotalPCP values
    p <- ggplot(df_filtered, aes(x = DATE, y = .data[[VAR]], fill = factor(CODE))) +
      geom_col(position="dodge") +
      labs(title = paste(VAR, "for stations:", station1, "and", station2),
           x = "Date",
           y = VAR,
           fill ="Stations") +
      theme_minimal()+
      theme(legend.position = "bottom")  # Move legend below
    ggsave(
      filename = paste0(output_dir, "/", station1, "_", station2, ".png"),
      plot = p, width = 30, height = 5, dpi = 200
    )
    
  }
}

# PLot neighboring stations neighbor 

plot_neighborstations <- function (df, stationset, neighbor_stations, VAR, csv_output) {
  output_dir <- paste0("GeographicFilter/", VAR)
  #subset dataset
  df_filtered <- df%>%
    filter(CODE %in% c(stationset, neighbor_stations))
  
  # First: calculate actual range of your DATE column (safely)
  min_date <- min(df_filtered$DATE, na.rm = TRUE)
  max_date <- max(df_filtered$DATE, na.rm = TRUE)
  
  # Now add limits explicitly in your plot
  p <- ggplot(df_filtered, aes(x = DATE, y = .data[[VAR]], fill = factor(CODE))) +
    geom_col(position = "dodge") +
    labs(
      title = paste(VAR, "for stations CASE", stationset[1], stationset[2]),
      x = "Date",
      y = VAR,
      fill = "Stations"
    ) +
    theme_minimal() +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "3 month",
      limits = c(min_date, max_date)  # This trims to the actual data
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  ggsave (paste0(output_dir, "/", stationset[1], "_", stationset[2], "_neighbours.png"), plot=p, width = 40, height = 4, dpi = 200)
  
  if (csv_output==TRUE){
    df_filtered %>%
      select(DATE, CODE, all_of(VAR)) %>%
      pivot_wider(
        names_from = CODE,
        values_from = all_of(VAR)
      ) %>%
      write_csv(paste0(output_dir, "/", stationset[1], "_", stationset[2], "_neighbours.csv"))
  }
  
}

# Merge station 

MergeStations <- function (df, stationset, new_code, Thesaurus, VAR, csv_output) {
  if (length(stationset) < 2) {
    stop("stationset must contain at least 2 station codes.")
  }
  # --- Check if all station codes exist in the dataset --
  missing_codes <- stationset[!stationset %in% unique(df$CODE)]
  if (length(missing_codes) > 0) {
    stop(paste("ERROR: These station codes were not found in the dataset:", paste(missing_codes, collapse = ", ")))
  }
  
  # --- Subset data for selected stations ---
  df_filtered <- df %>% 
    filter(CODE %in% stationset)%>%
    select(DATE, CODE, all_of(VAR))
  
  
  # Pivot to wide format by station
  df_wide <- df_filtered %>%
    pivot_wider(names_from = CODE, values_from = all_of(VAR))%>% 
    select(DATE, all_of(stationset) )
  
  # Create merged VAR column using coalesce  var_cols <- stationset
  df_wide <- df_wide %>%
    mutate(!!VAR := coalesce(!!!syms(stationset))) %>% # Retains de first non-NA value in the same order of priority as in the vector. 
    select(DATE, all_of(VAR))
  
  # Set new metadata
  avg_coords <- Thesaurus %>%
    filter(CODE %in% stationset) %>%
    summarise(C_X = mean(C_X, na.rm = TRUE),
              C_Y = mean(C_Y, na.rm = TRUE),
              ALT = mean(ALT, na.rm = TRUE))
  
  station_names <- Thesaurus %>%
    filter(CODE %in% stationset) %>%
    pull(NAME_STATION) %>%
    paste(collapse = "-")
  
  # Build new station dataset
  df_newstation <- df_wide %>%
    mutate(CODE = new_code,
           NAME_STATION = paste("Fusion", station_names),
           ALT = avg_coords$ALT,
           C_X = avg_coords$C_X,
           C_Y = avg_coords$C_Y,
           source = "merged data") %>%
    select(DATE, CODE, NAME_STATION, ALT, C_X, C_Y, all_of(VAR), source)
  
  if (csv_output==TRUE){
    # Save new merged dataset (optional)
    output_dir <- paste0("GeographicFilter/", VAR)
    write.csv(df_newstation,
              file =paste0(output_dir, "/",VAR,"_", new_code, ".csv"),
              row.names = FALSE)
  }
  
  # Replace in original dataset
  df_out <- df %>%
    filter(!CODE %in% stationset) %>%
    bind_rows(df_newstation)
  
  return(df_out)
  
}


# GEOGRAPHIC FILETERING WITH ALL THE VARIABLES AT THE SAME TIME _____________####

#Merge Variables for all stations
weather_data <- ROI_weather_data %>%
  st_drop_geometry()

MergeStations_multivariable <- function (df, stationset, new_code, Thesaurus, csv_output=FALSE) {
  if (length(stationset) < 2) {
    stop("stationset must contain at least 2 station codes.")
  }
  # --- Check if all station codes exist in the dataset --
  missing_codes <- stationset[!stationset %in% unique(df$CODE)]
  if (length(missing_codes) > 0) {
    stop(paste("ERROR: These station codes were not found in the dataset:", paste(missing_codes, collapse = ", ")))
  }
  
  variable_list <- c("PCP", "MIN","MAX", "SLR", "WND", "HMD")
  
  df_var_merge <- data.frame(DATE = seq(from = as.Date("2000-01-01"), to = as.Date("2022-12-31"), by = "day"))
  
  # --- Subset data for selected stations ---
  df_filtered <- df  %>% 
    filter(CODE %in% stationset)
  
  for (VAR in variable_list) {
    # --- Subset data for selected stations ---
    df_var <- df_filtered %>% 
      select(DATE, CODE, all_of(VAR))
    
    
    # Pivot to wide format by station
    df_wide <- df_var %>%
      pivot_wider(names_from = CODE, values_from = all_of(VAR))%>% 
      select(DATE, all_of(stationset) )
    
    # Create merged VAR column using coalesce  var_cols <- stationset
    df_wide <- df_wide %>%
      mutate(!!VAR := coalesce(!!!syms(stationset))) %>% # Retains de first non-NA value in the same order of priority as in the vector. 
      select(DATE, all_of(VAR))
    
    df_var_merge <- df_var_merge %>% left_join (df_wide , by="DATE") 
  }
  
  
  # Set new metadata
  avg_coords <- Thesaurus %>%
    filter(CODE %in% stationset) %>%
    summarise(C_X = mean(C_X, na.rm = TRUE),
              C_Y = mean(C_Y, na.rm = TRUE),
              ALT = mean(ALT, na.rm = TRUE))
  
  
  
  # Build new station dataset
  df_newstation <- df_var_merge %>%
    mutate(CODE = new_code,
           NAME_STATION = paste("Fusion", paste(stationset, collapse = "-")),
           ALT = avg_coords$ALT,
           C_X = avg_coords$C_X,
           C_Y = avg_coords$C_Y,
           source = "merged data") %>%
    select(DATE, CODE, NAME_STATION, ALT, source, C_X, C_Y, all_of(variable_list))
  
  if (csv_output==TRUE){
    # Save new merged dataset (optional)
    output_dir <- paste0("GeographicFilter")
    write.csv(df_newstation,
              file =paste0(output_dir, "/", new_code, ".csv"),
              row.names = FALSE)
  }
  
  # Replace in original dataset
  df_out <- df %>%
    filter(!CODE %in% stationset) %>%
    bind_rows(df_newstation)
  
  return(df_out)
  
} 

#Geographic filtering merging station of different variables: 
Distance_between_stations_multivar <- function (Thesaurus_sf, VAR, df) {
  
  # 1. Calculate pairwise distances between stations
  dist_matrix <- st_distance(Thesaurus_sf)
  
  # 2. Convert distance matrix to a dataframe and find pairs closer than 1 km
  dist_df <- as.data.frame(as.matrix(dist_matrix))  # Convert to a data frame
  names(dist_df) <- Thesaurus_sf$CODE  # Assign station IDs as column names
  dist_df$CODE <- Thesaurus_sf$CODE  # Add station IDs as row names
  
  
  #Create folder
  output_dir <- "GeographicFilter/Multivar"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write_csv(dist_df, paste0(output_dir, "/DistanceMatrix_multivar.csv"))
  
  # 3. Find station pairs with distance < 1000 m (excluding self-pairs)
  dist_df_long <- dist_df %>%
    pivot_longer(-CODE, names_to = "station2", values_to = "distance") %>%
    mutate(distance = as.numeric(distance)) %>% 
    filter(distance < 3000 & CODE != station2) %>% # Keep only unique combinations of station1 and station2# Exclude self-pairs, and 'close_stations' will contain the pairs of stations that are closer than 1 km
    rowwise() %>%
    mutate(
      station1 = pmin(CODE, station2),  # Always keep the smaller station code as station1
      station2 = pmax(CODE, station2)) %>%  # The larger code becomes station2
    ungroup() %>%
    distinct(station1, station2, .keep_all = TRUE) 
  
  # 5. add temporal data
  time_range1 <- Thesaurus_sf %>%
    st_drop_geometry() %>%
    select(CODE, NAME_STATION, ALT) %>%
    rename(station1 = CODE,
           NAME_STATION_station1 = NAME_STATION,
           ALT_station1 = ALT)
  
  time_range2 <- Thesaurus_sf %>%
    st_drop_geometry() %>%
    select(CODE, NAME_STATION, ALT) %>%
    rename(station2 = CODE,
           NAME_STATION_station2 = NAME_STATION,
           ALT_station2 = ALT)
  
  dist_df_long_2 <- dist_df_long %>%
    left_join(time_range1, by = "station1") %>%
    left_join(time_range2, by = "station2")
  
  write_csv(dist_df_long_2, paste0(output_dir, "/GeograStationNearbyEachOther_multivar.csv"))
  
  # 5. Prepare data for plotting
  df_subset <- df %>%
    select(DATE, CODE, ALT, all_of(VAR))
  
  #6. Generate comparison plots
  for (i in 1:nrow(dist_df_long_2)) {
    station1 <- dist_df_long_2$station1[i]
    station2 <- dist_df_long_2$station2[i]
    
    # Filter df2 for the given station1 and station2
    df_filtered <- df_subset %>%
      filter(CODE%in% c(station1, station2))%>%
      pivot_longer(cols = VAR, 
                   names_to="variable",
                   values_to="measure")
    
    
    # Plot TotalPCP values
    p <- ggplot(df_filtered, aes(x = DATE, y = measure, fill = factor(CODE))) +
      geom_col(position="dodge") +
      facet_wrap(~ variable, scales = "free_y") +
      labs(
        title = paste("for stations:", station1, "and", station2),
        x = "Date",
        y = "Variable",
        color = "Stations"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggsave(
      filename = paste0(output_dir, "/", station1, "_", station2, ".png"),
      plot = p, width = 25, height = 25, dpi = 200
    )
    
  }
}

