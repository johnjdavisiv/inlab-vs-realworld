#Process FIT data from garmin devices
# (suggested: use a different library, this is a slow function)


library(tidyverse)
library(lubridate)

#file_name is a raw .csv file extracted from a .FIT file, by the java script in the FIT SDK

process_fit_data <- function(file_name, base_path){
  print("Parsing failures are expected, do not worry...")
  full_file <- paste(base_path, file_name, sep="")
  save_file_name <- sub(".csv", "_processed.csv", full_file)
  
  #For fixing timestamps: Garmin "time zero" is a weird one
  garmin_t_zero <- as.POSIXct("1989-12-31 00:00:00",tz="UTC")
  
  #Read file
  garmin_csv <- read_csv(full_file, col_names=TRUE, col_types = cols(.default = "c"))
  #Yeah yeah, parsing failures...
  
  #Get a list of all the data types we collected from the session
  garmin_data_fields <- unique(garmin_csv$Message)
  
  #Separate the data into several different data frames
  garmin_list <- list()
  
  for (i in unique(garmin_csv$Message)){
    garmin_list[[i]] <- garmin_csv %>% 
      filter(Message == i) %>% 
      select_if(~!all(is.na(.)))
  }
  
  stryd_field_descriptions <- garmin_list$field_description
  
  #To see all of the data types:
  #names(garmin_list)
  
  #Get all record data (the record column has "data" and 'definitions')
  record_data <- garmin_list$record %>% 
    filter(Type == "Data")
  
  
  #Get all unique record fields as a character vector
  all_data_fields <- record_data %>%
    select(starts_with("Field")) %>%
    pivot_longer(everything(), names_to = "name", values_to = "Field") %>%
    filter(!is.na(Field), Field != "unknown") %>%
    select(Field) %>%
    unique()
  
  #Do we have stryd data? if no power field, guess not...
  have_stryd_data <- "Leg Spring Stiffness" %in% all_data_fields$Field 
  have_hrm_data <- "stance_time_percent" %in% all_data_fields$Field
  have_gps_data <- "position_lat" %in% all_data_fields$Field
  
  #Loop through every record observation
  all_record_list <- list()
  
  #This is surprisingly sluggish
  for (i in 1:nrow(record_data)) {
    if (i %% 50 == 0) print(i)
    #Get this row's data in "long" format
    this_row_long <- record_data[i,] %>%
      pivot_longer(starts_with(c("Field", "Value", "Units")), 
                   names_to = c("field_type", "field_number"), 
                   values_to = "value", names_sep = " ") %>%
      pivot_wider(-(1:3), names_from = field_type, values_from = value) %>%
      mutate(Field = ifelse(is.na(Field), "unknown", Field)) %>%
      filter(Field != "unknown")
    
    #Join with a standardized table of all possible fields
    this_row_record <- all_data_fields %>%
      left_join(this_row_long, by = "Field") %>%
      select(Field, Value) %>%
      pivot_wider(names_from = "Field", values_from = "Value")
    #Store in list
    all_record_list[[i]] <- this_row_record
  }
  
  #Will also need to detect whether this is an inside or outside run 
  #because that affects the fields we have 
  if (have_stryd_data & have_hrm_data) {
    final_garmin_data <- bind_rows(all_record_list) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(date_time = garmin_t_zero + seconds(timestamp),
             elapsed_seconds = timestamp - first(timestamp)) %>%
      mutate(posix_timestamp = timestamp + as.double(garmin_t_zero)) %>%
      rename(hrm_vertical_oscillation = vertical_oscillation,
             hrm_stance_time_percent = stance_time_percent,
             hrm_stance_time = stance_time,
             hrm_vertical_ratio = vertical_ratio,
             hrm_stance_time_balance = stance_time_balance,
             hrm_step_length = step_length,
             hrm_heart_rate = heart_rate,
             hrm_cadence = cadence,
             hrm_fractional_cadence = fractional_cadence,
             stryd_power = Power,
             stryd_cadence = Cadence,
             stryd_stance_time = `Ground Time`,
             stryd_vertical_oscillation = `Vertical Oscillation`,
             stryd_air_power = `Air Power`,
             stryd_form_power = `Form Power`, 
             stryd_leg_spring_stiffness = `Leg Spring Stiffness`) %>%
      mutate(hrm_cadence_sum = hrm_cadence + hrm_fractional_cadence) %>%
      select(date_time, elapsed_seconds, posix_timestamp, timestamp, 
             distance, enhanced_speed, 
             #enhanced_altitude, #Doesn't exist in treadmill run mode
             #position_lat, position_long, #ditto
             starts_with("hrm"), starts_with("stryd"), everything())
    
  } else if (!have_stryd_data & have_hrm_data) {
    #If you don't have stryd pod data, treat hrm data normally but assign NA to strd variables
    final_garmin_data <- bind_rows(all_record_list) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(date_time = garmin_t_zero + seconds(timestamp),
             elapsed_seconds = timestamp - first(timestamp)) %>%
      mutate(posix_timestamp = timestamp + as.double(garmin_t_zero)) %>%
      rename(hrm_vertical_oscillation = vertical_oscillation,
             hrm_stance_time_percent = stance_time_percent,
             hrm_stance_time = stance_time,
             hrm_vertical_ratio = vertical_ratio,
             hrm_stance_time_balance = stance_time_balance,
             hrm_step_length = step_length,
             hrm_heart_rate = heart_rate,
             hrm_cadence = cadence,
             hrm_fractional_cadence = fractional_cadence) %>%
      mutate(hrm_cadence_sum = hrm_cadence + hrm_fractional_cadence) %>%
      mutate(stryd_power = NA,
             stryd_cadence = NA,
             stryd_stance_time = NA,
             stryd_vertical_oscillation = NA,
             stryd_air_power = NA,
             stryd_form_power = NA, 
             stryd_leg_spring_stiffness = NA) %>%
      select(date_time, elapsed_seconds, posix_timestamp, timestamp,
             distance, enhanced_speed, 
             #enhanced_altitude, #Doesn't exist in treadmill run mode?
             #position_lat, position_long, #ditto
             starts_with("hrm"), starts_with("stryd"), everything())
  } else if (!have_hrm_data & have_stryd_data){
    #Else we have ONLY stryd data - Need to add activity_type = 1 else will fail! 
    final_garmin_data <- bind_rows(all_record_list) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(date_time = garmin_t_zero + seconds(timestamp),
             elapsed_seconds = timestamp - first(timestamp)) %>%
      mutate(activity_type = 1) %>%
      mutate(posix_timestamp = timestamp + as.double(garmin_t_zero)) %>%
      rename(stryd_power = Power,
             stryd_cadence = Cadence,
             stryd_stance_time = `Ground Time`,
             stryd_vertical_oscillation = `Vertical Oscillation`,
             stryd_air_power = `Air Power`,
             stryd_form_power = `Form Power`, 
             stryd_leg_spring_stiffness = `Leg Spring Stiffness`) %>%
      mutate(hrm_vertical_oscillation = NA,
             hrm_stance_time_percent = NA,
             hrm_stance_time = NA,
             hrm_vertical_ratio = NA,
             hrm_stance_time_balance = NA,
             hrm_step_length = NA, 
             hrm_heart_rate = heart_rate,
             hrm_cadence = cadence,
             hrm_fractional_cadence = fractional_cadence) %>%
      #Slight misuse here because heart_rate and cadence are actually coming from watch if you have no HRM
      mutate(hrm_cadence_sum = hrm_cadence + hrm_fractional_cadence) %>%
      select(date_time, elapsed_seconds, posix_timestamp, timestamp, 
             distance, enhanced_speed, 
             starts_with("hrm"), starts_with("stryd"), everything())
    
    
  } else {
    #Raise error (this should never happen...)
    stop("This run does not have HRM or Stryd data!")
  }
  
  if (!have_gps_data) {
    final_garmin_data <- final_garmin_data %>% 
      mutate(position_lat = NA,
             position_long = NA,
             enhanced_altitude = NA)
  }
  
  final_garmin_data <- final_garmin_data %>%
    mutate(have_gps_data = ifelse(have_gps_data, 1, 0),
           have_hrm_data = ifelse(have_hrm_data, 1, 0),
           have_stryd_data = ifelse(have_stryd_data,1,0)) %>%
    select(date_time, elapsed_seconds, posix_timestamp, timestamp, 
           distance, enhanced_speed, enhanced_altitude,
           position_lat, position_long, 
           starts_with("have"), starts_with("hrm"), starts_with("stryd"),
           activity_type)
  
  #Uncomment to save csv - programmatically generate filename later...
  #write_csv(final_garmin_data, save_file_name)
  print("CSV file returned! Do not worry about the warning, everything is fine.")
  return(final_garmin_data)
}
