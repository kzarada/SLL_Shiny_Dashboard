######################################
#
#   LiCOR Data Download
#
######################################
#load libraries
library(tidyverse)
library(httr2)
library(jsonlite)
library(vroom)


data_dir = "/app/Data/"

source("scripts/api_keys.R")

#Device IDs 
device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
  filter(API == "LiCOR")

#Parameters of interest
parms = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_Parameters.csv")) %>% 
  filter(API == "LiCOR") %>% 
  mutate(Parm_Unit = paste0(Parameter, "_", Units)) %>% 
  dplyr::select(Parameter.ID, Parm_Unit) 
  

#File for saving data
filename = paste0(data_dir, "Outputs/LiCOR_Rainsford_Island_Data.csv")


for(i in 1:dim(device_id)[1]){
  
  last_time = vroom(filename, col_select = timestamp, col_types = c(timestamp = "T")) %>%
    pull() %>%
    last()
  current_time = now(tzone = "UTC")
  
  #if statement to capture missing last_time values
  if(is.na(last_time) | !exists("last_time")){ 
    last_time = current_time - days(1)}
  
  last_time = str_replace_all(str_replace(as.character(round_date(last_time, unit = "minute")), " ", "%20"), ":", "%3A") 
  
  if(str_detect(last_time, ":00", negate = TRUE)){
    last_time = paste0(last_time, "%2000%3A00%3A00")}
  
  current_time = str_replace_all(str_replace(as.character(round_date(current_time, unit = "minute")), " ", "%20"), ":", "%3A")
  
  #API URL
  url = paste0("https://api.licor.cloud/v1/data?loggers=", device_id$ID[1], 
               "&start_date_time=", last_time, "&end_date_time=", current_time)
  
  req <- request(url) %>% 
            req_auth_bearer_token(licor_key)
    
  response <- req_perform(req)
  
  #log file 
  log_con <- file("/app/scripts/logs/licor_log.txt", open = "a")
  

  if (response$status_code!= 200) {
    cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
               " to: ", current_time, " failed with error ", resp_status_desc(response), " at ", Sys.time()), file = log_con, sep = "\n")
    next
  } else{cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
                    " to: ", current_time, " succeeded at ", Sys.time()), file = log_con, sep = "\n")}
  
  close(log_con)
  #Pull data and make dataframe
  api_data = response %>% 
    resp_body_json(simplifyVector = TRUE) 
  
  api_df = as.data.frame(api_data$data) %>% 
    mutate(timestamp = str_remove(timestamp, "Z$"), 
           timestamp= as.POSIXct(timestamp, tz = "UTC")) %>% 
    mutate(Time_ET = round_date(with_tz(timestamp, tzone = "America/New_York"), unit = "minute")) %>% 
    left_join(parms, by = c("sensor_sn" = "Parameter.ID")) %>% 
    dplyr::select(timestamp, value, Parm_Unit, Time_ET) %>% 
    drop_na(Parm_Unit) %>% 
    pivot_wider(names_from = Parm_Unit, 
                values_from = value)


  
  write.table(api_df,
              file = filename,
              sep = ",",
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = TRUE)
  
  #write.csv(api_df, filename)
}


