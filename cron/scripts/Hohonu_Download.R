######################################
#
#   Hohonu Data Download
#
######################################
#load libraries
library(tidyverse)
library(httr2)
library(jsonlite)
library(vroom)

#Hohonu API Descriptions: https://hohonu.readme.io/reference/viewstation
#QC Flag for level 2: 
  #string is Z, X, X, X, X, X 
  # Z = roll up flag 
  #X is value for: spike test, rate of change test, flat line test, volatility test, climatology test
  # 1: pass
  # 3: suspect
  # 4: Fail
  # 9: missing 

data_dir = "/app/Data/"

source(file.path(data_dir, "Inputs/api_keys.R"))

#Device IDs 
device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
  filter(str_detect(Location, "Tide", negate = T)) %>% 
  filter(API == "Hohonu") %>% 
  filter(Active == "Yes") 

for(i in 1:dim(device_id)[1]){

  filename = paste0(data_dir, "Outputs/", device_id$API[i], "_",  device_id$Location[i], "_Data.csv")
  
  current_time = round_date(now(tzone = "UTC"), unit = "minute")
  
  #if statement to capture missing last_time values
  last_time = current_time - days(2) 
  
  #Create API url
  url <- paste0("https://dashboard.hohonu.io/api/v1/stations/hohonu-", device_id$ID[i], "/waterlevel")

  queryString <- list(
      from = as.character(last_time),
      to = as.character(current_time),
      units = "english",
      datum = "FLOOD_DEPTH",
      qc_level = "2",
      flags = "true",
      predictions = "false",
      observations = "true")

  req <- request(url) %>% 
            req_url_query(!!!queryString) %>% 
            req_headers('Authorization' = hohonu_key, 
                        "Content-Type" = "application/octet-stream",
                        "Accept" = "application/json") 
 
 response <- req_perform(req)
 

 
if (response$status_code!= 200) {
  next
} 

hohonu_download =  response %>% 
                        resp_body_json(simplifyVector = TRUE) 

if(length(hohonu_download$data$waterlevel)==0){

  next
}

hohonu_data = as.data.frame(hohonu_download$data) %>% 
                  mutate(Location = rep(device_id$Location[i]), 
                         Unit = rep(paste(hohonu_download$meta$datum$label,hohonu_download$meta$datum$unit, sep = "_" ))) %>% 
                 rename(Timestamp_UTC = waterlevel.t, 
                        Flood.Depth = waterlevel.o, 
                        QC_flag = waterlevel.f) %>% 
                 mutate(Time_ET = str_remove(str_replace(Timestamp_UTC, "T", " "), "Z"), 
                        Time_ET = as.POSIXct(Time_ET, tz = "UTC"), 
                        Time_ET = round_date(with_tz(Time_ET, tzone = "America/New_York"), unit = "minute")) %>% 
                separate_wider_delim(QC_flag, 
                                     delim = ",", 
                                     names = c("QC_Roll_Up", "QC_Spike_Test", "QC_Rate_of_Change", "QC_Flat_Line", "QC_Volatility", "QC_Climatology" )) %>% 
                distinct(Time_ET, .keep_all = T)



write.csv(hohonu_data, filename)


}


################################# Tide Data ####################################


#Device IDs 
device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
  filter(str_detect(Location, "Tide")) %>% 
  filter(API == "Hohonu") %>% 
  filter(Active == "Yes") 


for(i in 1:dim(device_id)[1]){
  
  
  filename = paste0(data_dir, "Outputs/Tide_", device_id$API[i], "_",  device_id$Location[i], "_Data.csv")
  
  current_time = round_date(now(tzone = "UTC"), unit = "minute")
  
  #if statement to capture missing last_time values
  last_time = current_time - days(2) 
  
  #Create API url
  url <- paste0("https://dashboard.hohonu.io/api/v1/stations/hohonu-", device_id$ID[i], "/waterlevel")
  
  queryString <- list(
    from = as.character(last_time),
    to = as.character(current_time),
    units = "english",
    datum = "NAVD",
    qc_level = "2",
    flags = "true",
    predictions = "false",
    observations = "true")
  
  req <- request(url) %>% 
    req_url_query(!!!queryString) %>% 
    req_headers('Authorization' = hohonu_key, 
                "Content-Type" = "application/octet-stream",
                "Accept" = "application/json") 
  
  response <- req_perform(req)
  
  
  
  if (response$status_code!= 200) {
    next
  }
  
  
  hohonu_download =  response %>% 
    resp_body_json(simplifyVector = TRUE) 
  
  if(length(hohonu_download$data$waterlevel)==0){
  
    next
  }

  hohonu_data = as.data.frame(hohonu_download$data) %>% 
    mutate(Location = rep(device_id$Location[i]), 
           Unit = rep(paste(hohonu_download$meta$datum$label,hohonu_download$meta$datum$unit, sep = "_" ))) %>% 
    rename(Timestamp_UTC = waterlevel.t, 
           Flood.Depth = waterlevel.o, 
           QC_flag = waterlevel.f) %>% 
    mutate(Time_ET = str_remove(str_replace(Timestamp_UTC, "T", " "), "Z"), 
           Time_ET = as.POSIXct(Time_ET, tz = "UTC"), 
           Time_ET = round_date(with_tz(Time_ET, tzone = "America/New_York"), unit = "minute")) %>% 
    separate_wider_delim(QC_flag, 
                         delim = ",", 
                         names = c("QC_Roll_Up", "QC_Spike_Test", "QC_Rate_of_Change", "QC_Flat_Line", "QC_Volatility", "QC_Climatology" )) %>% 
    distinct(Time_ET, .keep_all = T)
  
  
  
  write.csv(hohonu_data, filename)
  
  
}


