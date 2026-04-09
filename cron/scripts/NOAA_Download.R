######################################
#
#   NOAA Data Download
#
######################################
#load libraries
library(tidyverse)
library(httr2)
library(jsonlite)
library(vroom)

data_dir = "/app/Data/"

############ NOAA TIDE ##################

device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
  filter(API == "NOAA") %>% 
  filter(Active == "Yes")


for(i in 1:dim(device_id)[1]){
  
 
  filename = paste0(data_dir, "Outputs/", device_id$API[i], "_",  device_id$Location[i], "_Data.csv")
  
  
  last_time = vroom(filename, col_select = Time_ET, col_types = c(Time_ET= 'T')) %>% 
    pull() %>% 
    last() %>% 
    force_tz(tzone = "America/New_York")
  
  current_time = as.Date(now(tzone = "America/New_York"))
  
  #if statement to capture missing last_time values
  if(is.na(last_time) | !exists("last_time")){ 
    last_time = current_time - days(1)}
  
  if(str_detect(last_time, ":00", negate = TRUE)){
    last_time = paste0(last_time, "00:00:00")}
  
  current_time = str_remove_all(current_time, "-")
  last_day = str_remove_all(as.Date(last_time), "-")
  
  #API URL
  url = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=", 
               last_day, "&end_date=", current_time, "&station=", device_id$ID[i], 
               "&product=water_level&datum=MLLW&time_zone=lst_ldt&units=english&format=json")
  
  req <- request(url)
  
  response <- req_perform(req)
  
  #log file 
  log_con <- file("/app/scripts/logs/noaa_log.txt", open = "a")
  
  if (response$status_code!= 200) {
    cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
               " to: ", current_time, " failed with error ", resp_status_desc(response), " at ", Sys.time()), file = log_con, sep = "\n")
    next
    
  } else{cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
                    " to: ", current_time, " succeeded at ", Sys.time()), file = log_con, sep = "\n")}
  
  close(log_con)
  
  api_data = response %>% 
    resp_body_json(simplifyVector = TRUE) 
  

  api_data_clean = api_data$data %>% 
                      rename(Time_ET = t, 
                             Water_MLLW = v) %>% 
                      dplyr::select(Time_ET, Water_MLLW) %>% 
                    filter(Time_ET > last_time) %>% 
                    arrange(Time_ET)
  
  if(dim(api_data_clean)[1] == 0){next}
  
  #write.csv(api_data_clean, filename)
  write.table(api_data_clean,
              file = filename,
              sep = ",",
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = TRUE)
  
  
} #end loop 
  
