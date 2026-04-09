######################################
#
#   Nexsens Data Download
#
######################################
#load libraries
library(tidyverse)
library(httr2)
library(jsonlite)
library(vroom)


data_dir = "/app/Data/"

source("apps/scripts/api_keys.R")

#Device IDs 

device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
              filter(API == "Nexsens") %>% 
              filter(Active == "Yes")

#Nexsens Parameters of Interest

parms = read.csv(file.path(data_dir, "/Inputs/CCO_Sensor_Parameters.csv")) %>% 
              filter(API == "Nexsens") %>% 
              mutate(Parameter = str_replace_all(Parameter, " ", "_"), 
                      Parm_Unit = paste0(Parameter, "_", Units)) %>% 
              dplyr::select(Parameter.ID, Parm_Unit) 
  
  
      
#API Key


for(i in 1:dim(device_id)[1]){

#log file 
log_con <- file("/app/scripts/Nexsens_log.txt", open = "a")
  
filename = paste0(data_dir,"Outputs/", device_id$API[i], "_",  device_id$Location[i], "_Data.csv")

last_time = vroom(filename, col_select = Time, col_types = c(Time= 'T')) %>% 
                pull() %>% 
                last()

last_time = str_replace(as.character(round_date(last_time, unit = "minute")), " ", "%20")

current_time = now(tzone = "UTC")

#if statement to capture missing last_time values
if(is.na(last_time) | !exists("last_time")){ 
  last_time = current_time - days(1)}

if(str_detect(last_time, ":00", negate = TRUE)){
  last_time = paste0(last_time, "%2000:00:00")}

current_time = str_replace(as.character(round_date(current_time, unit = "minute")), " ", "%20")


#API URL
url = paste0("https://www.wqdatalive.com/api/v1/devices/", device_id$ID[i], 
             "/parameters/data?apiKey=", nexsens_key, "&from=", last_time, 
             "&to=", current_time)


req <- request(url)

response <- req_perform(req)

if (response$status_code!= 200) {
  cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
             " to: ", current_time, " failed with error ", resp_status_desc(response), " at ", Sys.time()), file = log_con, sep = "\n")
  next
  
} else{cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
                  " to: ", current_time, " succeeded at ", Sys.time()), file = log_con, sep = "\n")}


#Pull data and make dataframe
api_data = response %>% 
  resp_body_json(simplifyVector = TRUE) 

names(api_data$data$values) <- api_data$data$timestamp

api_data = do.call(rbind.data.frame, api_data$data$values)


#convert to wide dataframe
api_wide = api_data %>% 
              mutate(Time = rownames(.), 
                     parameterId = as.character(parameterId)) %>% 
              remove_rownames(.) %>% 
              left_join(parms, by = c("parameterId" = "Parameter.ID"), relationship = 'many-to-many') %>% 
              drop_na(Parm_Unit) %>% 
              dplyr::select(-parameterId) %>% 
              mutate(Time = str_remove(Time, "\\.[0-9]{1,2}$"), 
                     Time = as.POSIXct(Time, tz = "UTC")) %>% 
              pivot_wider(names_from = Parm_Unit,  
                          values_from = value) %>% 
              mutate(Time_ET = with_tz(Time, tzone = "America/New_York"))


#write.csv(api_wide, filename)
write.table(api_wide,
            file = filename,
            sep = ",",
            append = TRUE,
            quote = FALSE,
            col.names = FALSE,
            row.names = TRUE)

close(log_con)

}




##### Separate Loop for Gallops Data 
#The water level variable wasn't pulling with the full parameter pull but works fine if we just pull water level
device_id = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_ID.csv")) %>% 
  filter(API == "Gallops") %>% 
  filter(Active == "Yes")

for(i in 1:dim(device_id)[1]){

#log file 
log_con <- file("/app/scripts/logs/nexsens_log.txt", open = "a")
  
filename = paste0(data_dir, "Outputs/Nexsens_", device_id$Location[i], "_Data.csv")

last_time = vroom(filename, col_select = Time_UTC, col_types = c(Time_UTC= 'T')) %>% 
                pull() %>% 
                last()

current_time = now(tzone = "UTC")

#if statement to capture missing last_time values
if(is.na(last_time) | !exists("last_time")){ 
  last_time = current_time - days(1)}

if(str_detect(last_time, ":00", negate = TRUE)){
  last_time = paste0(last_time, "%2000:00:00")}

last_time = str_replace(as.character(round_date(last_time, unit = "minute")), " ", "%20")

current_time = str_replace(as.character(round_date(current_time, unit = "minute")), " ", "%20")


#API URL
url = paste0("https://www.wqdatalive.com/api/v1/devices/", device_id$ID[1],
             "/parameters/78922/data?apiKey=", nexsens_key, "&from=", last_time, 
             "&to=", current_time)

req <- request(url)

response <- req_perform(req)

if (response$status_code!= 200) {
  cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
             " to: ", current_time, " failed with error ", resp_status_desc(response), " at ", Sys.time()), file = log_con, sep = "\n")
  next
  
} else{cat(paste0("API request for ", device_id$Location[i], " for last time: ", last_time, 
                  " to: ", current_time, " succeeded at ", Sys.time()), file = log_con, sep = "\n")}


#Pull data and make dataframe
gallops_data = response %>% 
  resp_body_json(simplifyVector = TRUE) 

gallops_data  = as.data.frame(gallops_data$data)


#convert to wide dataframe
gallops_data  = gallops_data  %>% 
              rename(Time_UTC = timestamp) %>%  
              rename(Water_Level_ft = value) %>% 
              mutate(Time_UTC = as.POSIXct(Time_UTC, tz = "UTC")) %>% 
              mutate(Time_ET = round_date(with_tz(Time_UTC, tzone = "America/New_York"), unit = "minute")) 
  

#write.csv(gallops_data , filename)
write.table(gallops_data , 
            file = filename, 
            sep = ",", 
            append = TRUE, 
            quote = FALSE, 
            col.names = FALSE, 
            row.names = TRUE)

close(log_con)

}







