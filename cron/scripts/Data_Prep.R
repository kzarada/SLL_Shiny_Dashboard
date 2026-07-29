########################################
#
#   Dashboard Prep Script
#
########################################

#Load libraries 
library(tidyverse)
library(zoo)


data_dir = "/app/Data/"



#set current and start time (24 hours before current)
current_time = floor_date(now(tzone = "America/New_York"), unit = " 15 minutes")
start_time = current_time - days(1)

parms = read.csv(file.path(data_dir, "Inputs/CCO_Sensor_Parameters.csv"))%>% 
  mutate(Parameter = str_replace_all(Parameter, " ", "_"), 
         Parm_Unit = paste0(Parameter, "_", Units)) 


harbor_entrance = read.csv(file.path(data_dir, "Outputs/Nexsens_Harbor_Entrance_Data.csv")) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -100000))) %>% 
  arrange(Time_ET) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, 
                Hs_Wave_Height_m, 
                Hmax_Wave_Height_m,
                Mean_Wave_Direction__Deg) %>%
  mutate(Hs_Wave_Height_m = ifelse(Hs_Wave_Height_m < 0 | Hs_Wave_Height_m > 10, NA, Hs_Wave_Height_m), 
         Mean_Wave_Direction__Deg = ifelse(Hs_Wave_Height_m < 0 | Hs_Wave_Height_m > 360, NA, Hs_Wave_Height_m)) %>% 
  distinct() %>% 
  mutate(Hs_Wave_Height_ft = Hs_Wave_Height_m * 3.281, 
         Hmax_Wave_Height_ft = Hmax_Wave_Height_m * 3.281) %>% 
  rename_with(~paste0("Harbor_Entrance_", .x), !c(Time_ET))


north_shore= read.csv(file.path(data_dir, "Outputs/Nexsens_North_Shore_Data.csv")) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -100000))) %>%
  arrange(Time_ET) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, 
                Hs_Wave_Height_m, 
                Hmax_Wave_Height_m,
                Mean_Wave_Direction_.MWD._Deg) %>% 
  mutate(Hs_Wave_Height_m = ifelse(Hs_Wave_Height_m < 0 | Hs_Wave_Height_m > 10, NA, Hs_Wave_Height_m), 
         Mean_Wave_Direction_.MWD._Deg = ifelse(Mean_Wave_Direction_.MWD._Deg < 0 | Mean_Wave_Direction_.MWD._Deg > 360, NA, Mean_Wave_Direction_.MWD._Deg)) %>% 
  distinct() %>% 
  mutate(Hs_Wave_Height_ft = Hs_Wave_Height_m * 3.281, 
         Hmax_Wave_Height_ft = Hmax_Wave_Height_m * 3.281)%>% 
  rename_with(~paste0("North_Shore_", .x), !c(Time_ET))

rainsford_buoy = read.csv(file.path(data_dir, "Outputs/Nexsens_Rainsford_NE_Data.csv")) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -100000))) %>% 
  arrange(Time_ET) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, 
                Hs_Wave_Height_m, 
                Mean_Wave_Direction__Deg) %>% 
  mutate(Hs_Wave_Height_m = ifelse(Hs_Wave_Height_m < 0 | Hs_Wave_Height_m > 10, NA, Hs_Wave_Height_m), 
         Mean_Wave_Direction__Deg = ifelse(Mean_Wave_Direction__Deg< 0 | Mean_Wave_Direction__Deg > 360, NA, Mean_Wave_Direction__Deg)) %>% 
  distinct() %>% 
  mutate(Hs_Wave_Height_ft = Hs_Wave_Height_m * 3.281) %>% 
  rename_with(~paste0("Rainsford_", .x), !c(Time_ET))

rainsford = read.csv(file.path(data_dir, "Outputs/LiCOR_Rainsford_Island_Data.csv"))  %>% 
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -100000))) %>% 
  arrange(Time_ET) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, Wind.Speed_RMYoung_mph, Gust.Speed_RMYoung_mph, 
                Wind.Direction_RMYoung_deg) %>% 
  mutate(Wind.Speed_RMYoung_mph = ifelse(Wind.Speed_RMYoung_mph < 0 | Wind.Speed_RMYoung_mph > 120, NA, Wind.Speed_RMYoung_mph ),
         Gust.Speed_RMYoung_mph = ifelse(Gust.Speed_RMYoung_mph < 0 | Gust.Speed_RMYoung_mph > 130, NA, Gust.Speed_RMYoung_mph ), 
         Wind.Direction_RMYoung_deg = ifelse(Wind.Direction_RMYoung_deg < 0 | Wind.Direction_RMYoung_deg > 360, NA, Wind.Direction_RMYoung_deg)) %>% 
  distinct()



gallops = read.csv(file.path(data_dir, "Outputs/Nexsens_Gallops_Data.csv")) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -100000))) %>% 
  arrange(Time_ET) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, Water_Level_ft) %>%
  rename(Gallops_Water_Level_ft = Water_Level_ft) %>% 
  mutate(Gallops_Water_Level_ft = Gallops_Water_Level_ft + 5.5) %>% 
  distinct()


essex_tide = read.csv(file.path(data_dir, "Outputs/Tide_Hohonu_Essex_Tide_Data.csv")) %>% 
  mutate(Time_ET = as.POSIXct(Time_ET,format = "%Y-%m-%d %H:%M:%S",  tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time) %>% 
  filter(QC_Roll_Up == 1) %>% 
  arrange(Time_ET) %>% 
  rename(Essex_Water_Level_ft = Flood.Depth) %>% 
  dplyr::select(Time_ET, Essex_Water_Level_ft) %>% 
  mutate(Essex_Water_Level_ft = Essex_Water_Level_ft + 5.4) %>% 
  distinct() 
  

noaa_boston_tide = read.csv(file.path(data_dir, "Outputs/NOAA_Boston_Data.csv")) %>%
  mutate(Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M", tz = "America/New_York")) %>%
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, Water_MLLW) %>%
  rename(Boston_Water_MLLW = Water_MLLW) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)


noaa_fall_river_tide = read.csv(file.path(data_dir, "Outputs/NOAA_Fall_River_Data.csv")) %>%
  mutate(Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M", tz = "America/New_York")) %>%
  filter(Time_ET > start_time & Time_ET < current_time)  %>% 
  dplyr::select(Time_ET, Water_MLLW) %>%
  rename(Fall_River_Water_MLLW = Water_MLLW) %>% 
  filter(Time_ET > start_time & Time_ET < current_time)

combo = tibble(Time_ET = seq(start_time, current_time, by = "1 min")) %>% 
  left_join(harbor_entrance) %>% 
  left_join(rainsford) %>% 
  left_join(gallops) %>% 
  left_join(essex_tide) %>% 
  left_join(rainsford_buoy) %>% 
  left_join(north_shore) %>% 
  left_join(noaa_boston_tide) %>% 
  left_join(noaa_fall_river_tide) %>% 
  arrange(Time_ET) %>% 
  mutate(across(where(is.numeric), 
                ~zoo::na.approx(.x, na.rm = F))) 


################################################
##### Hohonu Data Grab + Cleaning 
################################################
hohonu_locations = read.csv(file.path(data_dir, "Inputs/Hohonu Locations.csv"))

files <- fs::dir_ls(path = file.path(data_dir, "Outputs/"), 
                    glob = "*/Hohonu_*")

hohonu <- vroom::vroom(files, 
                       id = "source", 
                       show_col_types = FALSE) %>% 
  mutate(Time_ET = lubridate::force_tz(Time_ET, tz = "America/New_York")) %>% 
  filter(Time_ET > start_time & Time_ET < current_time) %>% 
  dplyr::select(-c(`...1`, source)) %>% 
  group_by(Location) %>% 
  mutate(Location = str_replace(Location, "_", "\\."), 
         min = min(Flood.Depth, na.rm = T), 
         QC_Sensor_Error= ifelse(min > 0, 4, NA)) %>% 
  ungroup() %>% 
  mutate(QC_Note = case_when(
    QC_Sensor_Error == 4 ~ "Likely sensor error (e.g., object or snow blocking sensor", 
    QC_Roll_Up == 9 ~ "No data collected - check back in a couple of minutes", 
    QC_Roll_Up == 4 ~ "Data failed quality control checks (e.g., data out of range)", 
    QC_Roll_Up == 3 ~ "Suspect data, use with caution", 
    is.na(Flood.Depth) ~ "No data collected - check back in a couple minutes", 
    .default = "Data passed Hohonu's quality control checks")) %>% 
  left_join(hohonu_locations, by = c("Location" = "Station")) %>% 
  mutate(Flood.Depth = ifelse(Location == "Salem" & Flood.Depth < 0.2, 0, Flood.Depth), 
         Flood.Depth = ifelse(QC_Roll_Up != 1, NA, Flood.Depth)) %>% 
  distinct() %>% 
  #group_by(Location) %>% 
  #arrange(Time_ET) %>% 
  #mutate(across(where(is.numeric), 
  #              ~zoo::na.approx(.x, na.rm = F)), 
  #       Flood.Depth = round(Flood.Depth, 2)) %>% 
  ungroup() 

map_hohonu <- hohonu %>% 
  complete(., Time_ET, Location, 
           fill = list(QC_Note = "Gap Filled Data", 
                       Unit = "Flood Depth_ft", 
                       min = 0)) |> 
  group_by(Location) %>%
  mutate(Flood.Depth = zoo::na.approx(Flood.Depth, maxgap = 10, rule = 2)) |> 
  fill(c(Sponsor, Station.Name, Latitude, Longitude, Type, Directions), .direction = "down") 


data_avail = map_hohonu |> 
              group_by(Location) |> 
              slice_max(Time_ET) |> 
              filter(is.na(Flood.Depth)) |> 
              dplyr::select(Location, Time_ET)

data_avail_2 = map_hohonu |> 
                  filter(Location %in% data_avail$Location) |> 
                  drop_na(Flood.Depth) |> 
                  slice_max(Time_ET) |> 
                  dplyr::select(Location, Time_ET) |> 
                  rename(Last_Time = Time_ET) |> 
                  mutate(Time_Diff = current_time - Last_Time) |> 
                  mutate(Last_Available = paste0("Data temporarily unavailable from this sensor. Data was last recorded from this sensor ", Time_Diff, " minutes ago. 
                                                The Dashboard updates every 10 minutes - check back soon for updates.")) |> 
                  dplyr::select(Location, Last_Available) |> 
                  left_join(data_avail)

map_hohonu = map_hohonu |> 
              left_join(data_avail_2) |> 
              mutate(Last_Available = ifelse(is.na(Flood.Depth) & is.na(Last_Available), 
                                             "No data available for this sensor at this time point. Missing data is likely due to a sensor reading error (e.g., abnormally high reading or reading out of normal range).", 
                                             Last_Available),
                      Last_Available = replace_na(Last_Available, ""))

################################################
##### NOAA Predictions
################################################

fall_river_pred = read.csv(file.path(data_dir, "Outputs/NOAA_Fall_River_Predictions.csv")) %>%
  mutate(Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M", tz = "America/New_York")) %>%
  filter(Time_ET > start_time)  %>% 
  dplyr::select(Time_ET, Prediction_MLLW) %>%
  rename(Fall_River_Water_Prediction = Prediction_MLLW)

boston_pred = read.csv(file.path(data_dir, "Outputs/NOAA_Boston_Predictions.csv")) %>%
  mutate(Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M", tz = "America/New_York")) %>%
  filter(Time_ET > start_time)  %>% 
  dplyr::select(Time_ET, Prediction_MLLW) %>%
  rename(Boston_Water_Prediction = Prediction_MLLW)

predictions = full_join(fall_river_pred, boston_pred)


write.csv(combo, file.path(data_dir,"Outputs/combo.csv"))
write.csv(hohonu, file.path(data_dir, "Outputs/hohonu.csv"))
write.csv(map_hohonu, file.path(data_dir, "Outputs/map_hohonu.csv"))
write.csv(predictions, file.path(data_dir, "Outputs/tide_predictions.csv"))



