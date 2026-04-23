########################################
#
#   Current Coastal Conditions
#
########################################


library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(fresh)
library(sf)
library(shinybrowser)

#Set Data File Path (changes for dockerfile)
data_dir = "/srv/shiny-server/Data/"


################## Read in data #####################
instrument.locations = read.csv(file.path(data_dir, "Inputs/RealTimeMonitoring_Locations.csv")) %>% 
  dplyr::select(Name, ID, Latitude, Longitude) 


instrument.map = instrument.locations %>% 
  filter(str_detect(Name, "Flood Sensor", negate = T)) %>% 
  drop_na(Latitude) %>% 
  mutate(Type = case_when(
    str_detect(Name, "Buoy") ~  'buoy', 
    str_detect(Name, "Gauge") ~ "gauge", 
    str_detect(Name, "Weather") ~ "weather", 
    .default = NA)) 

combo = read.csv(file.path(data_dir, "Outputs/combo.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

hohonu = read.csv(file.path(data_dir, "Outputs/hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

map_hohonu = read.csv(file.path(data_dir, "Outputs/map_hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

tide_pred = read.csv(file.path(data_dir, "Outputs/tide_predictions.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

start_time = round_date(min(map_hohonu$Time_ET, na.rm = T), "10 mins")
end_time = max(map_hohonu$Time_ET, na.rm = T)


arrow_length_x <- 1800   # seconds (controls horizontal arrow size)
arrow_length_y <- 0.5   # wind-speed units (vertical size)


#colors: 
#blue: #256EFF
#teal: #2EBBAD
#darkblue: #002366
#white

####### Create theme #############

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2EBBAD"
  ),
  adminlte_sidebar(
    width = "200",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#256EFF",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

######### Helper Functions ##################
getColor <- function(hohonu) {
  sapply(hohonu$Flood.Depth, function(Flood.Depth) {
    if(is.na(Flood.Depth)) {
      "lightgray"
    } 
    
    else if(Flood.Depth == 0) {
      "#2CBF04"
    }
    
    else if(Flood.Depth > 0 & Flood.Depth < 0.5) {
      "#FAEE07"
    } 
    
    else if(Flood.Depth >= 0.5 & Flood.Depth < 1){
      "#F5A30C"
    }
    
    else if(Flood.Depth >=1 & Flood.Depth < 2){
      "#E82D07"
    }
    
    else if(Flood.Depth >= 2){
      "#8F00FF"
    }
  })
}


convert_units <- function(value, unit) {
  if (unit == "m") {
    return(round(value * 0.3048, 2))  # ft → meters
  } else {
    return(value)
  }
}




##############################################
#################################################

# Define UI for application that draws a histogram

ui <- dashboardPage(
  
  title = "SLL Current Coastal Conditions", 
  
  dashboardHeader(title = tags$a(href='https://stonelivinglab.org/',
                                 tags$img(src='LivingLab_logo_white_RGB.png', width =40, height = 40)), 
                  titleWidth = 70, 
                  tags$li(
                    class = "dropdown unit-toggle-nav",
                    shinyWidgets::prettySwitch(
                      inputId = "unit_toggle",
                      label = NULL,
                      value = FALSE,
                      fill = TRUE,
                      status = "primary"
                    )
                  )), 
  
  
  dashboardSidebar(
    sidebarMenu(id = 'tabs', 
                menuItem("Dashboard", tabName = 'dashboard', icon = icon('dashboard')), 
                menuItem("Stations", tabName = 'stations', icon = icon("water")), 
                menuItem("Instruments", tabName = 'instruments', icon = icon('cloud')),
                menuItem("Data Download", tabName = "download", icon = icon("download")), 
                menuItem("Feedback", tabName = 'feedback', icon = icon("comment-dots")),
                menuItem("Contact Us", tabName = 'contact', icon = icon("square-envelope"))), 
    collapsed = TRUE),
  
  dashboardBody(use_theme(mytheme),
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "CCC_styles.css")),
                
                tags$script(HTML('$(document).ready(function() {
                                 $("header").find("nav").append(\'<span class="myClass"> SLL Current Coastal Conditions</span>\');})')),
                
                shinybrowser::detect(), 
                
                
                tabItems(
                  tabItem(tabName = "dashboard", 
                          fluidRow(
                            
                            sliderInput(
                              inputId = "time",
                              label   = "Select time:",
                              min     = start_time,
                              max     = end_time,
                              value   = end_time,
                              step    = 6 * 60,   # 10 minutes (in seconds)
                              timeFormat = "%b %d %H:%M",
                              animate = animationOptions(interval = 300), 
                              width = "85%"),
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title = "Flood and Instrument Map", 
                                     class = "map-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     leafletOutput("flood_map", height = "100%")), 
                                   
                                   box(
                                     title = "Wind Speed at Rainsford Island",
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("wind_plot", height = "100%")))
                            ), #end column
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6",  
                                   
                                   box(title = selectInput(
                                     "tide_select",
                                     label = NULL, 
                                     choices = list("Select Tide Gauge" = 'intro',
                                                    #"Gallops Island" = "gallops", 
                                                    "NOAA - Boston" = 'boston', 
                                                    "NOAA - Fall River" = 'fall.river'),
                                     multiple = F), 
                                     solidHeader = TRUE, 
                                     width = 12, 
                                     class = 'plot-box',
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("tide_plot", height= '100%'))), 
                                   
                                   
                                   
                                   box(
                                     title = selectInput(
                                       "wave_select",
                                       label = NULL, 
                                       choices = list("Select Wave Buoy" = "intro",
                                                      "Harbor Entrance" = "harbor.entrance", 
                                                      "North Shore" = 'north.shore'),
                                       multiple = F),
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     shinyfullscreen::fullscreen_this(plotOutput("wave_plot", height = "100%"))))
                          ) #end fluid row
                  ), #end TabItem
                  
                  tabItem(tabName = "stations", 
                          fluidRow(
                            selectInput(
                              "station.id", 
                              "Select Station:", 
                              list("Boston - Border Street" = "Border.St", 
                                   "Boston - Lewis Mall" = "Lewis.Mall",
                                   "Boston - Long Wharf" = "Long.Wharf",
                                   "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                   "Boston - Tenean Beach" = "Tenean.Beach",
                                   "Essex - Main Street" = "Essex", 
                                   "Fall River - Stafford Square" = "Fall.River", 
                                   "Salem - Collin's Cove" = "Salem"), 
                              multiple = F), 
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title = "Sensor Photo", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     uiOutput("sensor_photo"), 
                                     width = 12
                                   )),
                            column(
                              width = 6,
                              class = "col-12 col-md-6", 
                              box(
                                title = 'Sensor Information', 
                                solidHeader = TRUE, 
                                height = "15vh",
                                status = "primary",  
                                htmlOutput("sensor_info"), 
                                width = 12
                              ),
                              
                              box(
                                title = 'Sensor Location', 
                                solidHeader = TRUE, 
                                status = 'primary', 
                                class = 'map-box',
                                leafletOutput("sensor_map"), 
                                width = 12
                              ), 
                              
                              box(
                                title = "Flood Depth", 
                                solidHeader = TRUE, 
                                class = 'plot-box',
                                status = "primary", 
                                shinyfullscreen::fullscreen_this(plotOutput("station_flood")), 
                                width = 12
                              ) #end box
                            ) #end col
                          ) #end fluid row
                  ), #end tabItem
                  
                  
                  tabItem(tabName = "instruments", 
                          fluidRow(
                            selectInput(
                              "instrument.id", 
                              "Select Instrument:", 
                              list("Boston NOAA Tide Gauge" = "Boston.Tide", 
                                   "Fall River NOAA Tide Gauge" = "Fall.River.Tide",
                                   "Gallops Island Tide Gauge" = "Gallops.Tide", 
                                   "Harbor Entrance Wave Buoy" = "Harbor.Entrance", 
                                   "North Shore Wave Buoy" = "North.Shore", 
                                   #"Rainsford NE Wave Buoy" = "Rainsford.Buoy",
                                   "Rainsford Island Weather Station" = "Rainsford.Weather"), 
                              multiple = F
                            ), 
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title = "Instrument Photo", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     uiOutput("instrument_photo"), 
                                     width = 12
                                   )),
                            
                            column(
                              width = 6,
                              class = "col-12 col-md-6", 
                              box(
                                title = 'Instrument Overview', 
                                solidHeader = TRUE, 
                                height = "15vh",
                                status = "primary",  
                                htmlOutput("instrument_text"), 
                                width = 12
                              ),
                              
                              box(
                                title = 'Instrument Location', 
                                solidHeader = TRUE, 
                                status = 'primary', 
                                class = 'map-box',
                                leafletOutput("instrument_map"), 
                                width = 12
                              ), 
                              
                              box(
                                title = "Instrument Data", 
                                solidHeader = TRUE, 
                                status = "primary", 
                                class = 'plot-box',
                                shinyfullscreen::fullscreen_this(plotOutput("instrument_graph")), 
                                width = 12
                              )
                            ) #end col
                          ) #end fluid row
                  ), #end tabItem
                  
                  tabItem(tabName = 'download', 
                          box(
                            title = "Download Data", 
                            status = 'primary', 
                            solidHeader = T, 
                            width = 12, 
                            p(HTML(paste0("The data used in the dashboard are available for download. 
                               Only the last 24 hours of data are available. <br><br>
                               
                              <strong>Data are real-time, not quality controlled, and may be inaccurate.</strong> 
                              The data have not been reviewed or edited. Real-time data may contain errors such as 
                              inaccurate sensor readings either from instrument error or sensor obstruction. 
                              For example, snow pack may result in false flood readings from the overland flood sensors.
                              Data users are cautioned to consider the provisional nature of the information 
                              before using it for decisions that concern personal or public safety or the conduct 
                              of business that involves substantial monetary or operational consequences. 
                              No warranty, express or implied, is given as to the accuracy, reliability, 
                              utility or completeness of the data provided in this download, and the 
                              Stone Living Lab and partners shall not be held liable for improper or 
                              incorrect use of the data provided, or information contained on these pages. <br><br>
                                    
                                If you would like access to data beyond the 24-hour window provided here, please email us at: ", 
                                          tags$a('info@stonelivinglab.org', 
                                                 href = 'mailto:info@stonelivinglab.org'), "<br><br>"))), 
                            div(style = "text-align:center;", 
                                actionButton(
                                  inputId = 'download_data', 
                                  label = "Download Data"))
                            
                          ) #end box
                  ),#end tabItem
                  
                  tabItem(tabName = "feedback", 
                          column(width = 12, 
                                 class = "col-12 col-md-6", 
                                 box(title = "Feedback Form", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     tags$iframe(
                                       src = "https://docs.google.com/forms/d/e/1FAIpQLSe8eRgdDoTZBjKnLsOeALOaG7zGSvQGpXPzf-gy8PBIQMaJrw/viewform?embedded=true", 
                                       style = "width:100%; height: 80vh;"
                                     ))
                          ) #end col
                  ), #end tabitem
                  tabItem(tabName = "contact", 
                          column(width = 12, 
                                 class = "col-12 col-md-6", 
                                 box(title = "About the Stone Living Lab", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     div(HTML("The Stone Living Lab is an innovative and collaborative initiative for testing and scaling up 
                                    nature-based approaches to climate adaptation, coastal resilience and ecological restoration in 
                                    the high-energy environment of the Boston Harbor Islands National and State Park. A “Living Lab” 
                                    brings research out of the lab and into the real world by creating a user-centered, open, 
                                    innovative ecosystem that engages scientists and the community in collaborative design and exploration.
                                    <br> <br> The Stone Living Lab is a partnership between Boston Harbor Now, UMass Boston’s School for the Environment, 
                                    the City of Boston, the Massachusetts Department of Conservation and Recreation, the Massachusetts Executive 
                                    Office of Energy and Environmental Affairs, the National Park Service, and the James M. and Cathleen D. 
                                    Stone Foundation that engages scientists and the community in research, education, and the promotion of equity.")))), 
                          
                          column(width = 12,
                                 class = "col-12 col-md-6", 
                                 box(title = "Contact Us", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     div(p(HTML(paste0("If you have feedback on this dashboard, questions about our work, 
                                                or have noticed issues with any of our overland flood sensors or instruments,
                                                please email us at ", tags$a("info@stonelivinglab.org", 
                                                                             href = "mailto:info@stonelivinglab.org"))))))), 
                          column(width = 12, 
                                 class = "col-12 col-md-6", 
                                 box(title = "Keep in touch!", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     tags$iframe(
                                       src = "https://mailchi.mp/stonelivinglab.org/oflzp4092d", 
                                       style = "width:100%; height: 80vh;"
                                     ))), 
                          tags$img(src='Full_Logo.png', 
                                   height = 200,
                                   style="display: block; margin-left: auto; margin-right: auto;")) #end tabItem
                  
                ), #end tabItems
                
                tags$div(
                  class = "app-footer",
                  tags$a(
                    href = "http://147.93.47.40:3838/",
                    target = "_blank",
                    HTML("Only interested in flooding? <u>Click here</u> to see the SLL Flooding Dashboard"))
                ) #end footer
  ) #end dashbody
) #end ui 



# ---- Server ----
server <- function(input, output, session) {
  
  ################## Popup ################## 
  ################## Popup ################## 
  showModal(modalDialog(
    title = "Welcome to the Stone Living Lab Current Coastal Conditions Dashboard!",
    HTML(paste0("This dashboard displays data from our real-time monitoring sensors. 
    For more information on how to navigate the dashboard, please see our <u>", tags$a("dashboard user guide.", 
                                                                                       href = "https://www.canva.com/design/DAGzC4w-HyY/FTvLHmZpkhQShXF0yTdktg/view?utm_content=DAGzC4w-HyY&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h023b1e34ec", 
                                                                                       target = '_blank'), "</u>"), 
         "<br><br> <strong> Please note that data are currently not available from the Gallops Tide Station. 
         We are working on getting this instrument back online as soon as possible."),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
  
  ########## Mobile Detection #############

  plot_theme <- reactive({
    if (shinybrowser::is_device_mobile()) {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 7),
          axis.title   = element_text(size = 9),
          legend.text  = element_text(size = 7),
          legend.title = element_blank(), 
          legend.position = "bottom"
        )
    } else {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text  = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text  = element_text(size = 16),
          legend.title = element_blank(), 
          legend.position = "bottom"
        )
    }
  })
  ########### Unit Toggle ####################
  
  unit_state <- reactive({ifelse(input$unit_toggle, "m", "ft")})
  
  observeEvent(input$unit_toggle, {
    unit_state = ifelse(input$unit_toggle, "m", "ft")
  })
  
  
  observe({
    updateActionButton(
      session, 
      "unit_toggle",
      label = unit_state()
    )
  })
  
  ################ Data ####################
  
  filtered_flood_data <- reactive({
    
    map_hohonu_data() %>% filter(Time_ET == with_tz(input$time, tzone = "America/New_York"))
    
  })
  
  sensor_loc <- reactive({
    hohonu_data() %>% filter(Location == input$station.id)
  })
  
  
  instrument_loc <- reactive({
    instrument.locations %>% filter(ID == input$instrument.id)
  })
  
  ############# Wind Direction ################
  
  wind_dir <- reactive({
    combo_data() %>% 
    mutate(Time_ET = round_date(Time_ET, unit = "hour")) %>% 
    group_by(Time_ET) %>% 
    summarise(Mean_Wind_Dir = mean(Wind.Direction_RMYoung_deg)) %>% 
    ungroup() %>%
    mutate(
      dir_rad = (Mean_Wind_Dir+ 180)*pi / 180, 
      arrow_y = rep(-1), 
      arrow_xend = Time_ET + arrow_length_x * cos(dir_rad), 
      arrow_yend = arrow_y + arrow_length_y * sin(dir_rad))
  
  })
  
  
  ################# Main Page Plots ##########################
  
  output$wind_plot <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Wind Speed (mph)", "Wind Speed (m/s)")
    
    wind_speed = if(unit == "m"){
      combo_data()$Wind.Speed_RMYoung_mph/2.237}else{combo_data()$Wind.Speed_RMYoung_mph}
    gust_speed = if(unit == "m"){combo_data()$Gust.Speed_RMYoung_mph/2.237}else{combo_data()$Gust.Speed_RMYoung_mph}
    
    y_max = if(unit == "m"){
      max(gust_speed + 1, 6.7)}else{max(gust_speed + 1, 15)}
    
    
    ggplot(combo_data(), aes(x = Time_ET, y = wind_speed)) +
      geom_line(aes(x = Time_ET, y = wind_speed, color = "Wind Speed"), linewidth = 1) +
      geom_line(aes(x = Time_ET, y = gust_speed, color = "Gust Speed"), linewidth = 1) +
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylim(c(-2, y_max)) + 
      geom_segment(data = wind_dir(), 
                   aes(xend = arrow_xend, 
                       y = arrow_y, 
                       yend = arrow_yend, 
                       color = "Wind Direction"), 
                   arrow = arrow(length = unit(0.15, 'cm'))) + 
      xlab("Time (ET)") + 
      ylab(y_label) +
      scale_color_manual(
        values = c("#256EFF", "#002366", "#2EBBAD")) +
      plot_theme()
    
    
  })
  
  
  output$wave_plot <- renderPlot({
    
    unit = unit_state() 
    
    wave_height = if(input$wave_select == 'intro'){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hs_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hs_Wave_Height_ft}
    }else if(input$wave_select == "harbor.entrance"){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hs_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hs_Wave_Height_ft}
    } else if(input$wave_select == 'rainsford'){
      if(unit == "m"){combo_data()$Rainsford_Hs_Wave_Height_m}else{combo_data()$Rainsford_Hs_Wave_Height_ft}
    } else if(input$wave_select == "north.shore"){
      if(unit == "m"){combo_data()$North_Shore_Hs_Wave_Height_m}else{combo_data()$North_Shore_Hs_Wave_Height_ft}
    }
    
    max_height = if(input$wave_select == 'intro'){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hmax_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hmax_Wave_Height_ft}
    }else if(input$wave_select == "harbor.entrance"){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hmax_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hmax_Wave_Height_ft}
    } else if(input$wave_select == 'rainsford'){
      NA
    } else if(input$wave_select == "north.shore"){
      if(unit == "m"){combo_data()$North_Shore_Hmax_Wave_Height_m}else{combo_data()$North_Shore_Hmax_Wave_Height_ft}
    }
    
    y_max = max(max_height, convert_units(2.5, unit))
    
    ggtitle = case_when(
      input$wave_select == "intro" ~ "Harbor Entrance Wave Buoy",
      input$wave_select == "harbor.entrance" ~ "Harbor Entrance Wave Buoy", 
      input$wave_select == "rainsford" ~ "Rainsford NE Wave Buoy", 
      input$wave_select == 'north.shore' ~ "North Shore Wave Buoy", 
      .default = NA
    )
    
    y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
    
    ggplot(combo_data(), aes(x = Time_ET, y = wave_height)) + 
      geom_line(aes(color = "Significant Wave Height"), linewidth= 1) + 
      geom_line(aes(x = Time_ET, y = max_height, color = "Maximum Wave Height"), linewidth = 1) + 
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (ET)") + 
      ggtitle(ggtitle) + 
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      scale_color_manual(
        values = c("#256EFF","#2EBBAD")) + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18))

  })
  
  output$tide_plot <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)")
    
    ggtitle = case_when(
      input$tide_select == "intro" ~ "NOAA Tide Gauge and Flood Predictions - Boston",
      input$tide_select == "gallops" ~ "Gallops Tide Gauge", 
      input$tide_select == "boston" ~ "NOAA Tide Gauge and Flood Predictions - Boston", 
      input$tide_select == 'fall.river' ~ "NOAA Tide Gauge and Flood Predictions - Fall River", 
      .default = NA
    )
    
    water_level = if(input$tide_select == "gallops"){
      combo_data()$Gallops_Water_Level_ft}
    else if(input$tide_select == "boston"){
      combo_data()$Boston_Water_MLLW
    }else if(input$tide_select == 'fall.river'){
      combo_data()$Fall_River_Water_MLLW
    }else if(input$tide_select == 'intro'){
      combo_data()$Boston_Water_MLLW
    }
    
    water_level = if(unit == "m"){
      water_level/3.281}else{water_level}
    
    
    prediction = if(input$tide_select == "gallops"){
      NA}
    else if(input$tide_select == "boston"){
      tide_pred()$Boston_Water_Prediction
    }else if(input$tide_select == 'fall.river'){
      tide_pred()$Fall_River_Water_Prediction
    }else if(input$tide_select == 'intro'){
      tide_pred()$Boston_Water_Prediction
    }
    
    prediction = if(unit == "m"){
      prediction/3.281}else{prediction}
    
    
    major = if(input$tide_select == "gallops"){
      NA}
    else if(input$tide_select == "boston"){
      16
    }else if(input$tide_select == 'fall.river'){
      11.98
    }else if(input$tide_select == 'intro'){
      16
    }
    
    major = if(unit == "m"){
      major/3.281}else{major}
    
    moderate = if(input$tide_select == "gallops"){
      NA}
    else if(input$tide_select == "boston"){
      14.49
    }else if(input$tide_select == 'fall.river'){
      9.48
    }else if(input$tide_select == 'intro'){
      14.49
    }
    
    moderate = if(unit == "m"){
      moderate/3.281}else{moderate}
    
    minor = if(input$tide_select == "gallops"){
      NA}
    else if(input$tide_select == "boston"){
      12.50
    }else if(input$tide_select == 'fall.river'){
      6.98
    }else if(input$tide_select == 'intro'){
      12.50
    }
    
    minor = if(unit == "m"){
      minor/3.281}else{minor}
    
    ymax = max(water_level, (major + 3))
    
    ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
      geom_line(aes(color = "Actual Water Level"), linewidth = 1) +
      geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level"), linetype = 'dotted', linewidth =1) + 
      geom_hline(yintercept = minor, color = "#F6C871", linewidth = 1.5, linetype = 'dotted') + 
      geom_hline(yintercept = moderate, color = "#EE7E6D", linewidth = 1.5, linetype = 'dotted') + 
      geom_hline(yintercept = major, color = "#F28FDB", linewidth = 1.5, linetype = 'dotted') + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= minor, 
                    ymax = moderate, 
                    fill = "NOAA - Minor Flooding")) + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= moderate + 0.1, 
                    ymax = major, 
                    fill = "NOAA - Moderate Flooding")) + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= major + 0.1, 
                    ymax = major + 2, 
                    fill = "NOAA - Major Flooding")) + 
      scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) +
      xlab("Time (ET)") +
      ggtitle(ggtitle) +
      scale_color_manual(
        values = c("#002366", "#2E3440")) + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical')
    
    
  }) 
  
  
############## MAP ###################
  zoom_level <- reactive({
    input$flood_map_zoom
  })
  
  marker_radius <- reactive({
    z <- zoom_level()
    if (is.null(z)) return(16)
    
    # smoother scaling
    return(2 * (1.2 ^ z))
  })
  
  label_size <- reactive({
    z <- input$flood_map_zoom
    
    if (is.null(z)) return("12px")
    
    if(z < 11) return("0px")
    
    # Scale text with zoom (adjust multiplier to taste)
    size <- 6 + z * 0.8
    
    paste0(size, "px")
  })
  
  instrument_width <- reactive({
    z <- input$flood_map_zoom
    
    if (is.null(z)) return(16)
    if(z < 10.5) return(0.1)
    
    return(25)
  })
  
  instrument_height <- reactive({
    z <- input$flood_map_zoom
    
    if (is.null(z)) return(16)
    if(z < 10.5) return(0.1)
    
    return(25)
  })

  output$flood_map <- renderLeaflet({
    
    unit = unit_state()
    
    ft_label = c("None", "< 0.5 ft", "0.5 - 1 ft", 
                 "1 - 2 ft", "> 2 ft", "No Data Available")
    m_label =  c("None", "< 0.15 m", "0.15 - 0.3 m", 
                 "0.3 m - 0.6 m", "> 0.6 m", "No Data Available")
    legend_label = if(unit == 'ft'){ft_label
    }else{m_label}
    
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -70.88, lat = 42.23, zoom = 7.5) %>% 
      addLegend(position = "bottomright", 
                opacity = 1, 
                colors = c("#2CBF04", "#FAEE07", "#F5A30C", "#E82D07", "#8F00FF", "lightgray"), 
                labels = legend_label, 
                title = "Flood Depth") 
  })
  
  observe({
    
    unit = unit_state()
    
    
    instrument.icons = iconList(
      buoy = makeIcon(
        iconUrl = 'wave.png', 
        iconWidth = instrument_width() + 10 , 
        iconHeight = instrument_height()), 
      gauge = makeIcon(
        iconUrl = 'tide.png', 
        iconWidth = instrument_width() , 
        iconHeight = instrument_height()), 
      weather = makeIcon(
        iconUrl = 'wind.png', 
        iconWidth = instrument_width(), 
        iconHeight = instrument_height()))
    
    
    leafletProxy("flood_map", data = filtered_flood_data()) %>% 
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      addCircleMarkers(data = filtered_flood_data(), 
                       lat = ~Latitude, 
                       lng = ~Longitude, 
                       # clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                       #                                       zoomToBoundsOnClick = TRUE, 
                       #                                       singleMarkerMode = TRUE),
                       color = getColor(filtered_flood_data()), 
                       radius = marker_radius(),  
                       fillOpacity = 1,
                       layerId = filtered_flood_data()$Location,
                       label=~as.character(Flood.Depth),
                       labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                   style = list(
                                                     "color" = "black",
                                                     "font-family" = "Replica Mono LL TT",
                                                     "font-style" = "bold",
                                                     "font-size" = label_size())), 
                       popup = ~paste0("<strong>", Station.Name,
                                       "</strong><br/>
                                      Flood Depth: ", Flood.Depth, " ", unit, 
                                       "<br/> Data quality note: ", QC_Note,
                                       "<br/> <a href='#'
                                          onclick=\"
                                          Shiny.setInputValue('go_to_tab',\'", 
                                       Location, "\',{priority:'event'});
                                          \">View station details </a>")) %>% 
      addMarkers(data = instrument.map, 
                 lat = ~Latitude, 
                 lng = ~Longitude,
                 icon = ~instrument.icons[Type], 
                 popup = ~paste0("<strong>", Name,
                                  "</strong><br/> <a href='#'
                                          onclick=\"
                                          Shiny.setInputValue('go_to_instrument',\'", 
                                  ID, "\',{priority:'event'});
                                          \">View instrument details </a>"))
    
  })
  
  ############ Observe Event for View Station Details ############
  observeEvent(input$go_to_tab, {
    
    updateTabItems(
      session,
      inputId = "tabs",
      selected = 'stations'
    )
    
    updateSelectInput(
      session, 
      "station.id", 
      select = input$go_to_tab
    )
    
  })
  
  observeEvent(input$go_to_instrument, {
    
    updateTabItems(
      session,
      inputId = "tabs",
      selected = 'instruments'
    )
    
    updateSelectInput(
      session, 
      "instrument.id", 
      select = input$go_to_instrument
    )
    
  })
  
  
  
  ############## Sensor Page ##############
  
  output$sensor_photo <- renderUI({
    
    req(input$station.id)
    
    tags$img(
      src = paste0(input$station.id, ".jpg"), 
      width = "100%")
  })
  
  output$sensor_info <- renderText({
    url = ifelse(unique(sensor_loc()$Type) == "ultrasonic", "https://docs.hohonu.io/How-Do-Ultrasonic-Sensors-Work-2a7d721e3e7e80be817addd2f6854972", 
                 "https://docs.hohonu.io/How-Do-Radar-Sensors-Work-2a7d721e3e7e80c69b08eb36e93858da")
    
    
    HTML(paste0("This sensor is a ", 
                tags$a(
                  href = url,
                  target = "_blank",
                  HTML(paste0("<u>",unique(sensor_loc()$Type) ,"</u>"))), 
                " overland flood sensor in partnership with the ", 
                unique(sensor_loc()$Sponsor), "."))
  })
  
  
  output$sensor_map <- renderLeaflet({
    
    leaflet() %>% 
      addMarkers(data = sensor_loc(), 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 popup = ~paste0("<a href= ", Directions,
                                 " target= '_blank' 
                                         > Click here for directions to the sensor </a>")) %>% 
      addProviderTiles(providers$Esri.WorldImagery)  
  })
  
  
  
  output$station_flood <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    depth = if(unit == "m"){sensor_loc()$Flood.Depth/3.281}else{sensor_loc()$Flood.Depth}
    y_max = max(depth, convert_units(1, unit_state()), na.rm = T)
    
    ggplot(sensor_loc(), aes(x = Time_ET, y = depth)) + 
      geom_line(linewidth = 1.5, color = "#2EBBAD") + 
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (ET)") + 
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18)) + 
      plot_theme()
    
  })
  
  
  ############# Instrument Page ##################
  
  output$instrument_photo <- renderUI({
    
    req(input$instrument.id)
    
    tags$img(
      src = paste0(input$instrument.id, ".jpg"), 
      width = "100%")
  })
  
  output$instrument_text <- renderText({
    
    if(input$instrument.id %in% c("Boston.Tide", "Fall.River.Tide", "Gallops.Tide")){
      
      "Tide gauges are acoustic or radar instruments that measure changes in sea level. The major, moderate, and minor flooding lines and the predicted future water level are from NOAA."
    }
    else if(input$instrument.id %in% c("Harbor.Entrance", "North.Shore", "Rainsford.Buoy")){
      "Wave buoys are floating oceanographic instruments anchored in place that measure wave characteristics such as wave height, direction, and period."
    }
    else if(input$instrument.id == "Rainsford.Weather"){
      "Weather stations are instruments that collect information on the weather including wind speed, wind direction, barometric pressure, and air temperature."
    }
    
    
  })
  
  output$instrument_map <- renderLeaflet({
    
    leaflet() %>% 
      addMarkers(data = instrument_loc(), 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 popup = ~paste0("Latitude: ", round(Latitude, 2), "<br> Longitude: ", round(Longitude, 2))) %>% 
      setView(lat = instrument_loc()$Latitude,
              lng = instrument_loc()$Longitude, 
              zoom = 12) %>% 
      addProviderTiles(providers$Esri.WorldImagery)  
  })
  
  output$instrument_graph <- renderPlot({
    unit = unit_state() 
    
    wave_height =  if(unit == "m"){combo_data()$Harbor_Entrance_Hs_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hs_Wave_Height_ft}
    max_wave = if(unit == "m"){combo_data()$Harbor_Entrance_Hmax_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hmax_Wave_Height_ft}
    
    y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
    
    if(input$instrument.id == "Harbor.Entrance"){
      ggplot(combo_data(), aes(x = Time_ET, y = wave_height)) + 
        geom_line(aes(color = "Significant Wave Height (ft)"), linewidth= 1) + 
        geom_line(aes(x = Time_ET, y = max_wave, color = "Maximum Wave Height (ft)"), linewidth = 1) + 
        ylab(y_label) + 
        xlab("Time (ET)") + 
        scale_color_manual(
          values = c("#256EFF","#2EBBAD")) + 
        plot_theme()
    }
    else if(input$instrument.id == "Rainsford.Weather"){
      ggplot(combo_data(), aes(x = Time_ET, y = Wind.Speed_RMYoung_mph)) +
        geom_line(aes(x = Time_ET, y = Wind.Speed_RMYoung_mph, color = "Wind Speed"), linewidth = 1) +
        geom_line(aes(x = Time_ET, y = Gust.Speed_RMYoung_mph, color = "Gust Speed"), linewidth = 1) +
        ylim(c(-2, max(combo_data()$Gust.Speed_RMYoung_mph + 1))) + 
        geom_segment(data = wind_dir, 
                     aes(xend = arrow_xend, 
                         y = arrow_y, 
                         yend = arrow_yend, 
                         color = "Wind Direction"), 
                     arrow = arrow(length = unit(0.15, 'cm'))) + 
        xlab("Time (ET)") + 
        ylab("Wind Speed (mph)") +
        scale_color_manual(
          values = c("#256EFF", "#002366", "#2EBBAD")) +
       plot_theme()
    }
    else if(input$instrument.id == "Gallops.Tide"){
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = combo_data()$Gallops_Water_Level_ft
      
      if(unit == "m"){
        water_level/3.281}else{water_level}
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        geom_line(aes(color = "Water Level"), linewidth = 1) +
        ylab(y_label) +
        xlab("Time (ET)") + 
        scale_color_manual(
          values = c("#002366")) + 
        plot_theme() + 
        theme(legend.position = 'none')
      
    }
    else if(input$instrument.id ==  "Boston.Tide"){
     
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      major = if(unit == "m"){
        16/3.281}else{16}
      
      moderate = if(unit == "m"){
        14.49/3.281}else{14.49}
      
      minor = if(unit == "m"){
        12.50/3.281}else{12.5}
      
      water_level = combo_data()$Boston_Water_MLLW

      if(unit == "m"){
        water_level/3.281}else{water_level}
      
      prediction = tide_pred()$Boston_Water_Prediction
    
        prediction = if(unit == "m"){
          prediction/3.281}else{prediction}
    
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        geom_line(aes(color = "Water Level"), linewidth = 1) +
        ylab(y_label) +
        xlab("Time (ET)") + 
        scale_color_manual(
          values = c("#002366")) + 
        geom_hline(yintercept = minor, color = "#F6C871", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = moderate, color = "#EE7E6D", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = major, color = "#F28FDB", linewidth = 1.5, linetype = 'dotted') + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= minor, 
                      ymax = moderate, 
                      fill = "NOAA - Minor Flooding")) + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= moderate + 0.1, 
                      ymax = major, 
                      fill = "NOAA - Moderate Flooding")) + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= major + 0.1, 
                      ymax = major + 2, 
                      fill = "NOAA - Major Flooding")) + 
        scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
        plot_theme() + 
        theme(plot.title = element_text(size = 18), 
              legend.box = 'vertical')
      
      
    }
    else if(input$instrument.id == "Fall.River.Tide"){
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = combo_data()$Fall_River_Water_MLLW
      
      if(unit == "m"){
        water_level/3.281}else{water_level}
      
      prediction = tide_pred()$Fall_River_Water_Prediction
      
      prediction = if(unit == "m"){
        prediction/3.281}else{prediction}
      
      major = if(unit == "m"){
        11.98/3.281}else{11.98}
      
      moderate = if(unit == "m"){
        9.48/3.281}else{9.48}
      
      minor = if(unit == "m"){
        6.98/3.281}else{6.98}
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        geom_line(aes(color = "Water Level"), linewidth = 1) +
        ylab(y_label) +
        xlab("Time (ET)") + 
        scale_color_manual(
          values = c("#002366")) + 
        geom_hline(yintercept = minor, color = "#F6C871", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = moderate, color = "#EE7E6D", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = major, color = "#F28FDB", linewidth = 1.5, linetype = 'dotted') + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= minor, 
                      ymax = moderate, 
                      fill = "NOAA - Minor Flooding")) + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= moderate + 0.1, 
                      ymax = major, 
                      fill = "NOAA - Moderate Flooding")) + 
        geom_rect(aes(xmin = -Inf, 
                      xmax = Inf, 
                      ymin= major + 0.1, 
                      ymax = major + 2, 
                      fill = "NOAA - Major Flooding")) + 
        scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
        plot_theme() + 
        theme(plot.title = element_text(size = 18), 
              legend.box = 'vertical')
      
    }
    else if(input$instrument.id == "North.Shore"){
      unit = unit_state() 
      
      wave_height = if(unit == "m"){combo_data()$North_Shore_Hs_Wave_Height_m}else{combo_data()$North_Shore_Hs_Wave_Height_ft}
      max_height = if(unit == "m"){combo_data()$North_Shore_Hmax_Wave_Height_m}else{combo_data()$North_Shore_Hmax_Wave_Height_ft}
      
      
      y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
      
      ggplot(combo_data(), aes(x = Time_ET, y = wave_height)) + 
        geom_line(aes(color = "Significant Wave Height"), linewidth= 1) + 
        geom_line(aes(x = Time_ET, y = max_height, color = "Maximum Wave Height"), linewidth = 1) + 
        ylab(y_label) + 
        xlab("Time (ET)") + 
        geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                   color = "darkred", linewidth = 1, linetype = "dashed") +
        theme_bw(base_family = "Replica Mono LL TT") + 
        scale_color_manual(
          values = c("#256EFF","#2EBBAD")) + 
        plot_theme()
    }
    else if(input$instrument.id == "Rainsford.Buoy"){
      unit = unit_state() 
      
      wave_height = if(unit == "m"){combo_data()$Rainsford_Hs_Wave_Height_m}else{combo_data()$Rainsford_Hs_Wave_Height_ft}
      
      y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
      
      ggplot(combo_data(), aes(x = Time_ET, y = wave_height)) + 
        geom_line(aes(color = "Significant Wave Height"), linewidth= 1) + 
        ylab(y_label) + 
        xlab("Time (ET)") + 
        geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                   color = "darkred", linewidth = 1, linetype = "dashed") +
        scale_color_manual(
          values = c("#256EFF")) + 
        plot_theme()
    }
    
  })
  
  
  
  
  ########## Reactive Statement to refresh the app ############
  
  combo_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/combo.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }
  )
  
  hohonu_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/hohonu.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }
  )
  
  map_hohonu_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/map_hohonu.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }
  )
  
  tide_pred <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/tide_predictions.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }
  )
  observe({
    newData = map_hohonu_data() 
    updateSliderInput(session, 
                      "time", 
                      min = round_date(min(newData$Time_ET), "10 mins"), 
                      max = max(newData$Time_ET), 
                      value = max(newData$Time_ET), 
                      timeFormat = "%b %d %H:%M")
  })
  
  ############## Data Download ############
  
  observeEvent(input$download_data, { 
    showModal(modalDialog(
      title = "User Agreement",
      tagList(
        p("By downloading this data, you agree that:"),
        tags$ul(
          tags$li("This data is provided as-is and may be inaccurate."),
          tags$li("You assume all risk associated with its use and the Stone Living Lab is not liable for any damages associated with the use of the data."),
          tags$li("You will credit the Stone Living Lab for any research or products released that use the data.")
        )),
      
      textInput('name', "Please enter your name:"),
      textInput("org", "Please enter your organization:"),
      textInput("email", "Please enter your email:"),
      
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("confirm_download", "Agree", class = "btn-primary")
      ),
      
      easyClose = TRUE
    ))
  })
  

  download_trigger <- reactiveValues(count = 0)

  output$confirm_download <- downloadHandler(
    
    filename = function(){
      paste0("SLL_RealTime_Data_", Sys.Date(), ".zip")},
    
    content = function(file) {
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      zip_files <- c("Instrument_Data.csv", "Flooding_Data.csv")
      write.csv(combo_data(), file = "Instrument_Data.csv")
      write.csv(hohonu_data(), file = "Flooding_Data.csv")
      
      zip(file, zip_files)
      
      emails <- read.csv(file.path(data_dir, "Outputs/User_Info.csv"))
        
      updated_emails = emails %>% 
          add_row(name = input$name, 
                  org = input$org, 
                  email = input$email)
      print(input$name)  
      write.csv(updated_emails, file.path(data_dir, "Outputs/User_Info.csv"))
   
      
      }, 
    contentType = "application/zip")
  
}


# ---- Run app ----
shinyApp(ui, server)
