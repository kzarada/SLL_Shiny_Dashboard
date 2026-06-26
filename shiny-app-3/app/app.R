########################################
#
#  Storm Explorer
#
########################################


library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(fresh)
library(shinybrowser)
library(htmlwidgets)
library(plotly)

#Set Data File Path (changes for dockerfile)
data_dir = "/srv/shiny-server/Data/"
################## Read in data #####################
instrument.locations = read.csv(file.path(data_dir, "Inputs/RealTimeMonitoring_Locations.csv")) %>% 
  dplyr::select(Name, ID, Latitude, Longitude) 

instrument.map = instrument.locations %>% 
  filter(str_detect(Name, "Flood Sensor", negate = T)) %>% 
  drop_na(Latitude) %>%
  filter(Name != "Rainsford Island Buoy") %>% 
  mutate(Type = case_when(
    str_detect(Name, "Buoy") ~  'buoy', 
    str_detect(Name, "Gauge") ~ "gauge", 
    str_detect(Name, "Weather") ~ "weather", 
    .default = NA)) 


arrow_length_x <- 1800   # seconds (controls horizontal arrow size)
arrow_length_y <- 0.5   # wind-speed units (vertical size)


combo_data <- read.csv(file.path(data_dir, "Inputs/February_2026_Noreaster_combo.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))


hohonu_data <- read.csv(file.path(data_dir, "Inputs/February_2026_Noreaster_hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

compare_data_1 <- read.csv(file.path(data_dir, "Inputs/February_2026_Noreaster_combo.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
         Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)

compare_data_2 <- read.csv(file.path(data_dir, "Inputs/February_2026_Flooding_combo.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
         Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)

storms = read.csv(file.path(data_dir, "Inputs/Storm Explorer Dates.csv"))

start_time = min(hohonu_data$Time_ET, na.rm = T)
end_time = max(hohonu_data$Time_ET, na.rm = T)


#colors: 
#blue: #256EFF
#teal: #2EBBAD
#darkblue: #002366
#white

####### Create theme #############

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#4890C3"
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
  
  title = "SLL Storm Explorer", 
  
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
                menuItem("Compare Storms", tabName = 'compare', icon = icon("chart-bar")),
                menuItem("Stations", tabName = 'stations', icon = icon("water")), 
                menuItem("Instruments", tabName = 'instruments', icon = icon('cloud')),
                menuItem("Feedback", tabName = 'feedback', icon = icon("comment-dots")),
                menuItem("Contact Us", tabName = 'contact', icon = icon("square-envelope"))), 
    collapsed = TRUE),
  
  dashboardBody(use_theme(mytheme),
                
                tags$head(
                  
                  tags$link(rel = "stylesheet", type = "text/css", href = "StormExplorer_styles.css")),
                
                tags$script(HTML('$(document).ready(function() {
                                 $("header").find("nav").append(\'<span class="myClass"> SLL Storm Explorer (Beta)</span>\');})')),
                
                shinybrowser::detect(), 
                
                
                tabItems(
                  tabItem(tabName = "dashboard", 
                          fluidRow(
                            
                            selectInput(
                              "storm_select", 
                              "Select Storm:", 
                              list("February 2026 Nor'easter" = "February_2026_Noreaster", 
                                   "February 2026 Flooding" = "February_2026_Flooding", 
                                   "October 2025 Nor'easter" = "October_2025_Noreaster"), 
                              multiple = F)),
                          fluidRow(
                            column(width = 12, 
                                   box(
                                     title = "Storm Overview", 
                                     solidHeader = TRUE, 
                                     width = 12, 
                                     status = 'primary', 
                                     uiOutput("storm_overview")
                                   )
                            ),
                            
                            fluidRow(
                              column(width = 12, 
                                     sliderInput(
                                       inputId = "time",
                                       label   = "Select time:",
                                       min     = start_time,
                                       max     = end_time,
                                       value   = start_time,
                                       step    = 6 * 60,   # 10 minutes (in seconds)
                                       timeFormat = "%b %d %H:%M",
                                       animate = animationOptions(interval = 300), 
                                       width = "85%"))),
                            
                            
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title =  pickerInput(
                                       inputId = "flood.station",
                                       label = NULL, 
                                       choices = list("Boston - Border Street" = "Border.St", 
                                                      "Boston - Lewis Mall" = "Lewis.Mall",
                                                      "Boston - Long Wharf" = "Long.Wharf",
                                                      "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                                      "Boston - Tenean Beach" = "Tenean.Beach"), 
                                       selected = list("Boston - Border Street" = "Border.St", 
                                                       "Boston - Lewis Mall" = "Lewis.Mall",
                                                       "Boston - Long Wharf" = "Long.Wharf",
                                                       "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                                       "Boston - Tenean Beach" = "Tenean.Beach"), 
                                       options = list(
                                         `actions-box` = TRUE, # Adds Select All/None buttons
                                         `selected-text-format` = "count > 2" # Shows count if many selected
                                       ), 
                                       multiple = TRUE
                                     ),
                                     class = "plot-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     plotlyOutput("flood_graph", height = "100%")), 
                                   
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
                                                    "Gallops Island" = "gallops", 
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
                                                      "Rainsford Island NE" = "rainsford"),
                                       multiple = F),
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     shinyfullscreen::fullscreen_this(plotOutput("wave_plot", height = "100%"))))
                          ) #end fluid row
                  ), #end TabItem
                  
                  tabItem(tabName = "storm",
                          fluidRow(
                            box(title = "Field Observation", 
                                solidHeader = TRUE, 
                                status = 'primary', 
                                width = 12, 
                                uiOutput("field_obs")))),
                  
                  
                  tabItem(tabName = "compare", 
                          fluidRow(
                            column(
                              width = 6, 
                              selectInput(
                                "compare_1", 
                                "Select Storm 1: ", 
                                list("February 2026 Nor'easter" = "February_2026_Noreaster", 
                                     "February 2026 Flooding" = "February_2026_Flooding", 
                                     "October 2025 Nor'easter" = "October_2025_Noreaster"), 
                                multiple = F)),
                            column(
                              width = 6, 
                              selectInput(
                                "compare_2", 
                                "Select Storm 2:",
                                list("February 2026 Nor'easter" = "February_2026_Noreaster", 
                                     "February 2026 Flooding" = "February_2026_Flooding", 
                                     "October 2025 Nor'easter" = "October_2025_Noreaster"), 
                                multiple = F))),
                          
                          fluidRow(
                            column(
                              width = 12, 
                              sliderInput(
                                inputId = "compare_time",
                                label   = "Hours:",
                                min     = 0,
                                max     = max(compare_data_1$Time_Seq)/60,
                                value   = 0,
                                step    = 1,   # 10 minutes (in seconds)
                                timeFormat = "%H",
                                animate = animationOptions(interval = 300), 
                                width = "85%"))),
                          fluidRow(
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     
                                     title = selectInput(
                                       "flood.compare",
                                       label = NULL, 
                                       choices = list("Select Flood Station" = 'intro', 
                                                      "Boston - Border Street" = "Border.St", 
                                                      "Boston - Lewis Mall" = "Lewis.Mall",
                                                      "Boston - Long Wharf" = "Long.Wharf",
                                                      "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                                      "Boston - Tenean Beach" = "Tenean.Beach"),
                                       multiple = F), 
                                     
                                     class = "plot-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     shinyfullscreen::fullscreen_this(plotOutput("flood_graph_compare", height = "100%"))), 
                                   
                                   box(
                                     title =  selectInput(
                                       "wind_select_compare",
                                       label = NULL, 
                                       choices = list("Select Wind Measurement" = 'intro', 
                                                      "Wind Speed" = 'wind_speed', 
                                                      "Gust Speed" = 'gust_speed'),
                                       multiple = F),
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("wind_compare", height = "100%")))
                            ), #end column
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6",  
                                   
                                   box(title = selectInput(
                                     "tide_select_compare",
                                     label = NULL, 
                                     choices = list("Select Tide Gauge" = 'intro',
                                                    "Gallops Island" = "gallops", 
                                                    "NOAA - Boston" = 'boston', 
                                                    "NOAA - Fall River" = 'fall.river'),
                                     multiple = F), 
                                     solidHeader = TRUE, 
                                     width = 12, 
                                     class = 'plot-box',
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("tide_compare", height= '100%'))), 
                                   
                                   
                                   
                                   box(
                                     title = selectInput(
                                       "wave_select_compare",
                                       label = NULL, 
                                       choices = list("Select Wave Buoy" = "intro",
                                                      "Harbor Entrance Sig. Wave Height" = "harbor.entrance.sig", 
                                                      "Harbor Entrance Max. Wave Height" = 'harbor.entrance.max', 
                                                      "Rainsford Island NE" = 'rainsford'),
                                       multiple = F),
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     shinyfullscreen::fullscreen_this(plotOutput("wave_compare", height = "100%"))))
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
                                   "Boston - Tenean Beach" = "Tenean.Beach"), 
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
                                leafletOutput("sensor_map", height = "100%"), 
                                width = 12
                              ), 
                              
                              box(
                                title = "Flood Depth", 
                                solidHeader = TRUE, 
                                class = 'plot-box',
                                status = "primary", 
                                shinyfullscreen::fullscreen_this(plotOutput("station_flood", height = "100%")), 
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
                                   "Rainsford NE Wave Buoy" = "Rainsford.Buoy",
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
                                height = "20vh",
                                status = "primary",  
                                htmlOutput("instrument_text", height = "100%"), 
                                width = 12
                              ),
                              
                              box(
                                title = 'Instrument Location', 
                                solidHeader = TRUE, 
                                status = 'primary', 
                                class = 'map-box',
                                leafletOutput("instrument_map", height = "100%"), 
                                width = 12
                              ), 
                              
                              box(
                                title = "Instrument Data", 
                                solidHeader = TRUE, 
                                status = "primary", 
                                class = 'plot-box',
                                shinyfullscreen::fullscreen_this(plotOutput("instrument_graph", height = "100%")), 
                                width = 12
                              )
                            ) #end col
                          ) #end fluid row
                  ), #end tabItem
                  
                  tabItem(tabName = "feedback", 
                          column(width = 12, 
                                 class = "col-12 col-md-6", 
                                 box(title = "Feedback Form", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     tags$iframe(
                                       src = "https://docs.google.com/forms/d/e/1FAIpQLSeieG-47oukyQa1h7blW-jy_icmwX1H19ojj0Zext7VYiU_Iw/viewform?embedded=true", 
                                       style = "width:100%; height: 80vh;"
                                     ))
                                 
                          ) #end col
                  ), #end tabitem
                  
                  tabItem(tabName = "contact", 
                          column(width = 12, 
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
                                    Stone Foundation that engages scientists and the community in research, education, and the promotion of equity."))), 
                                 
                                 
                                 box(title = "Contact Us", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     div(p(HTML(paste0("If you have feedback on this dashboard, questions about our work, 
                                                or have noticed issues with any of our overland flood sensors or instruments,
                                                please email us at ", tags$a("info@stonelivinglab.org", 
                                                                             href = "mailto:info@stonelivinglab.org")))))), 
                                 
                                 box(title = "Keep in touch!", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     width = 12, 
                                     tags$iframe(
                                       src = "https://mailchi.mp/stonelivinglab.org/oflzp4092d", 
                                       style = "width:100%; height: 80vh;"
                                     )))) #end tabItem
                  
                ), #end tabItems
                
                tags$div(
                  class = "app-footer",
                  tags$a(
                    href = "http://147.93.47.40:8080/app/SLL_Flood_Dashboard",
                    target = "_blank",
                    HTML("<u>Click here</u> to see the SLL Flooding Dashboard |")), 
                  tags$a(
                    href = "http://147.93.47.40:8080/app/SLL_Current_Coastal_Conditions",
                    target = "_blank",
                    HTML("<u>Click here</u> to see the SLL Current Coastal Conditions Dashboard"))
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
                                                                                       target = '_blank'), "</u>")),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
  
  ########## Mobile Detection #############
  
  plot_theme <- reactive({
    if (shinybrowser::is_device_mobile()) {
      theme_bw(base_family = "Replica LL TT") +
        theme(
          axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 7),
          axis.title   = element_text(size = 9),
          legend.text  = element_text(size = 7),
          legend.title = element_blank(), 
          legend.position = "bottom", 
          margins = margin(0.5, 1, 0.5, 0.5, unit = 'cm')
        )
    } else {
      theme_bw(base_family = "Replica LL TT") +
        theme(
          axis.text  = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text  = element_text(size = 16),
          legend.title = element_blank(), 
          legend.position = "bottom", , 
          margins = margin(0.5, 1, 0.5, 0.5, unit = 'cm')
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
  
  
  combo_data <- reactive({
    read.csv(paste0(data_dir, "Inputs/", input$storm_select, "_combo.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    
  })
  
  hohonu_data <- reactive({
    read.csv(paste0(data_dir, "Inputs/",input$storm_select, "_hohonu.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    
  })
  
  
  compare_data_1 <- reactive({
    read.csv(paste0(data_dir, "Inputs/", input$compare_1, "_combo.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
             Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)
    
  })
  
  compare_data_2 <- reactive({
    read.csv(paste0(data_dir, "Inputs/", input$compare_2, "_combo.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
             Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)
    
  })
  
  
  compare_hohonu_data_1 <- reactive({
    read.csv(paste0(data_dir, "Inputs/", input$compare_1, "_hohonu.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
             Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)
    
  })
  
  compare_hohonu_data_2 <- reactive({
    read.csv(paste0(data_dir, "Inputs/",  input$compare_2, "_hohonu.csv")) %>% 
      mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
             Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
             Time_Seq = (as.numeric(Time_ET) - min(as.numeric(Time_ET)))/60)
    
  })
  
  hohonu_graph <- reactive({
    
    unit = unit_state() 
    
    hohonu_data() %>% 
      filter(Location %in% input$flood.station) %>% 
      mutate(tooltip = paste0("<strong>", Station.Name,
                              "</strong><br/> Click for station details"), 
             click = paste0("Shiny.setInputValue('go_to_tab',\'", 
                            Location, "\',{priority:'event'});"))
    
  })
  
  
  sensor_loc <- reactive({
    hohonu_data() %>% 
      filter(Location == input$station.id)
  })
  
  
  instrument_loc <- reactive({
    instrument.locations %>% filter(ID == input$instrument.id)
  })
  
  observe({
    newData = hohonu_data() 
    updateSliderInput(session, 
                      "time", 
                      min = min(newData$Time_ET), 
                      max = max(newData$Time_ET), 
                      value = min(newData$Time_ET), 
                      timeFormat = "%b %d %H:%M")
  })
  
  filtered_flood_data <- reactive({
    if(with_tz(input$time, tzone = "America/New_York") %in% unique(hohonu_data()$Time_ET)){
      hohonu_data() %>%
        filter(Time_ET == with_tz(input$time, tzone = "America/New_York"))
    }
    else{
      hohonu_data() %>% group_by(Location) %>% slice_min(Time_ET)
    }
  })
  
  
  output$storm_overview <-  renderUI({
  
    url = storms %>% filter(ID == input$storm_select) %>% pull(Link)
    
    
    if(input$storm_select == "February_2026_Noreaster"){
      tags$a(
        href = url,
        style = 'color:black;',
        target = "_blank",
        HTML(
          "The February 2026 Nor'easter was an intense blizzard generated by a coastal cyclone between the 
          late afternoon and evening hours of Sunday, February 22 into Monday, February 23. Peak storm conditions 
          occured during the late morning to mid-afternoon hours of February 23. The combination of snow accumulation, 
          high winds, and temperature drops made for very hazardous conditions. <strong>Please note that snow pack impacted flood sensor readings.</strong> <u>Click here</u> to learn more about this storm in the 
          SLL Field Observation post. Use the time slider below to explore storm conditions."))
      
    }else if(input$storm_select == "February_2026_Flooding"){
      
      tags$a(
        href = url,
        style = 'color:black;',
        target = "_blank",
        HTML(
          "King tides surrounding the full moon on February 1, 2026 in conjunction with an offshore storm resulted in overland flooding
          at Tenean Beach in Dorchester. The Boston NOAA tide gauge reached a peak of 12.54 feet MLLW at 10 am on February 1st, 
          exceeding the National Weather Service threshold for minor flooding. <strong>Please note that snow pack impacted flood sensor readings.</strong> 
          <u>Click here</u> to learn more about this storm in the 
          SLL Field Observation post. Use the time slider below to explore storm conditions."))
      
    } else if(input$storm_select == "October_2025_Noreaster"){
      
      tags$a(
        href = url,
        style = 'color:black;',
        target = "_blank",
        HTML(
          "The October 2025 Nor'easter was a multi-day storm. The storm caused strong, gusty winds that led to coastal flooding during 
        high tide cycles. Tenean Beach in Dorchester recorded flood depths between 0.5 - 1 feet. <u>Click here</u> to learn more about this storm in the 
          SLL Field Observation post. Use the time slider below to explore storm conditions."))
    }#end if else statements
    
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
    
    shiny::validate(need(wind_speed, "Data are not available from this instrument"))
    
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
    
    shiny::validate(need(wave_height, "Data are not available from this instrument"))
    
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
    
    shiny::validate(need(water_level, "Data are not available from this instrument"))
    
    water_level = if(unit == "m"){
      water_level/3.281}else{water_level}
    
    
    major = if(input$tide_select == "gallops"){
      16}
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
      14.49}
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
      12.50}
    else if(input$tide_select == "boston"){
      12.50
    }else if(input$tide_select == 'fall.river'){
      6.98
    }else if(input$tide_select == 'intro'){
      12.50
    }
    
    minor = if(unit == "m"){
      minor/3.281}else{minor}
    
    ymax = max(water_level, (major * 1.1))
    
    ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
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
                    ymin= moderate + 0.05, 
                    ymax = major, 
                    fill = "NOAA - Moderate Flooding")) + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= major + 0.05, 
                    ymax = major *1.1, 
                    fill = "NOAA - Major Flooding")) + 
      geom_line(aes(color = "Observed Water Level"), linewidth = 1) +
      scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) +
      xlab("Time (ET)") +
      ggtitle(ggtitle) +
      scale_color_manual(
        values = c("#002366")) + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical')
    
    
  }) 
  
  
  
  
  output$flood_graph <- renderPlotly({
    
    
    shiny::validate(need(hohonu_graph()$Location, 
                         "This sensor does not have data during this storm. Please select another sensor or storm."))
    
    unit = unit_state()
    
    Depth = if(unit == "m"){round(hohonu_graph()$Flood.Depth/3.281, 2)}else{hohonu_graph()$Flood.Depth}
    
    y_max = max(Depth, convert_units(1, unit_state()), na.rm = T)
    
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    
    rows = ifelse(length(unique(hohonu_graph()$Location)) > 3, 2, 1)
    
    p = ggplot(hohonu_graph(), aes(x = Time_ET, y = Depth, 
                                   color = Station.Name)) + 
      geom_line(linewidth = 1) + 
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) + 
      xlab("") + 
      ylim(c(0, y_max)) + 
      plot_theme() + 
      guides(color = guide_legend(nrow = rows))
    
    if (shinybrowser::is_device_mobile()){
      text_size = 7
      title_size = 9
      legend_size = 6
    } else{
      text_size = 16
      title_size = 18
      legend_size = 14
    }
    
    ggplotly(p, tooltip = c("color", "y", "x")) %>%
      layout(
        xaxis = list(
          tickfont = list(family = "Replica LL TT", size = text_size)),
        yaxis = list(
          tickfont = list(family = "Replica LL TT", size = text_size),
          title = list(font = list(family = "Replica LL TT", size = title_size))),
        legend = list(
          orientation = 'h', 
          x = 0.5, 
          xanchor = 'center', 
          y = -0.2,
          title = list(text = NULL),
          font = list(family = "Replica LL TT Regular", 
                      size = legend_size)),
        hoverlabel = list(
          font = list(family = "Replica LL TT"),
          bgcolor = "white",
          align = "left"
        ), 
        margins = list(
          l = 15,
          r = 15,
          b = 1, 
          t = 1, 
          pad = 10
        )) 
    
  })
  
  
  
  
  
  ############################################################### 
  ################# Compare Page Plots ##########################
  ############################################################### 
  
  output$wind_compare <- renderPlot({
    
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Wind Speed (mph)", "Wind Speed (m/s)")
    
    
    storm_1 = str_replace_all(input$compare_1, "_", " ")
    storm_2 = str_replace_all(input$compare_2, "_", " ")
    
    wind_speed_1 = 
      
      if(input$wind_select_compare == "intro"){
        
        if(unit == "m"){compare_data_1()$Wind.Speed_RMYoung_mph/2.237}else{compare_data_1()$Wind.Speed_RMYoung_mph}
        
      } else if(input$wind_select_compare == "wind_speed"){
        
        if(unit == "m"){compare_data_1()$Wind.Speed_RMYoung_mph/2.237}else{compare_data_1()$Wind.Speed_RMYoung_mph}
        
      } else if(input$wind_select_compare == 'gust_speed'){
        
        if(unit == "m"){compare_data_1()$Gust.Speed_RMYoung_mph/2.237}else{compare_data_1()$Gust.Speed_RMYoung_mph}
      }
    
    wind_speed_2 = 
      if(input$wind_select_compare == "intro"){
        
        if(unit == "m"){compare_data_2()$Wind.Speed_RMYoung_mph/2.237}else{compare_data_2()$Wind.Speed_RMYoung_mph}
        
      } else if(input$wind_select_compare == "wind_speed"){
        
        if(unit == "m"){compare_data_2()$Wind.Speed_RMYoung_mph/2.237}else{compare_data_2()$Wind.Speed_RMYoung_mph}
        
      } else if(input$wind_select_compare == 'gust_speed'){
        
        if(unit == "m"){compare_data_2()$Gust.Speed_RMYoung_mph/2.237}else{compare_data_2()$Gust.Speed_RMYoung_mph}
      }
    
    
    
    shiny::validate(need(wind_speed_1, "Data are not available from this instrument for the first storm"))
    shiny::validate(need(wind_speed_2, "Data are not available from this instrument for the second storm"))
    
    y_max = if(unit == "m"){
      max(wind_speed_1 + 1, wind_speed_2 + 1,  6.7)}else{max(wind_speed_1 + 1, wind_speed_2 + 1,  15)}
    
    ggtitle = case_when(
      input$wind_select_compare == "intro" ~ "Wind Speed at Rainsford Island",
      input$wind_select_compare == "wind_speed" ~ "Wind Speed at Rainsford Island", 
      input$wind_select_compare == "gust_speed" ~ "Gust Speed at Rainsford Island", 
      .default = NA
    )
    
    ggplot(compare_data_1(), aes(x = Time_Seq/60, y = wind_speed_1)) +
      geom_line(data = compare_data_1(), aes(x = Time_Seq/60, y = wind_speed_1, color = storm_1), linewidth = 1) +
      geom_line(data = compare_data_2(), aes(x = Time_Seq/60, y = wind_speed_2, color = storm_2), linewidth = 1) +
      geom_vline(xintercept =input$compare_time , 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylim(c(-2, y_max)) + 
      xlab("Time (hours)") + 
      ggtitle(ggtitle) + 
      ylab(y_label) +
      scale_color_manual(
        values = c("#2EBBAD", 'gray40')) +
      plot_theme() + 
      theme(plot.title = element_text(size = 18))
    
    
    
  })
  
  
  output$wave_compare <- renderPlot({
    
    unit = unit_state() 
    
    
    storm_1 = str_replace_all(input$compare_1, "_", " ")
    storm_2 = str_replace_all(input$compare_2, "_", " ")
    
    
    wave_height_1 = if(input$wave_select_compare == 'intro'){
      if(unit == "m"){compare_data_1()$Harbor_Entrance_Hs_Wave_Height_m}else{compare_data_1()$Harbor_Entrance_Hs_Wave_Height_ft}
    }else if(input$wave_select_compare == "harbor.entrance.sig"){
      if(unit == "m"){compare_data_1()$Harbor_Entrance_Hs_Wave_Height_m}else{compare_data_1()$Harbor_Entrance_Hs_Wave_Height_ft}
    } else if(input$wave_select_compare == 'harbor.entrance.max'){
      if(unit == "m"){compare_data_1()$Harbor_Entrance_Hmax_Wave_Height_m}else{compare_data_1()$Harbor_Entrance_Hmax_Wave_Height_ft}
    }  else if(input$wave_select_compare == 'rainsford'){
      if(unit == "m"){compare_data_1()$Rainsford_Hs_Wave_Height_m}else{compare_data_1()$Rainsford_Hs_Wave_Height_ft}
    } 
    
    wave_height_2 = if(input$wave_select_compare == 'intro'){
      if(unit == "m"){compare_data_2()$Harbor_Entrance_Hs_Wave_Height_m}else{compare_data_2()$Harbor_Entrance_Hs_Wave_Height_ft}
    }else if(input$wave_select_compare == "harbor.entrance.sig"){
      if(unit == "m"){compare_data_2()$Harbor_Entrance_Hs_Wave_Height_m}else{compare_data_2()$Harbor_Entrance_Hs_Wave_Height_ft}
    } else if(input$wave_select_compare == 'harbor.entrance.max'){
      if(unit == "m"){compare_data_2()$Harbor_Entrance_Hmax_Wave_Height_m}else{compare_data_2()$Harbor_Entrance_Hmax_Wave_Height_ft}
    }  else if(input$wave_select_compare == 'rainsford'){
      if(unit == "m"){compare_data_2()$Rainsford_Hs_Wave_Height_m}else{compare_data_2()$Rainsford_Hs_Wave_Height_ft}
    } 
    
    shiny::validate(need(wave_height_1, "Data are not available from this instrument for the first storm"))
    shiny::validate(need(wave_height_2, "Data are not available from this instrument for the second storm"))
    
    y_max = max(wave_height_1, wave_height_2, convert_units(2.5, unit))
    
    ggtitle = case_when(
      input$wave_select_compare == "intro" ~ "Harbor Entrance Wave Buoy",
      input$wave_select_compare == "harbor.entrance.sig" ~ "Harbor Entrance Wave Buoy - Significant Wave Height", 
      input$wave_select_compare == 'harbor.entrance.max' ~ "Harbor Entrance Wave Buoy - Maximum Wave Height", 
      input$wave_select_compare == "rainsford" ~ "Rainsford NE Wave Buoy - Significant Wave Height", 
      .default = NA
    )
    
    y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
    
    ggplot(compare_data_1(), aes(x = Time_Seq/60, y = wave_height_1)) + 
      geom_line(data = compare_data_1(), aes(x = Time_Seq/60, y = wave_height_1, color = storm_1), linewidth= 1) + 
      geom_line(data = compare_data_2(), aes(x = Time_Seq/60, y = wave_height_2, color = storm_2), linewidth = 1) + 
      geom_vline(xintercept = input$compare_time, 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (hours)") + 
      ggtitle(ggtitle) + 
      scale_color_manual(
        values = c("#2EBBAD", 'gray40')) +
      plot_theme() + 
      theme(plot.title = element_text(size = 18))
    
    
    
  })
  
  output$tide_compare <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)")
    
    
    storm_1 = str_replace_all(input$compare_1, "_", " ")
    storm_2 = str_replace_all(input$compare_2, "_", " ")
    
    ggtitle = case_when(
      input$tide_select_compare == "intro" ~ "NOAA Tide Gauge and Flood Predictions - Boston",
      input$tide_select_compare == "gallops" ~ "Gallops Tide Gauge", 
      input$tide_select_compare == "boston" ~ "NOAA Tide Gauge and Flood Predictions - Boston", 
      input$tide_select_compare == 'fall.river' ~ "NOAA Tide Gauge and Flood Predictions - Fall River", 
      .default = NA
    )
    
    water_level_1 = if(input$tide_select_compare == "gallops"){
      compare_data_1()$Gallops_Water_Level_ft}
    else if(input$tide_select_compare == "boston"){
      compare_data_1()$Boston_Water_MLLW
    }else if(input$tide_select_compare == 'fall.river'){
      compare_data_1()$Fall_River_Water_MLLW
    }else if(input$tide_select_compare == 'intro'){
      compare_data_1()$Boston_Water_MLLW
    }
    
    
    water_level_2 = if(input$tide_select_compare == "gallops"){
      compare_data_2()$Gallops_Water_Level_ft}
    else if(input$tide_select_compare == "boston"){
      compare_data_2()$Boston_Water_MLLW
    }else if(input$tide_select_compare == 'fall.river'){
      compare_data_2()$Fall_River_Water_MLLW
    }else if(input$tide_select_compare == 'intro'){
      compare_data_2()$Boston_Water_MLLW
    }
    
    shiny::validate(need(water_level_1, "Data are not available from this instrument for Storm 1"))
    shiny::validate(need(water_level_2, "Data are not available from this instrument for Storm 2"))
    
    water_level_1 = if(unit == "m"){
      water_level_1/3.281}else{water_level_1}
    
    water_level_2 = if(unit == "m"){
      water_level_2/3.281}else{water_level_2}
    
    
    major = if(input$tide_select_compare == "gallops"){
      16}
    else if(input$tide_select_compare == "boston"){
      16
    }else if(input$tide_select_compare == 'fall.river'){
      11.98
    }else if(input$tide_select_compare == 'intro'){
      16
    }
    
    major = if(unit == "m"){
      major/3.281}else{major}
    
    moderate = if(input$tide_select_compare == "gallops"){
      14.49}
    else if(input$tide_select_compare == "boston"){
      14.49
    }else if(input$tide_select_compare == 'fall.river'){
      9.48
    }else if(input$tide_select_compare == 'intro'){
      14.49
    }
    
    moderate = if(unit == "m"){
      moderate/3.281}else{moderate}
    
    minor = if(input$tide_select_compare == "gallops"){
      12.50}
    else if(input$tide_select_compare == "boston"){
      12.50
    }else if(input$tide_select_compare == 'fall.river'){
      6.98
    }else if(input$tide_select_compare == 'intro'){
      12.50
    }
    
    minor = if(unit == "m"){
      minor/3.281}else{minor}
    
    ymax = max(water_level_1, water_level_2, (major * 1.1))
    
    ggplot(compare_data_1(), aes(x = Time_Seq/60, y = water_level_1)) + 
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
                    ymin= moderate + 0.05, 
                    ymax = major, 
                    fill = "NOAA - Moderate Flooding")) + 
      geom_rect(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= major + 0.05, 
                    ymax = major *1.1, 
                    fill = "NOAA - Major Flooding")) + 
      geom_line(data = compare_data_1(), aes(x = Time_Seq/60, y = water_level_1, color = paste0("Water level for ", storm_1)), linewidth = 1) +
      geom_line(data = compare_data_2(), aes(x = Time_Seq/60, y = water_level_2, color =paste0("Water level for ", storm_2)), linewidth = 1, linetype = 'dashed') +
      scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
      geom_vline(xintercept = input$compare_time , 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) +
      xlab("Time (hours)") + 
      ggtitle(ggtitle) +
      scale_color_manual(
        values = c("#2EBBAD", 'gray40')) + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical')
    
    
  }) 
  
  
  
  
  output$flood_graph_compare <- renderPlot({
    
    
    flood.data.1 = if(input$flood.compare == 'intro'){
      compare_hohonu_data_1() %>% filter(Location == "Border.St") 
    }else if(input$flood.compare == 'Border.St'){
      compare_hohonu_data_1() %>% filter(Location == "Border.St") 
    }else if(input$flood.compare == "Lewis.Mall"){
      compare_hohonu_data_1() %>% filter(Location == "Lewis.Mall") 
    }else if(input$flood.compare == "Long.Wharf"){
      compare_hohonu_data_1() %>% filter(Location == "Long.Wharf")
    }else if(input$flood.compare == "Morrissey.Blvd"){
      compare_hohonu_data_1() %>% filter(Location == "Morrissey.Blvd") 
    }else if(input$flood.compare == "Tenean.Beach"){
      compare_hohonu_data_1() %>% filter(Location == "Tenean.Beach") 
    }
    
    flood.data.2 = if(input$flood.compare == 'intro'){
      compare_hohonu_data_2() %>% filter(Location == "Border.St") 
    }else if(input$flood.compare == 'Border.St'){
      compare_hohonu_data_2() %>% filter(Location == "Border.St") 
    }else if(input$flood.compare == "Lewis.Mall"){
      compare_hohonu_data_2() %>% filter(Location == "Lewis.Mall") 
    }else if(input$flood.compare == "Long.Wharf"){
      compare_hohonu_data_2() %>% filter(Location == "Long.Wharf") 
    }else if(input$flood.compare == "Morrissey.Blvd"){
      compare_hohonu_data_2() %>% filter(Location == "Morrissey.Blvd") 
    }else if(input$flood.compare == "Tenean.Beach"){
      compare_hohonu_data_2() %>% filter(Location == "Tenean.Beach") 
    }
    
    
    shiny::validate(need(flood.data.1$Flood.Depth, 
                         "This sensor does not have data during the first storm. Please select another sensor or storm."))
    
    shiny::validate(need(flood.data.2$Flood.Depth, 
                         "This sensor does not have data during the second storm. Please select another sensor or storm."))
    unit = unit_state()
    
    Depth_1 = if(unit == "m"){round(flood.data.1$Flood.Depth/3.281, 2)}else{flood.data.1$Flood.Depth}
    Depth_2 = if(unit == "m"){round(flood.data.2$Flood.Depth/3.281, 2)}else{flood.data.2$Flood.Depth}
    
    storm_1 = str_replace_all(input$compare_1, "_", " ")
    storm_2 = str_replace_all(input$compare_2, "_", " ")
    
    y_max = max(Depth_1, Depth_2, convert_units(1, unit_state()), na.rm = T)
    
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    
    ggplot(flood.data.1, aes(x = Time_Seq/60, y = Depth_1)) + 
      geom_line(data = flood.data.1, aes(x = Time_Seq/60, y = Depth_1, color = storm_1), linewidth= 1) + 
      geom_line(data = flood.data.2, aes(x = Time_Seq/60, y = Depth_2, color = storm_2), linewidth = 1) + 
      geom_vline(xintercept = input$compare_time, 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (hours)") + 
      scale_color_manual(
        values = c("#2EBBAD", 'gray40')) +
      plot_theme() + 
      theme(plot.title = element_text(size = 18))
    
    
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
    
    shiny::validate(need(depth, "Data are not available from this instrument"))
    
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
    
    if(input$instrument.id %in% c("Boston.Tide", "Fall.River.Tide", "Gallops.Tide", "Essex.Tide")){
      
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
    
    
    if(input$instrument.id == "Harbor.Entrance"){
      
      unit = unit_state() 
      
      wave_height =  if(unit == "m"){combo_data()$Harbor_Entrance_Hs_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hs_Wave_Height_ft}
      max_wave = if(unit == "m"){combo_data()$Harbor_Entrance_Hmax_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hmax_Wave_Height_ft}
      
      shiny::validate(need(wave_height, "Data are not available from this instrument"))
      
      y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
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
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Wind Speed (mph)", "Wind Speed (m/s)")
      
      wind_speed = if(unit == "m"){
        combo_data()$Wind.Speed_RMYoung_mph/2.237}else{combo_data()$Wind.Speed_RMYoung_mph}
      gust_speed = if(unit == "m"){combo_data()$Gust.Speed_RMYoung_mph/2.237}else{combo_data()$Gust.Speed_RMYoung_mph}
      
      y_max = if(unit == "m"){
        max(gust_speed + 1, 6.7)}else{max(gust_speed + 1, 15)}
      
      shiny::validate(need(wind_speed, "Data are not available from this instrument"))
      
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
      
    }
    else if(input$instrument.id == "Gallops.Tide"){
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = combo_data()$Gallops_Water_Level_ft
      
      if(unit == "m"){
        water_level/3.281}else{water_level}
      
      shiny::validate(need(water_level, "Data are not available from this instrument"))
      
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
      
      water_level = if(unit == "m"){combo_data()$Boston_Water_MLLW/3.281}else{combo_data()$Boston_Water_MLLW}
      
      shiny::validate(need(water_level, "Data are not available from this instrument"))
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        ylab(y_label) +
        xlab("Time (ET)") + 
        ggtitle("NOAA Tide Gauge and Flood Predictions - Boston") + 
        scale_color_manual(
          values = c("#002366", "#2E3440")) + 
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
        geom_line(aes(color = "Water Level"), linewidth = 1) +
        scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
        plot_theme() + 
        theme(legend.box = 'vertical')
      
      
    }
    else if(input$instrument.id == "Fall.River.Tide"){
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = if(unit == "m"){combo_data()$Fall_River_Water_MLLW/3.281}else{combo_data()$Fall_River_Water_MLLW}
      
      shiny::validate(need(water_level, "Data are not available from this instrument"))
      
      
      major = if(unit == "m"){
        11.98/3.281}else{11.98}
      
      moderate = if(unit == "m"){
        9.48/3.281}else{9.48}
      
      minor = if(unit == "m"){
        6.98/3.281}else{6.98}
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        ylab(y_label) +
        ggtitle("NOAA Tide Gauge and Flood Predictions - Fall River") + 
        xlab("Time (ET)") + 
        scale_color_manual(
          values = c("#002366", "#2E3440")) +  
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
        geom_line(aes(color = "Water Level"), linewidth = 1) +
        scale_fill_manual(values = c("#F28FDB", "#F6C871", "#EE7E6D")) + 
        plot_theme() + 
        theme(legend.box = 'vertical')
      
    }
    else if(input$instrument.id == "North.Shore"){
      unit = unit_state() 
      
      wave_height = if(unit == "m"){combo_data()$North_Shore_Hs_Wave_Height_m}else{combo_data()$North_Shore_Hs_Wave_Height_ft}
      max_height = if(unit == "m"){combo_data()$North_Shore_Hmax_Wave_Height_m}else{combo_data()$North_Shore_Hmax_Wave_Height_ft}
      
      shiny::validate(need(wave_height, "Data are not available from this instrument"))
      
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
      
      shiny::validate(need(wave_height, "Data are not available from this instrument"))
      
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
  

  
}


# ---- Run app ----
shinyApp(ui, server)
