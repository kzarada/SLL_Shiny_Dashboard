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
library(shinybrowser)
library(shinyWidgets)
library(ggiraph)

#Set Data File Path (changes for dockerfile)
data_dir = "/srv/shiny-server/Data/"

source(file.path(data_dir, "Inputs/api_keys.R"))


################## Read in data #####################
instrument.locations = data.table::fread(file.path(data_dir, "Inputs/RealTimeMonitoring_Locations.csv")) %>% 
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


time = data.table::fread(file.path(data_dir, "Outputs/map_hohonu.csv"), select = 'Time_ET', tz = "")  
time$Time_ET <- lubridate::ymd_hms(time$Time_ET, tz = "America/New_York")

start_time = round_date(min(time$Time_ET, na.rm = T), "10 mins")
end_time = max(time$Time_ET, na.rm = T)


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
    light_blue = "#14635B"
  ),
  adminlte_sidebar(
    width = "200",
    dark_bg = "#c8cdd7",
    dark_hover_bg = "#256EFF",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#e9edf3",
    box_bg = "#FFFF", 
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
      "#F59115"
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

info_button <- function(id) {
  actionButton(
    inputId = id,
    label = NULL,
    icon = icon("info-circle"),
    class = "info-button"
  )
}

compass_direction <- function(direction) {
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  directions[((direction + 22.5) %% 360 %/% 45) + 1]
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
                menuItem("Compare Instruments", tabName = 'compare', icon = icon("chart-bar")),
                menuItem("Stations", tabName = 'stations', icon = icon("water")), 
                menuItem("Instruments", tabName = 'instruments', icon = icon('cloud')),
                menuItem("Data Download", tabName = "download", icon = icon("download")), 
                menuItem("Feedback", tabName = 'feedback', icon = icon("comment-dots")),
                menuItem("Contact Us", tabName = 'contact', icon = icon("square-envelope"))), 
    collapsed = TRUE),
  
  dashboardBody(use_theme(mytheme),
                
                tags$head(
                  tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-T3BSSCMHRC"),
                  tags$script(HTML("
                                      window.dataLayer = window.dataLayer || [];
                                      function gtag(){dataLayer.push(arguments);}
                                      gtag('js', new Date());
                                      gtag('config', 'G-T3BSSCMHRC');
                                    
                                      /* Allows normal vertical scrolling even if touching inside the girafe plot */
                                    .ggiraph-svg {
                                      touch-action: pan-y !important;}

                                    $(document).on('shiny:value', function(event) {
                                      setTimeout(function() {
                                        $('svg.ggiraph-svg').attr('preserveAspectRatio', 'none');
                                      }, 100);
                                    }); "
                                  )), 
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
                              step    = 10 * 60,   # 10 minutes (in seconds)
                              timeFormat = "%b %d %H:%M",
                              animate = animationOptions(interval = 300), 
                              width = "85%"),
                           
                            
                             column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title = tagList(
                                       "Flood and Instrument Map", 
                                       info_button("flood_info")), 
                                     class = "map-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     leafletOutput("flood_map", height = "100%", width = "100%")), 
                                   
                                   box(
                                     title = div(
                                       span("Wind Speed at Rainsford Island"),
                                      info_button('wind_info')
                                     ),
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                    girafeOutput("wind_plot", height = "100%", width = "100%")), 
                                   
                                   box(
                                     title = div(
                                       span("Air Temperature at Rainsford Island"),
                                       info_button("temp_info")),
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                     girafeOutput("temp_plot", height = "100%", width = "100%"))
                            ), #end column
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6",  
                                   
                                   box(title = div(
                                     span(
                                     selectInput(
                                     "tide_select",
                                     label = NULL, 
                                     choices = list("Gallops Island" = "gallops",
                                                    "Cathleen Stone Island" = "csi",
                                                    "Essex - Main St." = 'essex',
                                                    "NOAA - Boston" = 'boston', 
                                                    "NOAA - Fall River" = 'fall.river', 
                                                    "NOAA - Woods Hole" = 'falmouth'),
                                     multiple = F)),
                                     info_button("tide_info")), 
                                     solidHeader = TRUE, 
                                     width = 12, 
                                     class = 'plot-box',
                                     status = 'primary',
                                     girafeOutput("tide_plot", height= '100%', width = "100%")), 

                                   box(
                                     title = div(
                                       span(
                                        selectInput(
                                           "wave_select",
                                           label = NULL, 
                                           choices = list("Harbor Entrance" = "harbor.entrance", 
                                                          "North Shore" = 'north.shore'),
                                           multiple = F)),
                                       info_button("wave_info")), 
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     girafeOutput("wave_plot", height = "100%", width = "100%")), 
                                   
                                   box(
                                     title = div(
                                       span("Air Pressure at Rainsford Island"),
                                       info_button("pressure_info")), 
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     girafeOutput("air_plot", height = "100%", width = "100%"))) 
                          ) #end fluid row
                  ), #end TabItem
                  
                  
                  tabItem(tabName = "compare", 
                          fluidRow(
                            column(width = 12, 
                                   box(
                                     
                                     title = pickerInput(
                                       "flood.compare",
                                       label = NULL, 
                                       choices = list("Boston - Border Street" = "Border.St", 
                                                      "Boston - Cathleen Stone Island" = "CSI", 
                                                      "Boston - Lewis Mall" = "Lewis.Mall",
                                                      "Boston - Long Wharf" = "Long.Wharf",
                                                      "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                                      "Boston - Tenean Beach" = "Tenean.Beach",
                                                      "Essex - Main Street" = "Essex", 
                                                      "Fall River - Stafford Square" = "Fall.River", 
                                                      "Falmouth - Surf Dr." = 'Falmouth', 
                                                      "Marshfield - Ocean St." = 'Marshfield', 
                                                      "Oak Bluffs - Lake Ave" = "Oak.Bluffs",
                                                      "Salem - Collin's Cove" = "Salem", 
                                                      "Wareham - Besse Park" = "Wareham"), 
                                       selected = list("Boston - Border Street" = "Border.St", 
                                                       "Boston - Cathleen Stone Island" = "CSI", 
                                                       "Boston - Lewis Mall" = "Lewis.Mall"), 
                                       options = list(
                                         `actions-box` = TRUE, # Adds Select All/None buttons
                                         `selected-text-format` = "count > 3" # Shows count if many selected
                                       ), 
                                       multiple = TRUE
                                     ),  
                                     
                                     class = "flood-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     girafeOutput("flood_graph_compare", height = "100%")))), 
                                   
                          
                          fluidRow(
                            column(width = 6, 
                                 class = "col-12 col-md-6",  
                                 
                                 box(title = pickerInput(
                                   "tide_select_compare",
                                   label = NULL, 
                                   choices = list("Cathleen Stone Island" = 'CSI', 
                                                  "Gallops Island" = "Gallops", 
                                                  "Essex - Main St." = "Essex", 
                                                  "NOAA - Boston" = 'Boston', 
                                                  "NOAA - Fall River" = 'Fall River', 
                                                  "NOAA - Woods Hole" = 'Falmouth'),
                                   selected = list("Gallops Island" = "Gallops", 
                                                   "Essex - Main St." = "Essex", 
                                                   "NOAA - Boston" = 'Boston'), 
                                   options = list(
                                     `actions-box` = TRUE, # Adds Select All/None buttons
                                     `selected-text-format` = "count > 2" # Shows count if many selected
                                   ),
                                   multiple = TRUE), 
                                   solidHeader = TRUE, 
                                   width = 12, 
                                   class = 'plot-box',
                                   status = 'primary',
                                   girafeOutput("tide_compare", height= '100%'))), 
                                
                                 
                              column(width = 6, 
                                     class = "col-12 col-md-6",  
                                box(
                                   title = selectInput(
                                     "wave_select_compare",
                                     label = NULL, 
                                     choices = list("Significant Wave Height" = "significant", 
                                                    "Maximum Wave Height" = "maximum"),
                                     multiple = F),
                                   solidHeader = TRUE,
                                   class = 'plot-box',
                                   status = 'primary',
                                   width = 12,
                                   girafeOutput("wave_compare", height = "100%")))

                          ) #end fluid row
                  ), #end TabItem
                  
                          
                      tabItem(tabName = "stations", 
                          fluidRow(
                            selectInput(
                              "station.id", 
                              "Select Station:", 
                              list("Boston - Border Street" = "Border.St", 
                                   "Boston - Cathleen Stone Island" = "CSI", 
                                   "Boston - Lewis Mall" = "Lewis.Mall",
                                   "Boston - Long Wharf" = "Long.Wharf",
                                   "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                   "Boston - Tenean Beach" = "Tenean.Beach",
                                   "Essex - Main Street" = "Essex", 
                                   "Fall River - Stafford Square" = "Fall.River", 
                                   "Falmouth - Surf Dr." = "Falmouth", 
                                   "Marshfield - Ocean St." = "Marshfield",
                                   "Oak Bluffs - Lake Ave" = "Oak.Bluffs",
                                   "Salem - Collin's Cove" = "Salem", 
                                   "Wareham - Besse Park" = "Wareham"), 
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
                              
                              box(
                                title = 'Sensor Information', 
                                solidHeader = TRUE, 
                                class = 'text-box',
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
                                   "Cathleen Stone Island Tide Gauge" = "CSI.Tide",
                                   "Essex - Main St. Tide Gauge" = "Essex.Tide",
                                   "Harbor Entrance Wave Buoy" = "Harbor.Entrance", 
                                   "North Shore Wave Buoy" = "North.Shore", 
                                   #"Rainsford NE Wave Buoy" = "Rainsford.Buoy",
                                   "Rainsford Island Weather Station" = "Rainsford.Weather", 
                                  "Woods Hole NOAA Tide Gauge" = 'Falmouth.Tide'), 
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
                    href = "https://stonelivinglab.online/SLL_Flood_Dashboard/",
                    target = "_blank",
                    HTML("Only interested in flooding? <u>Click here</u> to see the SLL Flooding Dashboard"))
                ) #end footer
  ) #end dashbody
) #end ui 



# ---- Server ----
server <- function(input, output, session) {
  
  ################## Popup ################## 
  showModal(modalDialog(
    title = "Welcome to the Stone Living Lab Current Coastal Conditions Dashboard!",
    HTML(paste0("This dashboard displays data from our real-time monitoring sensors. 
    For more information on how to navigate the dashboard, please see our <u>", tags$a("dashboard user guide.", 
                                                                                       href = "https://canva.link/yne2ktj8dnz2c81", 
                                                                                       target = '_blank'), "</u>")),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
  
  observeEvent(input$flood_info, {
    showModal(
      modalDialog(
        title = "Flood and Instrument Map",
        
        p(
          "This map displays the real-time flood depth at each of our overland flood sensors. Click the dots to learn more about the station and current flood depth. Zoom in to view the locations of the other instruments (e.g., wave buoys and tide gauges). "),

        p("A gray circle indicates that data are not currently available for a given sensor. Flood data are collected in real-time. Data may be temporarily unavailable for many reasons including missed data transmissions, temporary loss of cellular connection, or sensor errors (e.g., objects underneath sensors, unusually high data points). Sensors typically transmit data every 4 to 12 minutes. Any missed connections are backfilled once connection is restored, but sensor errors will remain as missing data."),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  observeEvent(input$wind_info, {
    showModal(
      modalDialog(
        title = "Wind Speed Graph",
        
        p(
          "This graph shows the gust speed (blue), wind speed (green), and wind direction (arrows) from the weather station on Rainsford Island. Wind speed is the rate at which the air is moving horizontally past a fixed point. Gusts are rapid fluctuations in wind. The arrows indicate the direction the wind is traveling (e.g., an arrow pointing towards the y-axis is wind blowing from the east). Click on the graph to see a full-screen version."),

        p("Wind speed and direction influence waves which can impact coastal flooding. For example, high wind speeds that are traveling towards the shore might lead to flooding during a high tide compared to lower wind speeds or wind traveling east."),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  
  observeEvent(input$tide_info, {
    showModal(
      modalDialog(
        title = "Tide Graph",
        
      p("This graph shows the water level in feet or meters above mean lower low water (learn more about tidal datums", a(href = "https://tidesandcurrents.noaa.gov/datum_options.html", "here)."), " The yellow, orange, and purple lines represent the NOAA coastal flood thresholds for minor, moderate, and major flood advisories (advisory definitions and examples can be found ", a(href= "https://www.weather.gov/images/okx/coastalflood/Coastal%20Flood%20Threshold%20Definitions.png", "here).")),

      p("Use the drop down menu to explore different tide gauge locations."),
        
     
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$pressure_info, {
    showModal(
      modalDialog(
        title = "Air Pressure Graph",
        
        p("This graph shows the air pressure measured at the weather station on Rainsford Island."),

        p("Air pressure is an important metric for understanding changing weather conditions and the severity of storms. Dropping air pressure typically indicates an approaching storm system, rising air pressure indicates improving weather conditions, and stable air pressure indicates stable weather conditions. Significant drops in air pressure can also indicate severe weather systems or explosive storm developments such as ", a(href = "https://weather.com/science/weather-explainers/news/2025-11-12-what-is-a-bomb-cyclone-and-why-is-it-so-powerful", "bomb cyclones.")), 
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$flood_info, {
    showModal(
      modalDialog(
        title = "Flood and Instrument Map",
        
        p("This map displays the real-time flood depth at each of our overland flood sensors. Click the dots to learn more about the station and current flood depth. Zoom in to view the locations of the other instruments (e.g., wave buoys and tide gauges)."),

        p("A gray circle indicates that data are not currently available for a given sensor. Flood data are collected in real-time. Data may be temporarily unavailable for many reasons including missed data transmissions, temporary loss of cellular connection, or sensor errors (e.g., objects underneath sensors, unusually high data points). Sensors typically transmit data every 4 to 12 minutes. Any missed connections are backfilled once connection is restored, but sensor errors will remain as missing data."),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$temp_info, {
    showModal(
      modalDialog(
        title = "Air Temperature Graph",
        
        p("This graph shows the air temperature measured at the weather station on Rainsford Island. Use the toggle in the right hand corner of the Dashboard to change the units from Fahrenheit to Celsius."),

        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$wave_info, {
    showModal(
      modalDialog(
        title = "Wave Graph",
        
       p("This graph shows the maximum and significant wave height at different wave buoy locations. Maximum wave height is the single tallest wave during a sampling period while the significant wave height is the average height of the largest third of waves during a sampling period."),

       p("Use the drop down menu to explore different wave buoy locations."),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  ########## Mobile Detection #############
  
  plot_theme <- reactive({
    if (shinybrowser::get_width() < 1000) {
      theme_bw(base_family = "Replica LL TT") +
        theme(
          axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 7),
          axis.title   = element_text(size = 9),
          legend.text  = element_text(size = 7),
          legend.title = element_blank(), 
          legend.position = "bottom", 
          plot.title = element_text(size = 10), 
          plot.margin = margin(0.1,1,0.1,0.1, "cm") #t,r,b,l
        )
    } else {
      theme_bw(base_family = "Replica LL TT") +
        theme(
          axis.text  = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.text  = element_text(size = 14),
          legend.title = element_blank(), 
          legend.position = "bottom", 
          plot.margin = margin(0.5,1,0.5,1, "cm")
        )
    }
  })

  graph_width = reactive({
    if (shinybrowser::get_width() < 1000) {
      4
    } else {
      9
    }
  })

  flood_graph_width = reactive({
    if (shinybrowser::get_width() < 1000) {
      4
    } else {
      18
    }
  })


    graph_height = reactive({
    if (shinybrowser::get_width() < 1000) {
      4
    } else {
      5
    }
  })
  ########### Unit Toggle ####################
  
  unit_state <- reactive({ifelse(input$unit_toggle, "m", "ft")})
  
  observeEvent(input$unit_toggle, {
    unit_state = ifelse(input$unit_toggle, "m", "ft")
    
    updateActionButton(
      session, 
      "unit_toggle",
      label = unit_state()
    )
  })
  

  
  ################ Data Downloads - reactive file reader to update ####################
  
 combo_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/combo.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) |> 
      arrange(Time_ET) |> 
      mutate(data_id = seq_len(length(Time_ET)))
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
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
        mutate(Last_Available = as.character(Last_Available), 
               Last_Available = replace_na(Last_Available, "")) 
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
  
  #### Reactive Statements to Filter Data #####
   # Debounce time input by 250ms to prevent rapid re-render spikes
    selected_time <- reactive({ round_date(input$time, unit = 'mins') }) %>% debounce(250)
  
  
  
  filtered_flood_data <- reactive({
    
    map_hohonu_data() %>% filter(Time_ET == round_date(with_tz(input$time, tzone = "America/New_York"), unit = "mins"))
    
  })
  
  sensor_loc <- reactive({
    hohonu_data() %>% filter(Location == input$station.id)
  })
  
  
  instrument_loc <- reactive({
    instrument.locations %>% filter(ID == input$instrument.id)
  })


 
  
  ############# Wind Direction ################
  
  wind_dir <- reactive({
    
    req(combo_data())

    combo_data() %>% 
    mutate(Time_ET = round_date(Time_ET, unit = "hour")) %>% 
    group_by(Time_ET) %>% 
    summarise(Mean_Wind_Dir = mean(Wind.Direction_RMYoung_deg)) %>% 
    ungroup() %>%
    mutate(
        # Convert meteorological direction to direction wind is traveling toward
        dir_to = (Mean_Wind_Dir + 180) %% 360,

        # Convert compass bearing to radians
        dir_rad = dir_to * pi / 180,

        arrow_y = -1,

        # Compass bearing: 0 = North, 90 = East
        arrow_xend = Time_ET + arrow_length_x * sin(dir_rad),
        arrow_yend = arrow_y + arrow_length_y * cos(dir_rad),

        # Compass direction must use the same "toward" bearing
        compass = compass_direction(dir_to)
)
  })
  

  
  ################# Main Page Plots ##########################
  
  output$wind_plot <- renderGirafe({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Wind Speed (mph)", "Wind Speed (m/s)")
    tool_label = ifelse(unit == 'ft', "mph", "m/s")
    
    wind_speed = if(unit == "m"){
      combo_data()$Wind.Speed_RMYoung_mph/2.237}else{combo_data()$Wind.Speed_RMYoung_mph}
    gust_speed = if(unit == "m"){combo_data()$Gust.Speed_RMYoung_mph/2.237}else{combo_data()$Gust.Speed_RMYoung_mph}
    
    
    y_max = if(unit == "m"){
      max(gust_speed + 1, 6.7)}else{max(gust_speed + 1, 15)}
    
    shiny::validate(need(wind_speed, "Data are not available from this instrument"))
    
    
    p = ggplot(combo_data(), aes(x = Time_ET, y = wind_speed)) +
      geom_line(aes(x = Time_ET, y = wind_speed, color = "Wind Speed"), linewidth = 1) +
       geom_point_interactive(data = combo_data(), aes(
                   x = Time_ET,
                   y = wind_speed, 
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nWind Speed: ",
                            sprintf("%.2f", wind_speed),
                            tool_label
                            ),
                  data_id = paste0('wind_', seq(1, length(wind_speed)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      geom_line(aes(x = Time_ET, y = gust_speed, color = "Gust Speed"), linewidth = 1) +
       geom_point_interactive(data = combo_data(), aes(
                   x = Time_ET,
                   y = gust_speed, 
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nGust Speed: ",
                            sprintf("%.2f", gust_speed),
                            tool_label
                            ),
                  data_id = paste0('gust_', seq(1, length(gust_speed)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylim(c(-2, y_max)) + 
      geom_segment_interactive(data = wind_dir(), 
                   aes(xend = arrow_xend, 
                       y = arrow_y, 
                       yend = arrow_yend, 
                       color = "Wind Direction", 
                       tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nWind is blowing towards the ",
                            compass
                            ),
                   data_id = paste0('dir_', seq(1, length(wind_dir()$arrow_y)))), 
                   arrow = arrow(length = unit(0.15, 'cm')), 
                  hover_nearest = TRUE) + 
      xlab("Time (ET)") + 
      ylab(y_label) +
      scale_color_manual(
        values = c("#256EFF", "#002366", "#14635B")) +
      plot_theme()
    
    
    girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
    
  })
  
  
  output$wave_plot <- renderGirafe({
    
    unit = unit_state() 
    
    wave_height =  if(input$wave_select == "harbor.entrance"){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hs_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hs_Wave_Height_ft}
    } else if(input$wave_select == "north.shore"){
      if(unit == "m"){combo_data()$North_Shore_Hs_Wave_Height_m}else{combo_data()$North_Shore_Hs_Wave_Height_ft}
    }
    
    max_height = if(input$wave_select == "harbor.entrance"){
      if(unit == "m"){combo_data()$Harbor_Entrance_Hmax_Wave_Height_m}else{combo_data()$Harbor_Entrance_Hmax_Wave_Height_ft}
    }  else if(input$wave_select == "north.shore"){
      if(unit == "m"){combo_data()$North_Shore_Hmax_Wave_Height_m}else{combo_data()$North_Shore_Hmax_Wave_Height_ft}
    }
    
    y_max = max(max_height, convert_units(2.5, unit))
    
    shiny::validate(need(wave_height, "Data are not available from this instrument"))
  
    
    y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
    tool_label = ifelse(unit == 'ft', "ft", "m")
    
   p =  ggplot(combo_data(), aes(x = Time_ET, y = wave_height)) + 
      geom_line(aes(color = "Significant Wave Height"), linewidth= 1) + 
      geom_point_interactive(data = combo_data(), aes(
                   x = Time_ET,
                   y = wave_height, 
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nSignificant Wave Height: ",
                            sprintf("%.2f", wave_height),
                            tool_label
                            ),
                  data_id = paste0('wind_', seq(1, length(wave_height)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      geom_line(aes(x = Time_ET, y = max_height, color = "Maximum Wave Height"), linewidth = 1) + 
      geom_point_interactive(data = combo_data(), aes(
                   x = Time_ET,
                   y = max_height, 
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nMaximum Wave Height: ",
                            sprintf("%.2f", max_height),
                            tool_label
                            ),
                  data_id = paste0('max_', seq(1, length(max_height)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (ET)") + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      scale_color_manual(
        values = c("#256EFF","#14635B")) + 
      plot_theme()
    
    girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
  })
  
  output$tide_plot <- renderGirafe({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)")
    tool_label = ifelse(unit == 'ft', "ft, MLLW", "m, MLLW")
    
    
    water_level = if(input$tide_select == "gallops"){
      combo_data()$Gallops_Water_Level_ft}
    else if(input$tide_select == 'csi'){
      combo_data()$CSI_Water_Level_ft
    }
    else if(input$tide_select == "boston"){
      combo_data()$Boston_Water_MLLW
    }else if(input$tide_select == 'fall.river'){
      combo_data()$Fall_River_Water_MLLW
    }else if(input$tide_select == 'essex'){
      combo_data()$Essex_Water_Level_ft
    }else if(input$tide_select == 'intro'){
      combo_data()$Gallops_Water_Level_ft
    } else if(input$tide_select == 'falmouth'){
      combo_data()$Falmouth_Water_MLLW
    }
    
    water_level = if(unit == "m"){
      water_level/3.281}else{water_level}
    
    shiny::validate(
          shiny::need(
            length(water_level) > 0 && any(!is.na(water_level)),
            "Data are not available from this instrument"
          )
        )
    
    prediction = if(input$tide_select %in% c("gallops", 'csi', 'essex')){
      NA}
    else if(input$tide_select == "boston"){
      tide_pred()$Boston_Water_Prediction
    }else if(input$tide_select == 'fall.river'){
      tide_pred()$Fall_River_Water_Prediction
    }  else if(input$tide_select == 'falmouth'){
      tide_pred()$Falmouth_Water_Prediction
    }    
    prediction = if(unit == "m"){
      prediction/3.281}else{prediction}
    

    major = if(input$tide_select %in% c("gallops", 'csi', 'boston')){
      16
    } else if(input$tide_select == 'fall.river'){
      11.98
    } else if(input$tide_select == 'essex') {
      NA
    } else if(input$tide_select == 'falmouth'){
      8.50
    }
    
    major = if(unit == "m"){
      major/3.281}else{major}
    
    moderate = if(input$tide_select %in% c("gallops", "csi", "boston")){
      14.49}
    else if(input$tide_select == 'fall.river'){
      9.48
    } else if(input$tide_select == 'essex'){
      NA
    } else if(input$tide_select == 'falmouth'){
      7.01
    }
    
    moderate = if(unit == "m"){
      moderate/3.281}else{moderate}
    
    minor = if(input$tide_select %in% c("gallops", 'csi', "boston")){
      12.50}
    else if(input$tide_select == 'fall.river'){
      6.98
    }else if(input$tide_select == 'intro'){
      12.50
    } else if(input$tide_select == 'essex'){
      NA
    } else if(input$tide_select == 'falmouth'){
      5.50
    }
    
    minor = if(unit == "m"){
      minor/3.281}else{minor}
    
    ymax = max(water_level, (major * 1.1))
    
    p = ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
      geom_rect_interactive(
                aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= minor, 
                    ymax = moderate, 
                    fill = "NOAA - Minor Flooding", 
                    tooltip = paste0("NOAA - Minor Flooding Threshold: ", minor))) + 
      geom_rect_interactive(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= moderate + 0.05, 
                    ymax = major, 
                    fill = "NOAA - Moderate Flooding",
                   tooltip = paste0("NOAA - Moderate Flooding Threshold: ", moderate))) + 
      geom_rect_interactive(aes(xmin = -Inf, 
                    xmax = Inf, 
                    ymin= major + 0.05, 
                    ymax = major *1.1, 
                    fill = "NOAA - Major Flooding", 
                    tooltip = paste0("NOAA - Major Flooding Threshold: ", major))) + 
      geom_line(aes(color = "Actual Water Level"), linewidth = 1) +
      geom_point_interactive(aes(
                  x = Time_ET,
                   y = water_level, 
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nObserved Water Level: ",
                            sprintf("%.2f", water_level),
                            tool_label
                            ),
                  data_id = data_id),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level"), linetype = 'dotted', linewidth =1) + 
      {if(length(prediction) > 1) geom_point_interactive(data = tide_pred(), aes(
                  x = Time_ET, 
                  y = prediction,
                  tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nPredicted Water Level: ",
                            sprintf("%.2f", prediction),
                            tool_label
                            ),
                  data_id = paste0("predicted_", seq(1, length(prediction)))),
                size = 1,
                alpha = 0,
                hover_nearest = TRUE)} + 
      scale_fill_manual(values = c("#8F62FF", "#F6C871", "#EE7E6D")) + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) +
      xlab("Time (ET)") +
      scale_color_manual(
        values = c("#002366", "#2E3440")) + 
      plot_theme() + 
      theme(legend.box = 'vertical')
    
 girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
    
  }) 
  
  
  output$temp_plot <- renderGirafe({
    
    
    unit = unit_state()
    
    y_label = ifelse(unit == 'ft', "Temperature (\u00B0 F)", "Temperature (\u00B0 C)")
    tool_label = ifelse(unit == 'ft', "\u00B0 F", "\u00B0 C")
    
    temp = if(unit == "m"){
      (combo_data()$Temperature_degF-32) * 5/9}else{combo_data()$Temperature_degF}
    
    shiny::validate(need(temp, "Data are not available from this instrument"))
    
    
   p =  ggplot(combo_data(), aes(x = Time_ET, y = temp)) +
      geom_line(linewidth = 1, color = "#256EFF") +
      geom_point_interactive(data = combo_data(), 
             aes(
                  x = Time_ET, 
                  y = temp, 
                  tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nAir Temperature: ",
                            sprintf("%.2f", temp),
                            tool_label
                            ),
                  data_id = data_id),
    size = 0.5,
    alpha = 0.01,
    hover_nearest = TRUE) +
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      xlab("Time (ET)") + 
      ylab(y_label) +
      plot_theme()
girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
    
  })
  
  output$air_plot <- renderGirafe({
    
    unit = unit_state() 
        
    shiny::validate(need(combo_data()$Pressure_inHg, "Data are not available from this instrument"))
    

 p = ggplot(combo_data(), aes(x = Time_ET, y = Pressure_inHg)) + 
      geom_line(linewidth= 1, color = "#14635B") + 
      geom_point_interactive(data = combo_data(),
                  aes( x = Time_ET, 
                       y = Pressure_inHg, 
                        tooltip = paste0(
                                "Time (ET): ",
                                format(
                                    Time_ET,
                                    format = "%b %d, %Y %I:%M %p",
                                    tz = "America/New_York"
                                ),
                                "\nAir Pressure: ",
                                sprintf("%.2f", Pressure_inHg),
                                " inHg"
                                ),
                      data_id = data_id),
                    size = 0.5,
                    alpha = 0.01,
                    hover_nearest = TRUE) +
      ylab("Air Pressure (inHg)") + 
      xlab("Time (ET)") + 
      ggtitle(ggtitle) + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      plot_theme() 

girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )


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
    
     legend_label <- if (unit == 'ft') {
             c("None", "< 0.5 ft", "0.5 - 1 ft", "1 - 2 ft", "> 2 ft", "Data Temporarily Unavailable")
          } else {
               c("None", "< 0.15 m", "0.15 - 0.3 m", "0.3 m - 0.6 m", "> 0.6 m", "Data Temporarily Unavailable")
                  }
    
    url <- paste0('CartoDB.Positron'="https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png",'?key=',leaflet_key)
  
    leaflet() %>% 
      addTiles(
        urlTemplate = url,
        group='baseMap') |> 
      setView(lng = -70.88, lat = 42.23, zoom = 7.5) |>
      addLegend(position = "bottomright", 
                opacity = 1, 
                colors = c("#2CBF04", "#FAEE07", "#F59115", "#E82D07", "#8F00FF", "lightgray"), 
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
       addMarkers(
          data = instrument.map, 
          lat = ~Latitude, 
          lng = ~Longitude,
          icon = ~instrument.icons[Type], 
          popup = ~paste0("<strong>", Name, "</strong><br/><a href='#' onclick=\"Shiny.setInputValue('go_to_instrument','", ID, "',{priority:'event'});\">View instrument details </a>")
          ) |>
      addCircleMarkers(data = filtered_flood_data(), 
                       lat = ~Latitude, 
                       lng = ~Longitude, 
                       color = getColor(filtered_flood_data()), 
                       radius = marker_radius(),  
                       fillOpacity = 1,
                       layerId = filtered_flood_data()$Location,
                       label=~as.character(filtered_flood_data()$Flood.Depth),
                       labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                   style = list(
                                                     "color" = "black",
                                                     "font-family" = "Replica LL TT",
                                                     "font-style" = "bold",
                                                     "font-size" = label_size())), 
                       popup = ~paste0("<strong>", Station.Name,
                                       "</strong><br/>
                                      Flood Depth: ", Flood.Depth, " ", unit, 
                                       "<br/>", Last_Available,
                                       "<br/> <a href='#'
                                          onclick=\"
                                          Shiny.setInputValue('go_to_tab',\'", 
                                       Location, "\',{priority:'event'});
                                          \">View station details </a>"))
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
  
############################################################### 
################# Compare Page Plots ##########################
############################################################### 
  output$wave_compare <- renderGirafe({
    
    unit = unit_state() 

    
    wave_height = if(input$wave_select_compare == 'significant'){
       combo_data()|> 
        dplyr::select(Time_ET, contains(paste0("Hs_Wave_Height_", unit))) |> 
        pivot_longer(cols = !Time_ET, 
                     names_to = "Location", 
                     values_to = "Wave_Height") |> 
        mutate(Location = str_replace(str_remove(Location, "_Hs_Wave_.*$"), "_", " ")) |> 
        filter(Location != "Rainsford")
    }else if(input$wave_select_compare == 'maximum'){
      combo_data()|> 
        dplyr::select(Time_ET, contains(paste0("Hmax_Wave_Height_", unit))) |> 
        pivot_longer(cols = !Time_ET, 
                     names_to = "Location", 
                     values_to = "Wave_Height") |> 
        mutate(Location = str_replace(str_remove(Location, "_Hmax_Wave_.*$"), "_", " "))
      
    }
    
    
    color = c("#256EFF", "gray66", "#14635B", "#750A7D")
    color = color[1:length(unique(wave_height$Location))]
    
    shiny::validate(need(wave_height$Wave_Height, "Data are not currently available for the selected parameter."))

   
    y_label = ifelse(unit == 'ft', "Wave Height (ft)", "Wave Height (m)")
    tool_label = ifelse(unit == 'ft', "ft", "m")
    
    p = ggplot(wave_height, aes(x = Time_ET, y = Wave_Height, color = Location)) + 
      geom_line(linewidth = 1) + 
      geom_point_interactive(aes(
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nLocation: ",
                            Location,
                            "\nWave Height: ",
                            sprintf("%.2f", Wave_Height),
                            tool_label
                            ),
                  data_id = paste0('wind_', seq(1, length(Wave_Height)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      ylab(y_label) + 
      xlab("") + 
      scale_color_manual(values = color) + 
      plot_theme() + 
      theme(legend.box = 'vertical')


 girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
  
    
   
  })
  
  output$tide_compare <- renderGirafe({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Tide Height (ft, MLLW)", "Tide Height (m, MLLW)")
    tool_label = ifelse(unit == 'ft', "ft, MLLW", "m, MLLW")
    
    color = c("#256EFF", "gray66", "#14635B", "#750A7D")
    color = color[1:length(input$tide_select_compare)]
 
    water_level = combo_data() |> 
                      dplyr::select(Time_ET, ends_with("Water_Level_Ft"), ends_with("Water_MLLW")) |> 
                      pivot_longer(cols = !Time_ET, 
                                   names_to = "Location", 
                                   values_to = "Water_Level") |> 
                      mutate(Location = str_replace(str_remove(Location, "_Water_.*$"), "_", " ")) |> 
                      filter(Location %in% input$tide_select_compare)

    
    shiny::validate(need(water_level$Water_Level, "Please pick tide gauges to compare"))

    water_level$Water_Level = if(unit == "m"){
      water_level$Water_Level /3.281}else{water_level$Water_Level }
   
    rows = ifelse(length(unique(water_level$Location)) > 3, 2, 1)
    
    
   p =  ggplot(water_level, aes(x = Time_ET, y = Water_Level, color = Location )) + 
      geom_line(linewidth = 1) +
     geom_point_interactive(aes(
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nLocation: ",
                            Location,
                            "\nWater Level: ",
                            sprintf("%.2f", Water_Level),
                            tool_label
                            ),
                  data_id = paste0('water_', seq(1, length(Water_Level)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      ylab(y_label) +
      xlab("") + 
      plot_theme() + 
      scale_color_manual(values = color) + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical') + 
      guides(color = guide_legend(nrow = rows))
    
girafe(
        ggobj = p,
        width_svg = graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
      )
    
  }) 
  
  
  
  
  output$flood_graph_compare <- renderGirafe({
    
    
    flood.data =  hohonu_data() %>% filter(Location %in% input$flood.compare) 
   
    
    
    shiny::validate(need(flood.data$Flood.Depth, 
                         "Please select sensors to compare"))
    
    
    unit = unit_state()
    
    Depth = if(unit == "m"){round(flood.data$Flood.Depth/3.281, 2)}else{flood.data$Flood.Depth}

    rows = ifelse(length(unique(flood.data$Location)) > 5, 3, 2)
    
    y_max = max(Depth,  convert_units(1, unit_state()), na.rm = T)
    
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    tool_label = ifelse(unit == 'ft', "ft", "m")
    
    p = ggplot(flood.data, aes(x = Time_ET, y = Depth, color = Station.Name)) + 
      geom_point(size = 0.75) + 
      geom_point_interactive(aes(
                   tooltip = paste0(
                            "Time (ET): ",
                            format(
                                Time_ET,
                                format = "%b %d, %Y %I:%M %p",
                                tz = "America/New_York"
                            ),
                            "\nLocation: ",
                            Station.Name,
                            "\nFlood Depth: ",
                            sprintf("%.2f", Depth),
                            tool_label
                            ),
                  data_id = paste0('water_', seq(1, length(Depth)))),
                size = 2,
                alpha = 0,
                hover_nearest = TRUE) + 
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("") + 
      plot_theme() +  
      guides(color = guide_legend(nrow = rows))
    
    
   girafe(
        ggobj = p,
        width_svg = flood_graph_width(),
        height_svg = graph_height(),
        options = list(
          opts_tooltip(
            css = paste0(
              "font-family: Replica LL TT;",
              "font-size: 14px;",
              "background-color: white;",
              "color: #333333;",
              "padding: 8px;"
            )
          ),
          opts_hover(
            css = "stroke-width: 2.5px;",
            nearest_distance = 10
          ),
    
          opts_sizing(
            rescale = TRUE, 
            width = 1
          ), 
          opts_toolbar(
            saveaspng = FALSE 
          )
        )
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
                unique(sensor_loc()$Sponsor), ". <br><br>
                Flood data is collected in real-time. The data have not been reviewed or edited and may be inaccurate. 
                Additionally, data may be temporarily unavailable for many reasons including 
                missed data transmissions, temporary loss of cellular connection, 
                or sensor errors (e.g., objects underneath sensors, unusually high data points). Sensors typically transmit data every 4 to 12 minutes. 
                Any missed connections are backfilled once connection is restored, but sensor errors will remain as missing data."))
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
  
  
    observe({
    newData = map_hohonu_data() 
    updateSliderInput(session, 
                      "time", 
                      min = round_date(min(newData$Time_ET, na.rm = T), "10 mins"), 
                      max = max(newData$Time_ET, na.rm = T), 
                      value = max(newData$Time_ET),                               
                      step    = 10 * 60,   
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
