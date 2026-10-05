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
library(plotly)


#Set Data File Path (changes for dockerfile)
#data_dir = "/srv/shiny-server/Data/"
data_dir = "~/Documents/01_Data_Products/Shiny_Apps/Data/"

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
                                    ")), 
                  
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
                                     title = tagList(
                                       "Flood and Instrument Map", 
                                       info_button("flood-info")), 
                                     class = "map-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     leafletOutput("flood_map", height = "100%")), 
                                   
                                   box(
                                     title = div(
                                       class = 'box-title-with-info', 
                                       span("Wind Speed at Rainsford Island"),
                                      info_button('wind_info')
                                     ),
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("wind_plot", height = "100%"))), 
                                   
                                   box(
                                     title = div(
                                       class = 'box-title-with-info', 
                                       span("Air Temperature at Rainsford Island"),
                                       info_button("temp_info")),
                                     class = 'plot-box',
                                     solidHeader = TRUE,
                                     width = 12,
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("temp_plot", height = "100%")))
                            ), #end column
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6",  
                                   
                                   box(title = div(
                                     class = 'box-title-with-info', 
                                     span(
                                     selectInput(
                                     "tide_select",
                                     label = NULL, 
                                     choices = list("Select Tide Gauge" = 'intro',
                                                    "Gallops Island" = "gallops",
                                                    "Essex - Main St." = 'essex',
                                                    "NOAA - Boston" = 'boston', 
                                                    "NOAA - Fall River" = 'fall.river'),
                                     multiple = F)),
                                     info_button("tide_info")), 
                                     solidHeader = TRUE, 
                                     width = 12, 
                                     class = 'plot-box',
                                     status = 'primary',
                                     shinyfullscreen::fullscreen_this(plotOutput("tide_plot", height= '100%'))), 
                                   
                                   
                                   
                                   box(
                                     title = div(
                                       class = 'box-title-with-info', 
                                       span(
                                        selectInput(
                                           "wave_select",
                                           label = NULL, 
                                           choices = list("Select Wave Buoy" = "intro",
                                                          "Harbor Entrance" = "harbor.entrance", 
                                                          "North Shore" = 'north.shore'),
                                           multiple = F)),
                                       info_button("wave_info")), 
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     shinyfullscreen::fullscreen_this(plotOutput("wave_plot", height = "100%"))), 
                                   
                                   box(
                                     title = div(
                                       class = 'box-title-with-info', 
                                       span("Air Pressure at Rainsford Island"),
                                       info_button("pressure_info")), 
                                     solidHeader = TRUE,
                                     class = 'plot-box',
                                     status = 'primary',
                                     width = 12,
                                     shinyfullscreen::fullscreen_this(plotOutput("air_plot", height = "100%")))) 
                          ) #end fluid row
                  ), #end TabItem
                  
                  
                  tabItem(tabName = "compare", 
                          fluidRow(
                            sliderInput(
                              inputId = "compare_time",
                              label   = "Select time:",
                              min     = start_time,
                              max     = end_time,
                              value   = end_time,
                              step    = 6 * 60,   # 10 minutes (in seconds)
                              timeFormat = "%b %d %H:%M",
                              animate = animationOptions(interval = 300), 
                              width = "85%")),
                          
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
                                     
                                     class = "plot-box",
                                     solidHeader = TRUE, 
                                     status = 'primary',
                                     width = 12, 
                                     plotlyOutput("flood_graph_compare", height = "100%"))), 
                                   
                          
                          column(width = 6, 
                                 class = "col-12 col-md-6",  
                                 
                                 box(title = pickerInput(
                                   "tide_select_compare",
                                   label = NULL, 
                                   choices = list("Gallops Island" = "Gallops", 
                                                  "Essex - Main St." = "Essex", 
                                                  "NOAA - Boston" = 'Boston', 
                                                  "NOAA - Fall River" = 'Fall River'),
                                   selected = list("Gallops Island" = "Gallops", 
                                                   "Essex - Main St." = "Essex", 
                                                   "NOAA - Boston" = 'Boston', 
                                                   "NOAA - Fall River" = 'Fall River'), 
                                   options = list(
                                     `actions-box` = TRUE, # Adds Select All/None buttons
                                     `selected-text-format` = "count > 2" # Shows count if many selected
                                   ),
                                   multiple = TRUE), 
                                   solidHeader = TRUE, 
                                   width = 12, 
                                   class = 'plot-box',
                                   status = 'primary',
                                   plotlyOutput("tide_compare", height= '100%'))), 
                                 
                                 
                                 
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
                                   plotlyOutput("wave_compare", height = "100%"))), 
                                
                                
                          
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
                                   "Essex - Main St. Tide Gauge" = "Essex.Tide",
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
                    href = "http://147.93.47.40:8080/app/SLL_Flood_Dashboard",
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
  
  observeEvent(input$wind_info, {
    showModal(
      modalDialog(
        title = "About Wind Speed",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  
  observeEvent(input$tide_info, {
    showModal(
      modalDialog(
        title = "About Tide",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$pressure_info, {
    showModal(
      modalDialog(
        title = "What does air pressure tell us?",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$flood_info, {
    showModal(
      modalDialog(
        title = "What does air pressure tell us?",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$temp_info, {
    showModal(
      modalDialog(
        title = "What does air pressure tell us?",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$wave_info, {
    showModal(
      modalDialog(
        title = "What does air pressure tell us?",
        
        p(
          "Wind speed can impact storm surge and wave height"
        ),
        
        tags$ul(
          tags$li("Water depth is measured relative to the station datum."),
          tags$li("Positive values indicate water above the reference level."),
          tags$li("Data are updated automatically as new observations become available.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
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
          legend.position = "bottom", 
          plot.title = element_text(size = 10), 
          plot.margin = margin(0.1,0.5,0.1,0.1, "cm") #t,r,b,l
        )
    } else {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text  = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.text  = element_text(size = 16),
          legend.title = element_blank(), 
          legend.position = "bottom", 
          plot.title = element_text(size = 18),
          plot.margin = margin(0.5,0.5,0.5,1, "cm")
        )
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
  

  
  ################ Data ####################
   ########## Reactive Statement to refresh the app ############
  
  combo_data <- reactiveFileReader(
    intervalMillis = 300000,
    session = session,
    filePath = file.path(data_dir, "Outputs/combo.csv"),
    readFunc = function(path){
      df <- data.table::fread(path, data.table = FALSE)
      df$Time_ET <- lubridate::ymd_hms(df$Time_ET, tz = "America/New_York")
      df
    }
  )
  
  hohonu_data <- reactiveFileReader(
    intervalMillis = 300000,
    session = session,
    filePath = file.path(data_dir, "Outputs/hohonu.csv"),
    readFunc = function(path){
      df <- data.table::fread(path, data.table = FALSE)
      df$Time_ET <- lubridate::ymd_hms(df$Time_ET, tz = "America/New_York")
      df
    }
  )
  
  map_hohonu_data <- reactiveFileReader(
    intervalMillis = 300000,
    session = session,
    filePath = file.path(data_dir, "Outputs/map_hohonu.csv"),
    readFunc = function(path){
      df <- data.table::fread(path, data.table = FALSE)
      df$Time_ET <- lubridate::ymd_hms(df$Time_ET, tz = "America/New_York")
      df
    }
  )
  
  tide_pred <- reactiveFileReader(
    intervalMillis = 300000,
    session = session,
    filePath = file.path(data_dir, "Outputs/tide_predictions.csv"),
    readFunc = function(path){
      df <- data.table::fread(path, data.table = FALSE)
      df$Time_ET <- lubridate::ymd_hms(df$Time_ET, tz = "America/New_York")
      df
    }
  )

  filtered_flood_data <- reactive({
    
    map_hohonu_data() %>% filter(Time_ET == with_tz(input$time, tzone = "America/New_York"))
    
  })
  
  sensor_loc <- reactive({
    hohonu_data() %>% filter(Location == input$station.id)
  })
  
  
  instrument_loc <- reactive({
    instrument.locations %>% filter(ID == input$instrument.id)
  })


  # Debounce time input by 250ms to prevent rapid re-render spikes
    selected_time <- reactive({ input$time }) %>% debounce(250)
    selected_compare_time <- reactive({ input$compare_time }) %>% debounce(250)
  
  ############# Wind Direction ################
  
  wind_dir <- reactive({
    
    req(combo_data())

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
    
    shiny::validate(need(wind_speed, "Data are not available from this instrument"))
    
    
    ggplot(combo_data(), aes(x = Time_ET, y = wind_speed)) +
      geom_line(aes(x = Time_ET, y = wind_speed, color = "Wind Speed"), linewidth = 1) +
      geom_line(aes(x = Time_ET, y = gust_speed, color = "Gust Speed"), linewidth = 1) +
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
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
        values = c("#256EFF", "#002366", "#14635B")) +
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
    
    shiny::validate(need(wave_height, "Data are not available from this instrument"))
    
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
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      scale_color_manual(
        values = c("#256EFF","#14635B")) + 
      plot_theme()
    
  })
  
  output$tide_plot <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)")
    
    ggtitle = case_when(
      input$tide_select == "intro" ~ "Gallops Tide Gauge and NOAA Flood Predictions",
      input$tide_select == "gallops" ~ "Gallops Tide Gauge and NOAA Flood Predictions", 
      input$tide_select == "boston" ~ "NOAA Tide Gauge and Flood Predictions - Boston", 
      input$tide_select == 'fall.river' ~ "NOAA Tide Gauge and Flood Predictions - Fall River", 
      input$tide_select == 'essex' ~ "Essex - Main St. Tide Gauge",
      .default = NA
    )
    
    water_level = if(input$tide_select == "gallops"){
      combo_data()$Gallops_Water_Level_ft}
    else if(input$tide_select == "boston"){
      combo_data()$Boston_Water_MLLW
    }else if(input$tide_select == 'fall.river'){
      combo_data()$Fall_River_Water_MLLW
    }else if(input$tide_select == 'essex'){
      combo_data()$Essex_Water_Level_ft
    }else if(input$tide_select == 'intro'){
      combo_data()$Gallops_Water_Level_ft
    }
    
    water_level = if(unit == "m"){
      water_level/3.281}else{water_level}
    
    shiny::validate(need(water_level, "Data are not available from this instrument"))
    
    prediction = if(input$tide_select == "gallops"){
      NA}
    else if(input$tide_select == "boston"){
      tide_pred()$Boston_Water_Prediction
    }else if(input$tide_select == 'fall.river'){
      tide_pred()$Fall_River_Water_Prediction
    }else if(input$tide_select == 'intro'){
      NA
    } else if(input$tide_select == 'essex'){
      NA
    }
    
    prediction = if(unit == "m"){
      prediction/3.281}else{prediction}
    
    
    major = if(input$tide_select == "gallops"){
      16}
    else if(input$tide_select == "boston"){
      16
    }else if(input$tide_select == 'fall.river'){
      11.98
    }else if(input$tide_select == 'intro'){
      16
    } else if(input$tide_select == 'essex') {
      NA
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
    } else if(input$tide_select == 'essex'){
      NA
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
    } else if(input$tide_select == 'essex'){
      NA
    }
    
    minor = if(unit == "m"){
      minor/3.281}else{minor}
    
    ymax = max(water_level, (major * 1.1))
    
    ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
      geom_hline(yintercept = minor, color = "#F6C871", linewidth = 1.5, linetype = 'dotted') + 
      geom_hline(yintercept = moderate, color = "#EE7E6D", linewidth = 1.5, linetype = 'dotted') + 
      geom_hline(yintercept = major, color = "#8F62FF", linewidth = 1.5, linetype = 'dotted') + 
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
      geom_line(aes(color = "Actual Water Level"), linewidth = 1) +
      geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level"), linetype = 'dotted', linewidth =1) + 
      scale_fill_manual(values = c("#8F62FF", "#F6C871", "#EE7E6D")) + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      ylab(y_label) +
      xlab("Time (ET)") +
      ggtitle(ggtitle) +
      scale_color_manual(
        values = c("#002366", "#2E3440")) + 
      plot_theme() + 
      theme(legend.box = 'vertical')
    
    
  }) 
  
  
  output$temp_plot <- renderPlot({
    
    
    unit = unit_state()
    
    y_label = ifelse(unit == 'ft', "Temperature (\u00B0 F)", "Temperature (\u00B0 C)")
    
    temp = if(unit == "m"){
      (combo_data()$Temperature_degF-32) * 5/9}else{combo_data()$Temperature_degF}
    
    shiny::validate(need(temp, "Data are not available from this instrument"))
    
    
    ggplot(combo_data(), aes(x = Time_ET, y = temp)) +
      geom_line(linewidth = 1, color = "#256EFF") +
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      xlab("Time (ET)") + 
      ylab(y_label) +
      plot_theme()
    
    
  })
  
  output$air_plot <- renderPlot({
    
    unit = unit_state() 
    
    air = combo_data()$Pressure_inHg
    
  
    shiny::validate(need(air, "Data are not available from this instrument"))
    

    

    ggplot(combo_data(), aes(x = Time_ET, y = air)) + 
      geom_line( linewidth= 1, color = "#14635B") + 
      ylab("Air Pressure (inHg)") + 
      xlab("Time (ET)") + 
      ggtitle(ggtitle) + 
      geom_vline(xintercept = with_tz(selected_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") +
      plot_theme() + 
      theme(plot.title = element_text(size = 18))
    
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
  
  output$wind_compare <- renderPlot({
    
    
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
      geom_vline(xintercept = with_tz(selected_compare_time(), tzone = "America/New_York"), 
                 color = "darkred", linewidth = 0.75, linetype = "dashed") +
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
        values = c("#256EFF", "#002366", "#14635B")) +
      plot_theme()
    
    
    
  })
  
  
  output$wave_compare <- renderPlotly({
    
    unit = unit_state() 

    
    wave_height = if(input$wave_select_compare == 'significant'){
       combo_data()|> 
        dplyr::select(Time_ET, contains(paste0("Hs_Wave_Height_", unit))) |> 
        pivot_longer(cols = !Time_ET, 
                     names_to = "Location", 
                     values_to = "Wave_Height") |> 
        mutate(Location = str_replace(str_remove(Location, "_Hs_Wave_.*$"), "_", " ")) 
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
    
    p = ggplot(wave_height, aes(x = Time_ET, y = Wave_Height, color = Location)) + 
      geom_line(linewidth = 0.75) + 
      geom_vline(xintercept = input$compare_time, 
                 color = "darkred", linewidth = 0.5, linetype = "dashed") +
      ylab(y_label) + 
      xlab("") + 
      scale_color_manual(values = color) + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical')
    
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
          font = list(family = "Replica LL TT", 
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
  
  output$tide_compare <- renderPlotly({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Tide Height (ft, MLLW)", "Tide Height (m, MLLW)")
    
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
    
    
   p =  ggplot(water_level, aes(x = Time_ET, y = Water_Level, color = Location)) + 
      geom_line(linewidth = 0.75) +
      geom_vline(xintercept = input$compare_time , 
                 color = "darkred", linewidth = 0.5, linetype = "dashed") +
      ylab(y_label) +
      xlab("") + 
      plot_theme() + 
      scale_color_manual(values = color) + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical') + 
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
         font = list(family = "Replica LL TT", 
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
  
  
  
  
  output$flood_graph_compare <- renderPlotly({
    
    
    flood.data =  hohonu_data() %>% filter(Location %in% input$flood.compare) 
   
    
    
    shiny::validate(need(flood.data$Flood.Depth, 
                         "Please select sensors to compare"))
    
    
    unit = unit_state()
    
    Depth = if(unit == "m"){round(flood.data$Flood.Depth/3.281, 2)}else{flood.data$Flood.Depth}

    rows = ifelse(length(unique(flood.data$Location)) > 5, 3, 2)
    
    y_max = max(Depth,  convert_units(1, unit_state()), na.rm = T)
    
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    
    p = ggplot(flood.data, aes(x = Time_ET, y = Depth, color = Station.Name)) + 
      geom_point(size = 0.75) + 
      geom_vline(xintercept = input$compare_time, 
                 color = "darkred", linewidth = 0.5, linetype = "dashed") +
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("") + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18))+ 
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
          font = list(family = "Replica LL TT", 
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
  
  output$temp_compare <- renderPlotly({
    
    
    unit = unit_state()
    
    y_label = ifelse(unit == 'ft', "Temperature (\u00B0 F)", "Temperature (\u00B0 C)")
    
    temp_data = combo_data() |> 
                  dplyr::select(Time_ET, Temperature_degF) |> 
                  mutate(Temperature_degF = if(unit == 'm'){(Temperature_degF - 32)*5/9}else{Temperature_degF})
    
   
    shiny::validate(need(temp_data$Temperature_degF, "Temperature data are not currently available."))

    p = ggplot(temp_data, aes(x = Time_ET, y = Temperature_degF)) +
      geom_line(linewidth = 0.75, color = "#256EFF") +
      geom_vline(xintercept =input$compare_time , 
                 color = "darkred", linewidth = 0.5, linetype = "dashed") +
      xlab(" ") + 
      ylab(y_label) +
      plot_theme() + 
      theme(plot.title = element_text(size = 18))
    
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
          font = list(family = "Replica LL TT", 
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
  
  output$air_compare <- renderPlotly({
    
    unit = unit_state() 
    
    
   air = if(input$air_select_compare == 'RH'){
      combo_data()|> 
        dplyr::select(Time_ET, `RH_.`) |> 
        rename(Air_value =`RH_.` )
    }else if(input$air_select_compare == 'Pressure'){
      combo_data()|> 
        dplyr::select(Time_ET, Pressure_inHg) |> 
        rename(Air_value = Pressure_inHg)
    }
    
    shiny::validate(need(air$Air_value, "Data are not currently available for the selected parameter."))
    
    
    y_label = ifelse(input$air_select_compare == 'RH', "Relative Humidity (%)", "Air Pressure (inHg)")
    
    p = ggplot(air, aes(x = Time_ET, y = Air_value)) + 
      geom_line(linewidth = 0.75, color = "#256EFF") + 
      geom_vline(xintercept = input$compare_time, 
                 color = "darkred", linewidth = 0.5, linetype = "dashed") +
      ylab(y_label) + 
      xlab("") + 
      plot_theme() + 
      theme(plot.title = element_text(size = 18), 
            legend.box = 'vertical')
    
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
          font = list(family = "Replica LL TT", 
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
  
  
  
  output$station_flood <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    depth = if(unit == "m"){sensor_loc()$Flood.Depth/3.281}else{sensor_loc()$Flood.Depth}
    y_max = max(depth, convert_units(1, unit_state()), na.rm = T)
    
    shiny::validate(need(depth, "Data are not available from this instrument"))
    
    ggplot(sensor_loc(), aes(x = Time_ET, y = depth)) + 
      geom_point(color = "#14635B") + 
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
          values = c("#256EFF","#14635B")) + 
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
          values = c("#256EFF", "#002366", "#14635B")) +
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
      
      prediction = if(unit == "m"){
        tide_pred()$Boston_Water_Prediction/3.281}else{tide_pred()$Boston_Water_Prediction}
      
      shiny::validate(need(water_level, "Data are not available from this instrument"))
      
      ggplot(combo_data(), aes(x = Time_ET, y = water_level)) + 
        ylab(y_label) +
        xlab("Time (ET)") + 
        ggtitle("NOAA Tide Gauge and Flood Predictions - Boston") + 
        scale_color_manual(
          values = c("#002366", "#2E3440")) + 
        geom_hline(yintercept = minor, color = "#F6C871", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = moderate, color = "#EE7E6D", linewidth = 1.5, linetype = 'dotted') + 
        geom_hline(yintercept = major, color = "#8F62FF", linewidth = 1.5, linetype = 'dotted') + 
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
        geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level"), linetype = 'dotted', linewidth =1) + 
        scale_fill_manual(values = c("#8F62FF", "#F6C871", "#EE7E6D")) + 
        plot_theme() + 
        theme(legend.box = 'vertical')
      
      
    }
    else if(input$instrument.id == "Fall.River.Tide"){
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = if(unit == "m"){combo_data()$Fall_River_Water_MLLW/3.281}else{combo_data()$Fall_River_Water_MLLW}
      
      shiny::validate(need(water_level, "Data are not available from this instrument"))
      
      prediction = if(unit == "m"){
        tide_pred()$Fall_River_Water_Prediction/3.281}else{tide_pred()$Fall_River_Water_Prediction}
      
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
        geom_hline(yintercept = major, color = "#8F62FF", linewidth = 1.5, linetype = 'dotted') + 
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
        geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level"), linetype = 'dotted', linewidth =1) + 
        scale_fill_manual(values = c("#8F62FF", "#F6C871", "#EE7E6D")) + 
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
        theme_bw(base_family = "Replica Mono LL TT") + 
        scale_color_manual(
          values = c("#256EFF","#14635B")) + 
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
        scale_color_manual(
          values = c("#256EFF")) + 
        plot_theme()
    }
    else if(input$instrument.id == "Essex.Tide"){
      
      unit = unit_state()
      y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
      
      water_level = combo_data()$Essex_Water_Level_ft
      
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
    
  })
  

  observe({
    new_time = map_hohonu_data()
    time$Time_ET <- lubridate::ymd_hms(new_time$Time_ET, tz = "America/New_York")

    new_start_time = round_date(min(new_time$Time_ET, na.rm = T), "10 mins")
    new_end_time = max(new_time$Time_ET, na.rm = T)
  
    updateSliderInput(session, 
                      "time", 
                      min = new_start_time, 
                      max = new_end_time, 
                      value = new_end_time, 
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
