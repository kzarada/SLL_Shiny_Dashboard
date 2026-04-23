########################################
#
#   Flood Dashboard
#
########################################

library(tidyverse)
library(shinydashboard)
library(leaflet)
library(fresh)
library(shinybrowser)
#Set Data File Path (changes for dockerfile)
#data_dir = "/Users/katherinezarada/Documents/Projects/Climate_Change_Observatory/01_Analysis/Monitoring_Data_Download/00_Data"
data_dir = "/srv/shiny-server/Data/"


###### Read in Data #######
hohonu = read.csv(file.path(data_dir, "Outputs/hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

map_hohonu = read.csv(file.path(data_dir, "Outputs/map_hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

start_time = round_date(min(map_hohonu$Time_ET, na.rm = T), "10 mins")
end_time = max(map_hohonu$Time_ET, na.rm = T)

#colors: 
#blue: #256EFF
#teal: #2EBBAD
#darkblue: #002366
#white


####### Create theme #############

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#002366"
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
  title = "SLL Flood Dashboard",
  
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
                menuItem("Feedback", tabName = 'feedback', icon = icon("comment-dots")),
                menuItem("Contact Us", tabName = 'contact', icon = icon("square-envelope"))), 
    collapsed = TRUE),
  
  dashboardBody(use_theme(mytheme),
                
                
                tags$head(
                  
                  tags$link(rel = "stylesheet", type = "text/css", href = "flood_dash_styles.css")),
                
                
                tags$script(HTML('$(document).ready(function() {
                                 $("header").find("nav").append(\'<span class="myClass"> SLL Flooding Dashboard </span>\');})')),
                
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
                            
                            box(
                              class = "map-box", 
                              title = "Flood Map", 
                              solidHeader = TRUE, 
                              status = 'primary',
                              width = 6,
                              #height = "auto",
                              leafletOutput("flood_map", height = "100%")), 
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6",
                                   column(width = 6, 
                                          class = "col-12 col-md-6",
                                          selectInput(
                                            "station_select",
                                            label = "Select Station:", 
                                            choices = list( "Boston - Border Street" = "Border.St", 
                                                            "Boston - Lewis Mall" = "Lewis.Mall",
                                                            "Boston - Long Wharf" = "Long.Wharf",
                                                            "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                                            "Boston - Tenean Beach" = "Tenean.Beach",
                                                            "Essex - Main Street" = "Essex", 
                                                            "Fall River - Stafford Square" = "Fall.River", 
                                                            "Salem - Collin's Cove" = "Salem"),
                                            multiple = F)), 
                                   
                                   column(width = 6,
                                          class = "col-12 col-md-6", 
                                          selectInput(
                                            "icon_select", 
                                            label = "Compare to:", 
                                            choices = list("Bus" = "bus", 
                                                           "Human" = "walk", 
                                                           "Bike" = "bike", 
                                                           "Giant Shrimp" = 'shrimp'), 
                                            multiple = F
                                          )),
                                   
                                   box(solidHeader = TRUE, 
                                       title = "Flood Context",
                                       width = 12, 
                                       height = "auto", 
                                       status = "primary", 
                                       column(7, 
                                              class = "col-xs-7",
                                              div(htmlOutput("flood_text"))), 
                                       
                                       column(5, 
                                              class = "col-xs-5", 
                                              uiOutput("icon"))
                                       
                                       
                                   ), #end box
                                   
                                   
                                   box(class = "plot-box", 
                                       solidHeader = TRUE, 
                                       title = "Last 24 hours of flood depth",
                                       width = 12, 
                                       height = "auto", 
                                       status = "primary", 
                                       shinyfullscreen::fullscreen_this(plotOutput("main_flood", height = "100%"))
                                       
                                   ) #end box
                                   
                            ) #end col
                          ) #end fluidrow 
                  ), #end tab item
                  
                  tabItem(tabName = "stations", 
                          fluidRow(
                            selectInput(
                              "station.id", 
                              "Select Station:", 
                              choices = list("Boston - Border Street" = "Border.St", 
                                             "Boston - Lewis Mall" = "Lewis.Mall",
                                             "Boston - Long Wharf" = "Long.Wharf",
                                             "Boston - Morrissey Blvd" = "Morrissey.Blvd",
                                             "Boston - Tenean Beach" = "Tenean.Beach",
                                             "Essex - Main Street" = "Essex", 
                                             "Fall River - Stafford Square" = "Fall.River", 
                                             "Salem - Collin's Cove" = "Salem"), 
                              multiple = F
                            ), 
                            
                            column(width = 6, 
                                   class = "col-12 col-md-6", 
                                   box(
                                     title = "Sensor Photo", 
                                     solidHeader = TRUE, 
                                     status = 'primary', 
                                     uiOutput("sensor_photo"), 
                                     width = 12
                                   )#end box
                            ), #end col
                            
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
                                class = "plot-box",
                                status = 'primary', 
                                leafletOutput("sensor_map", height = "100%"), 
                                width = 12
                              ), 
                              
                              box(
                                title = "Flood Depth", 
                                solidHeader = TRUE, 
                                class = "plot-box",
                                status = "primary", 
                                shinyfullscreen::fullscreen_this(plotOutput("station_flood", height = "100%")), 
                                width = 12
                              ) #end box
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
                                       src = "https://docs.google.com/forms/d/e/1FAIpQLSc2N62zj_VH6uqfW3w2mISb8jNECv5C1lRM_j1afsGX0wgZcQ/viewform?embedded=true", 
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
                                    Stone Foundation that engages scientists and the community in research, education, and the promotion of equity.")))), 
                          
                          column(width = 12, 
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
                                   style="display: block; margin-left: auto; margin-right: auto;")
                          
                  )
                ), #end Tab Items 
                tags$div(
                  class = "app-footer",
                  tags$a(
                    href = "http://147.93.47.40:8080/app/SLL_Current_Coastal_Conditions",
                    target = "_blank",
                    HTML("Interested in seeing more data? <u>Click here</u> to view our Current Coastal Conditions Dashboard")
                  )
                )   
                
  ) #end dashboard body
) #end ui 




# ---- Server ----
server <- function(input, output, session) {
  
  
  ################## Popup ################## 
  showModal(modalDialog(
    title = "Welcome to the Stone Living Lab Flooding Dashboard!",
    HTML(paste0("This dashboard displays real-time flooding conditions at our overland flood sensors. 
    For more information on how to navigate the dashboard, please see our <u>", tags$a("dashboard user guide.", 
                                                                                       href = "https://www.canva.com/design/DAGzC4w-HyY/FTvLHmZpkhQShXF0yTdktg/view?utm_content=DAGzC4w-HyY&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h023b1e34ec", 
                                                                                       target = '_blank'), "</u>")),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
  
  ################## Unit Switch ################## 
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
  
  ########## Mobile Detection #############
  
  plot_theme <- reactive({
    if (shinybrowser::is_device_mobile()) {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 7),
          axis.title   = element_text(size = 9),
          legend.position = "none"
        )
    } else {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text  = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.position = "none"
        )
    }
  })
  
  
  ################## Load Data ##################
  
  
  filtered_flood_data <- reactive({
    
    map_hohonu_data()  %>% filter(Time_ET == with_tz(input$time, tzone = "America/New_York")) %>% 
      mutate(Flood.Depth = convert_units(Flood.Depth, unit_state()))
  })
  
  sensor_loc <- reactive({
    hohonu_data() %>% filter(Location == input$station.id)
  })
  
  main_flood_graph <- reactive({
    hohonu_data() %>% filter(Location == input$station_select)
  })
  
  water_depth <- reactive({
    hohonu_data() %>% 
      filter(Location == input$station_select) %>%  
      filter(Time_ET == with_tz(input$time, tzone = "America/New_York")) %>% 
      pull(Flood.Depth)
  })
  
  ################## Water for Icon ##################
  
  water_percent <- reactive({
    req(water_depth(), input$icon_select)
    
    icon_height = switch(input$icon_select,
                         "bus" = 10, #the bus ruler is larger than the other icons
                         6)
    
    pct <- (water_depth() / icon_height) * 100
    pct <- max(0, min(100, pct))
    
    paste0(pct, "%")
  })
  
  
  
  ################## Main Plots ##################
  output$flood_text <- renderText({
    
    depth_ft <- water_depth()   # your reactive value
    
    depth <- convert_units(depth_ft, unit_state())
    
    bus_height = convert_units(9.8, unit_state())
    bike_height = convert_units(3, unit_state())
    person_height = convert_units(5.5, unit_state())
    shrimp_height = convert_units(4, unit_state())
    
    unit_label <- unit_state()
    
    if(length(depth)== 0){
      
      paste0("There is not data available for this station at the time selected. Please choose another location or time.")
    }
    else if(input$icon_select == 'bus'){
      paste0("This icon shows the flood depth of <strong> ", depth, " ", unit_label, 
             "  at ", str_replace(input$station_select, "[.]", " "), 
             " on ", format(with_tz(input$time, tzone = "America/New_York"), "%B %d at %H:%M"), 
             " </strong> compared to a ", bus_height, " ", unit_label, " bus. <br> <br> <small> <i>This comparison is intended to provide 
           general information about potential flooding conditions. Data are real-time and may
           be inaccurate. Please use your own judgment and consult any official warnings and local authorities regarding any flooding.</small></i><br> <br> ")
    }
    else if(input$icon_select == 'bike'){
      
      paste0("This icon shows the flood depth of <strong> ", depth, " ", unit_label, 
             "  at ", str_replace(input$station_select, "[.]", " "), 
             " on ", format(with_tz(input$time, tzone = "America/New_York"), "%B %d at %H:%M"), 
             " </strong> compared to a ", bike_height, " ", unit_label, " bike. <br> <br> <small> <i>This comparison is intended to provide 
           general information about potential flooding conditions. Data are real-time and may
           be inaccurate. Please use your own judgment and consult any official warnings and local authorities regarding any flooding.</small></i><br> <br> ")
      
    }
    else if(input$icon_select == 'walk'){
      
      paste0("This icon shows the flood depth of <strong> ", depth, " ", unit_label, 
             "  at ", str_replace(input$station_select, "[.]", " "), 
             " on ", format(with_tz(input$time, tzone = "America/New_York"), "%B %d at %H:%M"), 
             " </strong> compared to a ", person_height, " ", unit_label, " person. <br> <br> <small> <i>This comparison is intended to provide 
           general information about potential flooding conditions. Data are real-time and may
           be inaccurate. Please use your own judgment and consult any official warnings and local authorities regarding any flooding.</small></i><br> <br> ")
      
    }
    else if(input$icon_select == 'shrimp'){
      
      paste0("This icon shows the flood depth of <strong> ", depth, " ", unit_label, 
             "  at ", str_replace(input$station_select, "[.]", " "), 
             " on ", format(with_tz(input$time, tzone = "America/New_York"), "%B %d at %H:%M"), 
             " </strong> compared to a giant ", shrimp_height, " ", unit_label, " shrimp. <br> <br> <small> <i>This comparison is intended to provide 
           a whimsical break from flooding conditions. Please enjoy this gigantic pink shrimp. </small></i><br><br>")
      
    }
  })
  
  
  
  output$icon <- renderUI({
    
    unit <- unit_state() 
    
    if(input$icon_select == 'bus'){
      
      div(class = "icon_wrapper",
          # Ruler
          div(class = "ruler", 
              
              ticks <- lapply(0:10, function(i) {
                pos <- (i / 10) * 100
                
                div(class = "tick",
                    style = paste0("bottom:", pos, "%;"),
                    div(class = "tick-line"),
                    div(class = "tick-label", paste0(ifelse(unit == "ft", i, round(i/3.281, 1)), " ", unit)))
              }) #end lapply
          ), #end ruler dive
          # Container box
          div(class = "container-box", 
              div(class = "water", 
                  style = paste0("height:", water_percent())),
              div(class = "icon bus-icon", fontawesome::fa("bus"))
          ) #end container div
      ) #end icon_wrapper div
    } #end if statement
    else if(input$icon_select == 'bike'){
      div(class = "icon_wrapper",
          # Ruler
          div(class = "ruler", 
              
              ticks <- lapply(0:6, function(i) {
                pos <- (i / 6) * 100
                
                div(class = "tick",
                    style = paste0("bottom:", pos, "%;"),
                    div(class = "tick-line"),
                    div(class = "tick-label", paste0(ifelse(unit == "ft", i, round(i/3.281, 1)), " ", unit)))
              }) #end lapply
          ), #end ruler dive
          
          # Container box
          div(class = "container-box", 
              div(class = "water", 
                  style = paste0("height:", water_percent())),
              div(class = "icon bike-icon", fontawesome::fa("bicycle"))
          ) #end container div
      ) #end icon_wrapper div
    }
    
    else if(input$icon_select == 'walk'){
      div(class = "icon_wrapper",
          # Ruler
          div(class = "ruler", 
              
              ticks <- lapply(0:6, function(i) {
                pos <- (i / 6) * 100
                
                div(class = "tick",
                    style = paste0("bottom:", pos, "%;"),
                    div(class = "tick-line"),
                    div(class = "tick-label", paste0(ifelse(unit == "ft", i, round(i/3.281, 1)), " ", unit)))
              }) #end lapply
          ), #end ruler dive
          
          # Container box
          div(class = "container-box", 
              div(class = "water", 
                  style = paste0("height:", water_percent())),
              div(class = "icon walking-icon", bsicons::bs_icon("person-walking"))
          ) #end container div
      ) #end icon_wrapper div
      
    }
    
    else if(input$icon_select == 'shrimp'){
      div(class = "icon_wrapper",
          # Ruler
          div(class = "ruler", 
              
              ticks <- lapply(0:6, function(i) {
                pos <- (i / 6) * 100
                
                div(class = "tick",
                    style = paste0("bottom:", pos, "%;"),
                    div(class = "tick-line"),
                    div(class = "tick-label", paste0(ifelse(unit == "ft", i, round(i/3.281, 1)), " ", unit)))
              }) #end lapply
          ), #end ruler dive
          
          # Container box
          div(class = "container-box", 
              div(class = "water", 
                  style = paste0("height:", water_percent())),
              div(class = "icon shrimp-icon", fontawesome::fa("shrimp", fill = "#E75480"))
          ) #end container div
      ) #end icon_wrapper div
      
    }
    
  })
  
  output$main_flood <- renderPlot({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Flood Depth (ft)", "Flood Depth (m)")
    depth = if(unit == "m"){main_flood_graph()$Flood.Depth/3.281}else{main_flood_graph()$Flood.Depth}
    y_max = max(max(depth, na.rm = T), convert_units(1, unit_state()), na_rm = T)
    
    ggplot(main_flood_graph(), aes(x = Time_ET, y = depth)) + 
      geom_line(linewidth = 1.5, color = "#2EBBAD") + 
      geom_vline(xintercept = with_tz(input$time, tzone = "America/New_York"), 
                 color = "darkred", linewidth = 1, linetype = "dashed") + 
      ylab(y_label) + 
      ylim(c(0, y_max)) + 
      xlab("Time (ET)") + 
      plot_theme()
    
  })
  
  
  zoom_level <- reactive({
    input$flood_map_zoom
  })
  
  marker_radius <- reactive({
    z <- zoom_level()
    if (is.null(z)) return(16)
    
    # smoother scaling
    return(2 * (1.205 ^ z))
  })
  
  label_size <- reactive({
    z <- input$flood_map_zoom
    
    if (is.null(z)) return("12px")
    
    if(z < 11) return("0px")
    
    # Scale text with zoom (adjust multiplier to taste)
    size <- 6 + z * 0.8
    
    paste0(size, "px")
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
      setView(lng = -70.88, lat = 42.23, zoom = 8.5) %>% 
      addLegend(position = "bottomright", 
                opacity = 1, 
                colors = c("#2CBF04", "#FAEE07", "#F5A30C", "#E82D07", "#8F00FF", "lightgray"), 
                labels = legend_label, 
                title = "Flood Depth") 
    
  })
  
  observe({
    
    unit = unit_state()
    
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
                                          \">View station details </a>"))
  })
  
  #observe event to go to tab when user selects station on map  
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
  
  
  ################## Sensor Page ##################
  
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
  
  output$sensor_photo <- renderUI({
    
    req(input$station.id)
    
    tags$img(
      src = paste0(input$station.id, ".jpg"), 
      width = "100%")
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
      plot_theme()
    
  })
  
  ################## Reactive Statement to Update App ##################
  
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
  
  observe({
    newData = map_hohonu_data() 
    updateSliderInput(session, 
                      "time", 
                      min = round_date(min(newData$Time_ET), "10 mins"), 
                      max = max(newData$Time_ET), 
                      value = max(newData$Time_ET), 
                      timeFormat = "%b %d %H:%M")
  })
  
  
  
  
  
  
} #end server


shinyApp(ui, server)

#shinyApp(ui, server, options = list(launch.browser = TRUE))
