########################################
#
#  WICKED HIGH TIDES
#
########################################

library(tidyverse)
library(shinydashboard)
library(leaflet)
library(fresh)
library(shinybrowser)
library(plotly)


#Set Data File Path (changes for dockerfile)
data_dir = "/srv/shiny-server/Data/"

###### Read in Data #######
flood.depth = read.csv(file.path(data_dir, "Outputs/map_hohonu.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
  filter(Location == "Long.Wharf") 

combo = read.csv(file.path(data_dir, "Outputs/combo.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) |> 
  dplyr::select(Time_ET, Boston_Water_MLLW)

tide_pred = read.csv(file.path(data_dir, "Outputs/tide_predictions.csv")) %>% 
  mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
         Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) |> 
  dplyr::select(Time_ET, Boston_Water_Prediction)

peak = as.Date(c("2026-10-28", "2026-11-26", "2026-12-25"))
diff =  peak - Sys.Date()
count_down = as.numeric(min(diff[diff >= 0]))

peak_day = format(min(peak[diff >= 0]), "%b %d, %Y")

#colors: 
#blue: #256EFF
#teal: #2EBBAD
#darkblue: #002366
#white


####### Create theme #############

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2E0161"
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
  
  title = "Wicked High Tides",
  
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
  
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(use_theme(mytheme),
                
          
                tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "wht_styles.css")),
                tags$script(HTML('$(document).ready(function() {
                                 $("header").find("nav").append(\'<span class="myClass"> Wicked High Tides </span>\');})')),
                
                shinybrowser::detect(), 
                
    fluidRow(
            
                
                  box(solidHeader = TRUE, 
                      width = 2,
                      height = "380px",
                      title = "Days until Peak", 
                      status = 'primary', 
                      tags$div(class = 'main-text', count_down),
                      tags$div(class = 'sub-text', paste0("\n \n Peak tide on: \n" , peak_day))),
                      
                                   
                    box(solidHeader = TRUE, 
                        title = "Moon Phase",
                        height = "380px",
                        width = 2,
                        status = "primary", 
                        htmlOutput("frame", height = "100%", style = 'text-align:center;')), 
                  
                  box(solidHeader = TRUE, 
                      title = "Water Depth at Long Wharf", 
                      height = "380px", 
                      width = 3, 
                      status = 'primary', 
                      htmlOutput("water_level")),
                  
                  box(solidHeader = TRUE, 
                      title = "Other Links", 
                      height = '380px', 
                      width = 5, 
                      status = 'primary', 
                      tags$div(
                        class = "button-text", 
                          tags$a(
                            href = "https://mycoast.org/ma/king-tides",
                            target = "_blank",
                            HTML("<p><i class = 'fa fa-camera' ></i>   Have photos of flooding? <u>Click here</u> to submit a photo to MyCoast!</p> <br>  ")
                          ),
                      tags$a(
                        href = "http://147.93.47.40:8080/app/SLL_Flood_Dashboard",
                        target = "_blank",
                        HTML("<p><i class = 'fa fa-map-location-dot' ></i>  Want to see more real-time flooding conditions? <u>Click here</u> to check out the SLL Flood Dashboard!</p>")
                      ))),

                  
                  box(solidHeader = TRUE, 
                      class = 'plot-box',
                      title = "NOAA Tide Gauge - Boston", 
                      status = 'primary',
                      width = 12, 
                      plotlyOutput("Tide", height = "100%"))), 
               
                                   

                tags$div(
                  class = "app-footer",
                  tags$a(
                    href = "https://stonelivinglab.org/education/wicked-high-tides/",
                    target = "_blank",
                    HTML("<u>Click here</u> to learn more about Wicked High Tides!")
                  )
                )   #end footer 
                
  ) #end dashboard body
) #end UI 




# ---- Server ----
server <- function(input, output, session) {
  
  

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
          legend.text = element_text(size = 7),
          plot.margin = margin(0.1,0.5,0.1,0.1, "cm") #t,r,b,l
        )
    } else {
      theme_bw(base_family = "Replica Mono LL TT") +
        theme(
          axis.text  = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.margin = margin(0.5,1,0.5,0.5, "cm")
        )
    }
  })
  
  
  ################## Load Data ##################

  water_depth <- reactive({
    flood.depth() %>% 
      filter(Location == "Long.Wharf") |> 
      slice_max(Time_ET) |> 
      pull(Flood.Depth)
  })
  
  change <- reactive({
    flood.depth() |> 
      filter(Location == "Long.Wharf") |> 
      dplyr::select(Time_ET, Flood.Depth) |> 
      mutate(Diff = Flood.Depth - lag(Flood.Depth)) |> 
      tail(10) |> 
      pull(Diff) |> 
      mean() 
    
  })
  
  change_text <- reactive({
    case_when(
    change() == 0 ~ "stable",
    change() > 0 ~ "rising", 
    change() < 0 ~ "falling")
    })

  ################## Reactive Statement to Update App with Live Data ##################
  
  flood.depth <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/map_hohonu.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
        filter(Location == "Long.Wharf") 
    }
  )
  
  combo_data <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/combo.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))|> 
        dplyr::select(Time_ET, Boston_Water_MLLW)
    }
  )
  
  tide_pred <- reactiveFileReader(
    intervalMillis = 500,
    session = session,
    filePath = file.path(data_dir, "Outputs/tide_predictions.csv"),
    readFunc = function(path){
      read.csv(path) %>% 
        mutate(Time_ET = ifelse(str_detect(Time_ET, ":00$", negate = T), paste0(Time_ET, " 00:00:00"), Time_ET), 
               Time_ET = as.POSIXct(Time_ET, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))|> 
        dplyr::select(Time_ET, Boston_Water_Prediction)
    }
  )
  
  ################## Main Pages ##################
  
  #### Flood Depth Text #####
  
  output$water_level <- renderUI(
    
        
      tagList(
        div(class = 'main-text',
            paste0(ifelse(unit_state() == "ft", water_depth(), round(water_depth()/3.281,2)), " ", unit_state())), 
        div(class = 'sub-text',
            "Water levels are ", change_text()))
      )  
     

  
  ###### Moon Embed
  
  output$frame <- renderUI(
    tags$iframe(src="https://in-the-sky.org/widgets/moonphase.php?skin=0&locale=1&town=4930956", 
                width = "190px", 
                height = "320px")) #end frame

  
  ##### Tide Plot
  
  output$Tide <- renderPlotly({
    
    unit = unit_state()
    y_label = ifelse(unit == 'ft', "Height (ft, MLLW)", "Height (m, MLLW)") 
  

  
    water_level = if(unit == "m"){combo_data()$Boston_Water_MLLW/3.281}else{combo_data()$Boston_Water_MLLW}
    LW_elev = ifelse(unit == "m", 11.78/3.281, 11.78)
  
    prediction = if(unit == "m"){
      tide_pred()$Boston_Water_Prediction/3.281}else{tide_pred()$Boston_Water_Prediction}
  
    shiny::validate(need(water_level, "Data are not available from this instrument"))
  
  p =  ggplot(combo_data(), aes(x = Time_ET, y = water_level, group = 1)) + 
      ylab(y_label) +
      xlab("") +  
      geom_line(aes(color = "Water Level", 
                    text = paste0("Observed tide height of ", round(water_level, 2), " at ", Time_ET)), linewidth = 1) +
      geom_line(data = tide_pred(), aes(x = Time_ET, y = prediction, color = "Predicted Water Level", 
                                        text = paste0("Predicted tide height of ", round(prediction, 2)," at ", Time_ET)), linetype = 'dotted', linewidth =1) +
      geom_hline(aes(yintercept = LW_elev, color = 'Elevation at Long Wharf'), linetype = 'dashed', linewidth = 1) + 
      scale_color_manual(
          values = c("darkred","#002366", "#2E3440" )) + 
      plot_theme() + 
      theme(legend.position = 'bottom', 
            legend.title = element_blank())
  
  if (shinybrowser::is_device_mobile()){
    text_size = 7
    title_size = 9
    legend_size = 6
  } else{
    text_size = 16
    title_size = 18
    legend_size = 16
  }
  
  ggplotly(p, tooltip = "text") %>%
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
  
  }) #end plot
  
} #end server


shinyApp(ui, server)

#shinyApp(ui, server, options = list(launch.browser = TRUE))
