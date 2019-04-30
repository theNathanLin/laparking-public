## LA Parking Shiny App

#### Packages  --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(proj4)
library(ggmap)
library(maps)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(scales)

#### Data Input  -----------------------------------------------------------------------------------------------------
laparking <- read_csv("~/Downloads/parking-citations.csv")
agencies <- read_csv("state_agencies.csv")

laparking_2 <- laparking %>%
  filter(`Issue Date` >= as.Date("2019-01-01"), `Issue Date` <= as.Date("2019-04-15")) %>%
  select(-`Location`, -`Ticket number`, -`Meter Id`, -`Marked Time`, -VIN, -Route, -`Violation code`, -`Plate Expiry Date`, -Make, -`Body Style`, -`Color`, -`Fine amount`) %>%
  mutate(Latitude = ifelse(Latitude == 99999, NA, Latitude), 
         Longitude = ifelse(Longitude == 99999, NA, Longitude)) %>%
  mutate(`Issue time` = ifelse(nchar(`Issue time`)==3, paste("0", `Issue time`, sep = ""), `Issue time`)) %>%
  mutate(`Issue time` = as.POSIXct(`Issue time`, format = "%H%M")) %>%
  mutate(`Issue time` = format(`Issue time`,"%H%M")) %>%
  mutate(`Issue time2` = as.POSIXct(paste("01-01-2019", `Issue time`, sep = " "), format ="%m-%d-%Y %H%M")) %>%
  left_join(agencies[,c(1,3)], by = c("Agency" = "Code")) %>%
  select(-`Agency`, -`Issue time`) %>%
  drop_na()

proj4string <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"

pj <- project(laparking_2[, c("Latitude", "Longitude")], proj4string, inverse=TRUE)
latlong <- data.frame(New.Latitude=pj$y, New.Longitude=pj$x)
laparking_2 <- cbind(laparking_2, latlong)

laparking_2 <- laparking_2 %>%
  select(-Latitude, -Longitude) %>%
  filter(New.Latitude >= 34.00664, New.Latitude <= 34.09766, New.Longitude >= -118.2985, New.Longitude <= -118.1887)

laparking_2 <- laparking_2 %>%
  filter(`Violation Description` %in% violations_list$`Violation Description`[1:15])

violations_list <- laparking_2 %>%
  group_by(`Violation Description`) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

violations_list <- violations_list[,1]

states_non_ca <- levels(factor(laparking_2$`RP State Plate`))
states_non_ca<- states_non_ca[!states_non_ca %in% "CA"]

#### Maps  -----------------------------------------------------------------------------------------------------------
# Register a Google API key

la.map1 <- ggmap(get_map(location = 'Los Angeles', zoom = 13))
la.map_bw <- ggmap(get_map(location = 'Los Angeles', zoom = 13, color = "bw"))

#### Shiny App  ------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Parking Citations in Los Angeles"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = 'IssueDate', label = 'Issue Date Range:',
                     min = as.Date("01/01/2016","%m/%d/%Y"), max = as.Date("04/15/2019","%m/%d/%Y"),
                     start = as.Date("01/01/2019","%m/%d/%Y"), end = as.Date("04/15/2019","%m/%d/%Y"),
                     format = "mm/dd/yyyy"),
      
      sliderInput(inputId = "IssueTime", label = "Issue Time:",
                  min = as.POSIXct("2019-01-01 0001","%Y-%m-%d %H%M", tz = "EST"),
                  max = as.POSIXct("2019-01-01 2359","%Y-%m-%d %H%M", tz = "EST"),
                  value=c(as.POSIXct("2019-01-01 0001","%Y-%m-%d %H%M", tz = "EST"), as.POSIXct("2019-01-01 2359","%Y-%m-%d %H%M", tz = "EST")),
                  timeFormat="%I:%M %p", animate = animationOptions(interval = 1500, loop = TRUE)),
      
      pickerInput(inputId = "Violation", label = "Violations:", 
        choices = as.vector(violations_list[1:25,1]), 
        selected = pull(violations_list[1:5,1]),
        options = list(
          `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3",
          `select-all-text` = "Select All Violations",
          `none-selected-text` = "No Violations Selected"),
        multiple = TRUE),
      
      pickerInput(inputId = "Plates", label = "State Plates:", 
                  choices = c("CA", "Non-CA"), 
                  selected = c("CA", "Non-CA"),
                  options = list(
                    `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3",
                    `none-selected-text` = "No States Selected"),
                  multiple = TRUE),
      
      pickerInput(inputId = "Agency", label = "Ticketing Agency", 
                  choices = agencies[,3], 
                  selected = pull(agencies[,3]),
                  options = list(
                    `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3",
                    `select-all-text` = "Select All Agencies",
                    `none-selected-text` = "No Agencies Selected"),
                  multiple = TRUE),
      
      materialSwitch(inputId = "black_white", label = "Black/White Maps?", status = "success", value = TRUE),
      
      uiOutput("plotDiv")
      
      
    ), #Don't delete
    
    mainPanel(
      plotOutput(outputId = "heatmap", height = "550px"),
      plotOutput(outputId = "timemap", height = "300px")
      
    )
    
  )
)

server <- function(input, output, session) {
  
  # Subset data
  selected_data <- reactive(
    
    laparking_2 %>%
      filter(`Issue Date` >= as.Date(input$IssueDate[1]), `Issue Date` <= as.Date(input$IssueDate[2]))  %>%
      filter(`Violation Description` %in% input$Violation) %>%
      filter(`RP State Plate` %in% ifelse(input$Plates == "CA", "CA", 
                                          ifelse(input$Plates == "Non-CA", states_non_ca, c("CA", states_non_ca)))) %>%
      filter(`Issue time2` >= input$IssueTime[1],
             `Issue time2` <= input$IssueTime[2]) %>%
      filter(Short_Name %in% input$Agency) %>%
      drop_na()
    )
  
  map_type <- reactive({
    
    if(input$black_white == TRUE){
      la.map_bw
    } else{
      la.map1
    }
    
  })
  
  output$heatmap <- renderPlot({
    
    map_type() + 
      stat_density2d(data = selected_data(), aes(x = New.Longitude, y = New.Latitude, fill = stat(level), alpha = stat(level)),
                     geom = "polygon", size = 0.01, na.rm = TRUE) +
      scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(guide = 'none') +
      labs(title = "Geographic Density & Distribution", subtitle = "Tickets issued from 1/1/2019 to 4/15/2019",
           caption = "Source: City of Los Angeles") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
    
    
  })
  
  timemap_data <- reactive(
    
    laparking_2 %>%
      filter(`Issue Date` >= as.Date(input$IssueDate[1]), `Issue Date` <= as.Date(input$IssueDate[2]))  %>%
      filter(`Violation Description` %in% input$Violation) %>%
      filter(`RP State Plate` %in% ifelse(input$Plates == "CA", "CA", 
                                          ifelse(input$Plates == "Non-CA", states_non_ca, c("CA", states_non_ca)))) %>%
      filter(`Issue time2` >= input$IssueTime[1],
             `Issue time2` <= input$IssueTime[2]) %>%
      filter(Short_Name %in% input$Agency) %>%
      drop_na()
  )
  
  output$timemap <- renderPlot({
    
    selected_data() %>%
      select(`Violation Description`, `Issue time2`) %>%
      filter(`Violation Description` %in% input$Violation[1:5]) %>%
      ggplot() + geom_density(aes(x = `Issue time2`, color = `Violation Description`, fill = `Violation Description`), alpha = 0.05, size = .7) + 
      scale_x_datetime(labels = date_format("%I:%M %p", tz = "EST"), date_breaks = "4 hours",
                       limits = c(as.POSIXct("2019-01-01 00:01:00 EST"), as.POSIXct("2019-01-01 23:59:00 EST"))) +
      theme_light() + 
      labs(title = "Time Distribution of Top 5 Selected Violations",
           x = "Time", y = "Density", fill="Violation Type", color = "Violation Type")
    
    
  })
  
  output$barcount <- renderPlot({
    
    selected_data() %>%
      group_by(`Violation Description`) %>%
      filter(`Violation Description` %in% input$Violation[1:3]) %>%
      ggplot() + geom_bar(aes(x = `Violation Description`, y = stat(count), fill = `Violation Description`)) + 
      theme_light() + theme(panel.background = element_rect(fill="#F5F5F5", color='#F5F5F5'),
                            plot.background = element_rect(fill="#F5F5F5", color='#F5F5F5')) +
      labs(title = "Top 3 Violation Counts",
           x = "Violation", y = "Count", fill="Violation Type") +
      guides(fill=FALSE)
    
    
  })
  
  output$plotDiv <- renderUI({
      plotOutput("barcount")
  })
  
}

shinyApp(ui = ui, server = server)

