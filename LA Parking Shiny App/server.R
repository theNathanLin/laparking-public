server <- function(input, output) {
  
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
      labs(title = "Ticket Geographic Density & Distribution", caption = "Source: City of Los Angeles") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
    
    
  })
  
  output$timemap <- renderPlot({
    
    ggplot(data = filter(selected_data(), `Violation Description` %in% input$Violation[1:4])) +
      geom_density(aes(x = `Issue time2`, color = `Violation Description`, fill = `Violation Description`), alpha = 0.05, size = .7) + 
      scale_x_datetime(labels = date_format("%I:%M %p", tz = "EST"), date_breaks = "4 hours",
                       limits = c(as.POSIXct("2019-01-01 00:01:00 EST"), as.POSIXct("2019-01-01 23:59:00 EST"))) +
      theme_light() + 
      labs(title = "Time Distribution of Top 4 Selected Violations",
           x = "Time", y = "Density", fill="Violation Type", color = "Violation Type")
    
    
  })
  
  output$barcount <- renderPlot({
  
      ggplot(data = filter(selected_data(), `Violation Description` %in% input$Violation[1:3])) + 
      geom_bar(aes(x = `Violation Description`, y = stat(count), fill = `Violation Description`)) + 
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
