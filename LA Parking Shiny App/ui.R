ui <- fluidPage(
  
  titlePanel("Parking Citations in Los Angeles"),
  sidebarLayout(
    sidebarPanel(
      p(span("Produced by Nathan Lin, Johnathan Robinson, & Nick McDermott", style = "color:blue")),
      dateRangeInput(inputId = 'IssueDate', label = 'Issue Date Range:',
                     min = as.Date("01/01/2019","%m/%d/%Y"), max = as.Date("04/15/2019","%m/%d/%Y"),
                     start = as.Date("01/01/2019","%m/%d/%Y"), end = as.Date("04/15/2019","%m/%d/%Y"),
                     format = "mm/dd/yyyy"),
      
      sliderInput(inputId = "IssueTime", label = "Issue Time:",
                  min = as.POSIXct("2019-01-01 0001","%Y-%m-%d %H%M", tz = "EST"),
                  max = as.POSIXct("2019-01-01 2359","%Y-%m-%d %H%M", tz = "EST"),
                  value=c(as.POSIXct("2019-01-01 0001","%Y-%m-%d %H%M", tz = "EST"), as.POSIXct("2019-01-01 2359","%Y-%m-%d %H%M", tz = "EST")),
                  timeFormat="%I:%M %p", animate = animationOptions(interval = 1500, loop = TRUE)),
      
      pickerInput(inputId = "Violation", label = "Violations:", 
                  choices = as.vector(violations_list[1:15,1]), 
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
