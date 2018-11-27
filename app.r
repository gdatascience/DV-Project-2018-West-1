library(shiny)
library(ggplot2)
library(markdown)
library(leaflet)
library(rgdal)

server = function(input, output, session) {
  # Load the street lights data
  street_lights <- read.csv("Street_Lights.csv", stringsAsFactors = F)
  street_lights[street_lights$Pole_Type %in% c(""," "),]$Pole_Type <- "Unknown"
  street_lights[street_lights$Service %in% c(""," "),]$Service <- "Unknown"
  street_lights$Inspect_Date2 <- as.Date(street_lights$Inspect_Date)
  
  # Load the public facilities data
  facilities.points <- read.csv("Public_Facilities.csv")
  facilities.spatial <- SpatialPointsDataFrame(coords = facilities.points[,c("Lon","Lat")], 
                                               data = facilities.points,
                                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  facilities.spatial$popup = paste("<b>",facilities.spatial$POPL_NAME,"</b><br>",
                                   "Type: ",facilities.spatial$POPL_TYPE,"<br>",
                                   "Phone: ",facilities.spatial$POPL_PHONE,sep ="")
  
  data <- eventReactive(input$dates,{
    return(street_lights[street_lights$Inspect_Date2 >= input$dates[1]& street_lights$Inspect_Date2 <= input$dates[2],])
    })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
  output$map <- renderLeaflet({
    leaflet(data = data()) %>%
      addTiles() %>%
      addCircleMarkers(stroke = 0, fillOpacity = 1, radius = 1) %>%
      addMarkers(data = facilities.spatial, popup = ~popup)
    })
}

ui = navbarPage(
  title = "Mayor Pete Dashboard",
  tabPanel("Dan",
           sidebarLayout(
             sidebarPanel(
               radioButtons("plotType", "Plot type",
                            c("Scatter"="p", "Line"="l")
                            )
               ),
             mainPanel(
               plotOutput("plot")
               )
             )
           ),
    tabPanel("Gerard",
            verbatimTextOutput("summary")
    ),
    tabPanel("Mike",
            DT::dataTableOutput("table")
    ),
    tabPanel("Tony",
            dateRangeInput(inputId = "dates", label = "Date range", startview = "year",start="2010-01-01"),
            leafletOutput(outputId = "map")
    )
)


shinyApp(ui = ui, server = server)