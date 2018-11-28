library(shiny)
library(ggplot2)
library(markdown)
library(leaflet)
library(rgdal)

server = function(input, output, session) {
  # Load the street lights data
  street_lights <- read.csv("Street_Lights.csv", stringsAsFactors = F)
  street_lights$Inspect_Date2 <- as.Date(street_lights$Inspect_Date)
  street_lights$color <- if_else(is.na(street_lights$Inspect_Date2), "Inspect ASAP", 
                                 if_else(year(street_lights$Inspect_Date2) > 2007, 
                                         "Recently Inspected", 
                                         if_else(year(street_lights$Inspect_Date2) > 2000, 
                                                 "May Need Inspection", "Inspect ASAP")))
  pal1 = colorFactor(palette = c("red", "yellow", "green"), domain = street_lights$color)
  
  # Load the public facilities data
  facilities.points <- read.csv("Public_Facilities.csv")
  facilities.spatial <- SpatialPointsDataFrame(coords = facilities.points[,c("Lon","Lat")], 
                                               data = facilities.points,
                                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  facilities.spatial$popup = paste("<b>",facilities.spatial$POPL_NAME,"</b><br>",
                                   "Type: ",facilities.spatial$POPL_TYPE,"<br>",
                                   "Phone: ",facilities.spatial$POPL_PHONE,sep ="")
  
  # Load the city counsil districts data
  districts = readOGR(dsn="City_Council_Districts", 
                      layer = "City_Council_Districts", 
                      stringsAsFactors = FALSE)
  districts$popup = paste("<b>", districts@data$Council_Me, "</b><br>",
                          "Email: <a href=\"mailto:", districts@data$Email, "\">", districts@data$Email, "</a><br>",
                          "District #: ", districts@data$Dist,sep ="")
  pal2 = colorFactor(palette = 'Set1', domain = districts@data$Council_Me)
  
  data <- eventReactive(input$dates,{
    return(street_lights[(street_lights$Inspect_Date2 >= input$dates[1] & 
                           street_lights$Inspect_Date2 <= input$dates[2]) |
                           is.na(street_lights$Inspect_Date2),])
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
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = districts, 
                  color = ~pal2(Council_Me), popup = ~popup) %>%
      addCircleMarkers(data = data(), lng =  ~Lon, lat = ~Lat, 
                       color = ~pal1(color),
                       stroke = 0, fillOpacity = 1, radius = 1) %>%
      addMarkers(data = facilities.spatial, popup = ~popup) %>%
      addLegend(values = data()$color, position = "topright", 
                title = "Light Inspection Status",
                pal = colorFactor(palette = c("red", "yellow", "green"), domain = street_lights$color)) %>%
      addLegend(values = districts@data$Council_Me, position = "bottomleft",
                title = "City Counsil District",
                pal = colorFactor(palette = 'Set1', domain = districts@data$Council_Me))
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
    tabPanel("City Lights Map",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(inputId = "dates", 
                                label = "Inspection date range", 
                                startview = "year",
                                start="1970-01-01")
                 ),
               mainPanel(
                 "City Lights by Inspection Status", 
                 leafletOutput(outputId = "map")
                 )
               )
             )
)


shinyApp(ui = ui, server = server)