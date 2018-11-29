library(shiny)
library(ggplot2)
library(markdown)
library(leaflet)
library(rgdal)
library(lubridate)
library(tidyverse)
library(ggmap)
library(raster)
library(DT)

## DAN DATA LOAD AND MANIPULATE

# load abandoned parcel data
ab_property = readOGR(dsn="Abandoned_Property_Parcels", 
                      layer = "Abandoned_Property_Parcels", 
                      stringsAsFactors = FALSE)

# import school boundary data
ab_school_boundaries <- readOGR(dsn="School_Boundaries", 
                                layer = "School_Boundaries", stringsAsFactors = FALSE)

# convert all NA values for Outcome to 'Not Fixed'
ab_property@data$Outcome_St[is.na(ab_property@data$Outcome_St)] <- "Not Fixed"

# generate the basemap for South Bend abandoned properties
ab.base.map <- ggmap::get_stamenmap(bbox = c(left = -86.36, bottom = 41.59, 
                                             right = -86.14,top = 41.76), zoom = 12)

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
  
  # Load the city council districts data
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
  
  # DAN SERVER DATA SECTION
  
  # create output for abandoned properties map
  output$ab_map <- renderLeaflet({
    
    # create color palette based on property outcome state
    ab_pal <- colorFactor(palette = 'Set1', domain = ab_property$Outcome_St)
    
    # create property popup
    ab_property$ab_popup <- paste("<b>",ab_property$Address_Nu, " ", " ", ab_property$Direction, " ", 
                               ab_property$Street_Nam, " ", ab_property$Suffix, "</b><br>",
                               "State: ",ab_property$Outcome_St,"<br>",
                               "City Code: ",ab_property$Code_Enfor,sep ="")
    
    # create school popup
    ab_school_boundaries$sch_popup <- paste("<b>", ab_school_boundaries$School)
    
    # now dynamically build the filtered list based on checkbox selection
    ab_displayList <- ab_property[ab_property@data$Outcome_St %in% c(input$state),] 
    
    # build map  
    ab_city_map <- leaflet()  %>%
      addTiles() %>%
      addPolygons(data = ab_displayList, color = ~ab_pal(Outcome_St), 
                  fillOpacity = 1, popup = ~ab_popup) %>%
      addLegend(data = ab_displayList, 
                pal = ab_pal, opacity = 1, 
                values = ~Outcome_St, position = "bottomleft", title = "Property Status")
    
    # If statement to determine if the display map should have school boundaries or not
    if(input$schools == TRUE){
      ab_city_map %>%
        addPolygons(data = ab_school_boundaries, color = "black", popup = ~sch_popup)
    }
    else{
      ab_city_map
    }
  })
  
  # render table
  output$ab_table <- renderDataTable(
    
    # now dynamically build the table based on selected checkboxes
    datatable(ab_property[ab_property@data$Outcome_St %in% c(input$state),]@data)
  )
  
  ## END DAN SERVER SECTION
  
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
                title = "City Council Representative",
                pal = colorFactor(palette = 'Set1', domain = districts@data$Council_Me))
    })
}

ui = navbarPage(
  title = "Mayor Pete Dashboard",
  tabPanel("Abandoned Buildings",
           sidebarLayout(
             sidebarPanel(
               # first set of checkboxes for Property Status
               checkboxGroupInput("state", "Current Property Status:",
                                  c("Demolished" = "Demolished",
                                    "Deconstructed" = "Deconstructed",
                                    "Repaired" = "Repaired",
                                    "Repaired & Occupied" = "Repaired & Occupied",
                                    "Occupied & Not Repaired" = "Occupied & Not Repaired",
                                    "Not Fixed" = "Not Fixed"),
                                  selected = c("Demolished", "Deconstructed", "Repaired",
                                               "Repaired & Occupied", "Occupied & Not Repaired", 
                                               "Not Fixed")),
               
               # Individual checkbox for inclusion of school boundaries
               checkboxInput("schools", "Include School Boundaries?")
               ),
             
             mainPanel(
               # display two tabbed output with "Map" and "Table"
               tabsetPanel(
                 tabPanel("Map", leafletOutput("ab_map",height = 500)),
                 tabPanel("Table", dataTableOutput("ab_table"))
               )
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