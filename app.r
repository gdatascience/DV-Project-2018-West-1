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
library(geosphere)
library(rgeos)
library(maptools)
library(shinycssloaders)
library(RColorBrewer)

gpclibPermit()

########################################
## DAN DATA LOAD AND MANIPULATE
########################################

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

########################################
# GERARD DATA LOAD AND MANIPULATE
########################################

register_google(key = "")

# Import public facilities data
gm_facilities <- read.csv("Public_Facilities.csv", stringsAsFactors = F) %>%
  dplyr::select(POPL_NAME, POPL_TYPE, Lat, Lon) %>%
  rename(Name = POPL_NAME, Type = POPL_TYPE) %>%
  na.omit() %>%
  mutate(Dataset = "Facilities")

# Import parks data
gm_parks <- read.csv("Parks_Locations_and_Features.csv", stringsAsFactors = F) %>%
  dplyr::select(Park_Name, Park_Type, Lat, Lon) %>%
  rename(Name = Park_Name, Type = Park_Type) %>%
  na.omit() %>%
  mutate(Dataset = "Parks")

# Import school data.
gm_schools_ogr <- readOGR(dsn="School_Boundaries", layer = "School_Boundaries")
gm_schools_coordinates <- coordinates(gm_schools_ogr)

gm_schools <- data.frame(
  Name = as.character(gm_schools_ogr@data$School),
  Type = as.character(gm_schools_ogr@data$SchoolType),
  Lat = gm_schools_coordinates[,2],
  Lon = gm_schools_coordinates[,1],
  Dataset = "Schools"
)

# Combine datasets
gm_combined_data <- rbind(gm_facilities, gm_parks, gm_schools) %>%
  mutate(Dataset = as.factor(Dataset))

gm_schools_parks <- gm_combined_data %>%
  filter(Dataset != "Facilities")

# Get nearest facility
gm_find_nearest_facility <- function(longitude, latitude, facility_type) {
  desired_facilities <- gm_combined_data %>%
    filter(Dataset == "Facilities" & Type == facility_type)

  distances <- apply(desired_facilities, 1, FUN = function(row) {
    distm(c(as.numeric(row[4]), as.numeric(row[3])),
          c(longitude, latitude),
          fun = distCosine)
  })
  return(desired_facilities[which.min(distances),])
}

# Get distances to facilities
gm_get_distance_to_facility <- function(longitude, latitude, facility_type) {
  desired_facilities <- gm_combined_data %>%
    filter(Dataset == "Facilities" & Type == facility_type)

  distances <- apply(desired_facilities, 1, FUN = function(row) {
    distm(c(as.numeric(row[4]), as.numeric(row[3])),
          c(longitude, latitude),
          fun = distCosine)
  })

  desired_facilities$distance = distances
  return(desired_facilities)
}

################################
## MIKE DATA LOAD AND MANIPULATE
################################

# setwd to current directory where script is running
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load census data
census_dat = readOGR(dsn="2010_CensusData", layer = "2010_CensusData", stringsAsFactors = FALSE)

# load parks-locations-features data
plf_dat <- read.csv("Parks_Locations_and_Features.csv")

# generate the basemap for South Bend
#sb.base.map <- ggmap::get_stamenmap(bbox = c(left = -86.36, bottom = 41.59, right = -86.14,top = 41.76), zoom = 12)

# wrangle census data
census_dat2 <- census_dat
#head(census_dat2,1)
census_dat2$tot_pop = census_dat2$SE_T001_00
census_dat2$tot_pop = as.integer(census_dat2$tot_pop)
cols <- grep("SE_T", names(census_dat2@data))
census_dat2$pdensity <- round(census_dat2$SE_T002_01,0)
census_dat2@data <- census_dat2@data %>% dplyr::select(-cols)

#head(census_dat2,1)

#census_dat2@data <- census_dat2@data %>% select(Geo_QName, tot_pop)

census_dat3 <- fortify(census_dat2, region = 'Geo_QName')
census_dat3 <- merge(census_dat3, census_dat2@data, by.x = 'id', by.y = 'Geo_QName')

#head(census_dat3,1)

# wrangle park-locations-features data
plf_spatial <- SpatialPointsDataFrame(coords = plf_dat[,c("Lon","Lat")], data = plf_dat, 
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))
#head(plf_spatial,1)

#Group By id and summarize long and lat
#Create single representation of census location by mean of boundaries
census_dat4 <- census_dat3 %>% group_by(NAME) %>% summarise(long=mean(long), lat=mean(lat))
tot_pop = NA
census_dat4 <- cbind(census_dat4, tot_pop)

#Assign tot_pop to new dataframe with mean of boundaries
n = nrow(census_dat4)
count=1
for(i in 1:n){
  flag=0
  while(flag == 0){
    if(census_dat4$NAME[i] == census_dat3$NAME[count]){
      census_dat4$tot_pop[i] = census_dat3$tot_pop[count]
      census_dat4$NAMELSAD[i] = census_dat3$NAMELSAD[count]
      census_dat4$pdensity[i] = census_dat3$pdensity[count]
      flag=1
    }
    count=count+1
  }
}
#head(census_dat4,1)

########################################
## TONY DATA LOAD AND MANIPULATE
########################################
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


################################################################################
# SERVER
################################################################################
server = function(input, output, session) {
  
  ########################################
  # DAN SERVER DATA SECTION
  ########################################
  
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
  
  ########################################
  # TONY SERVER DATA SECTION
  ########################################
  
  # create reactive data object
  data <- eventReactive(input$dates,{
    return(street_lights[(street_lights$Inspect_Date2 >= input$dates[1] & 
                            street_lights$Inspect_Date2 <= input$dates[2]) |
                           is.na(street_lights$Inspect_Date2),])
    })
  
  # create leaflet map
  output$map <- renderLeaflet({
    lights_map = leaflet() %>%
      addTiles() %>%
      setView(mean(street_lights$Lon), mean(street_lights$Lat), zoom = 12) %>%
      addPolygons(data = districts, 
                  color = ~pal2(Council_Me), popup = ~popup) %>%
      addCircleMarkers(data = data(), lng =  ~Lon, lat = ~Lat, 
                       color = ~pal1(color),
                       stroke = 0, fillOpacity = 1, radius = 1.5) %>%
      addLegend(values = data()$color, position = "topright", 
                title = "Light Inspection Status",
                pal = colorFactor(palette = c("red", "yellow", "green"), domain = street_lights$color)) %>%
      addLegend(values = districts@data$Council_Me, position = "bottomleft",
                title = "City Council Representative",
                pal = colorFactor(palette = 'Set1', domain = districts@data$Council_Me))
    
    # If statement to determine if the display map should have public facilities or not
    if(input$show_facilities == TRUE){
      lights_map = lights_map %>%
        addMarkers(data = facilities.spatial, popup = ~popup)
    }
    
    return(lights_map)
    })

  ########################################
  # GERARD SERVER DATA SECTION
  ########################################
  output$gm_bar <- renderPlot(
    {
      # station color
      if (input$gm_facility_type == "POLICE STATION") {
        station_color <- "blue"
      } else {
        station_color = "darkred"
      }

      current_school_park <- gm_combined_data %>%
        filter(Name == input$gm_selected_school_park)

      distances <- gm_get_distance_to_facility(current_school_park$Lon,
                                               current_school_park$Lat,
                                               input$gm_facility_type)

      distances %>%
        arrange(distance) %>%
        head(n = 5) %>%
        ggplot(aes(x = reorder(Name, -distance), y = distance)) +
        geom_bar(stat="identity", fill = station_color) +
        xlab("") + ylab("Distance (m)") + ggtitle("Distance to Station") + 
        theme_minimal() +
        coord_flip()
    }
  )

  map_size <- 740
  output$gm_map <- renderPlot(
    {
      # constants
      pt_size <- 8
      current_color <- "black"

      # station color
      if (input$gm_facility_type == "POLICE STATION") {
        station_color <- "blue"
      } else {
        station_color = "darkred"
      }

      # school / park
      current_school_park <- gm_combined_data %>%
        filter(Name == input$gm_selected_school_park)

      # nearest station
      nearest_station <- gm_find_nearest_facility(current_school_park$Lon,
                                                  current_school_park$Lat,
                                                  input$gm_facility_type)

      # route
      route_df <- route(revgeocode(c(current_school_park$Lon, current_school_park$Lat)),
                        revgeocode(c(nearest_station$Lon, nearest_station$Lat)),
                        structure = "route")

      routeQueryCheck()

      # create map
      google_map <- qmap(c(current_school_park$Lon, current_school_park$Lat), zoom = 13) +
        # starting point / route
        geom_point(data = current_school_park,
                   aes(x = Lon, y = Lat), size = pt_size, color = current_color) +
        geom_path(data = route_df,
                  aes(x = lon, y = lat),
                  colour = current_color, size = 1.5,
                  alpha = 1, lineend = "round") +
        # station(s)
        geom_point(data = gm_combined_data %>% filter(Type == input$gm_facility_type),
                   aes(x = Lon, y = Lat), size = pt_size, alpha = 0.4, color = station_color) +
        geom_text(data = nearest_station,
                  aes(x = Lon, y = Lat, label = Name), color = station_color,
                  size = 8, vjust = -1) +
        # title
        ggtitle(paste(current_school_park$Name, "to", nearest_station$Name)) +
        # theme
        theme(plot.title = element_text(size = 25))

      # print map
      print(google_map)
    },
    height = map_size,
    width = map_size
  )
  ########################################
  # END GERARD SERVER DATA SECTION
  ########################################
  
  #################################
  ## MIKE SERVER DATA SECTION
  #################################
  
  # Adjust user input
  output$maps.output <-renderUI(
    
  if ( ("Population" %in% input$maps) & ("Population Density" %in% input$maps) ){
    print("Invalid Combination")
    return( updateCheckboxGroupInput(session, "maps", selected = "Geographic"))
  })
  
  # create output 
  output$census_plot <- renderLeaflet({
  
    # Create Color Palettes
    custcol1 <- brewer.pal(8, "Blues")[1:8]
    custcol2 <- brewer.pal(8, "Greens")[1:8]
    pop_pal <- colorNumeric(palette = custcol1, domain = census_dat2@data$tot_pop)
    den_pal <- colorNumeric(palette = custcol2, domain = census_dat2@data$pdensity)
    plf_pal <- colorFactor(palette = 'Set1', domain = plf_spatial@data$Park_Type)
    

    # create popups
    census_dat2$pop_popup <- paste("<b>", census_dat2@data$NAMELSAD, "<br>", 
                                 "Population = ", census_dat2@data$tot_pop)
    census_dat2$den_popup <- paste("<b>", census_dat2@data$NAMELSAD, "<br>", 
                                 "Population Density (per sq mi) = ", census_dat2@data$pdensity)
    plf_spatial$plf_popup <- paste("<b>", plf_spatial$Park_Name)
    
    # Dynamically select park types to display based on selection
    plf_displayList <- as.data.frame(plf_spatial[plf_spatial@data$Park_Type %in% c(input$ptype),])
    
    # Dynamic Display Geographic and Population
     if( ("Geographic" %in% input$maps) & ("Population" %in% input$maps) & !("Population Density" %in% input$maps) ){
      leaflet()  %>%
        addTiles() %>%
        addPolygons(data = census_dat2, fill = census_dat2@data$tot_pop, popup = ~pop_popup, stroke = TRUE, 
                  fillColor = ~pop_pal(tot_pop), color = "white", fillOpacity = .60) %>%
        addLegend(values = census_dat2@data$tot_pop, position = "bottomleft", 
                title = "Total Population", pal = pop_pal) %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)
      
    # Dynamic Display Geographic and Density    
    }else if( ("Geographic" %in% input$maps) & !("Population" %in% input$maps) & ("Population Density" %in% input$maps) ){ 
      leaflet()  %>%       
        addTiles() %>%
        addPolygons(data = census_dat2, fill = census_dat2@data$pdensity, popup = ~den_popup, stroke = TRUE, 
                  fillColor = ~den_pal(pdensity), color = "white", fillOpacity = .60) %>%
        addLegend(values = census_dat2@data$pdensity, position = "bottomleft", 
                title = "Population Density per SQ MI", pal = den_pal) %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)
      
    # Dynamic Display Population
      }else if( !("Geographic" %in% input$maps) & ("Population" %in% input$maps) & !("Population Density" %in% input$maps) ){
      leaflet()  %>%
        addPolygons(data = census_dat2, fill = census_dat2@data$tot_pop, popup = ~pop_popup, stroke = TRUE, 
                  fillColor = ~pop_pal(tot_pop), color = "white", fillOpacity = 1) %>%
        addLegend(values = census_dat2@data$tot_pop, position = "bottomleft", 
                title = "Census Population", pal = pop_pal) %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)

    # Dynamic Display Density    
    }else if( !("Geographic" %in% input$maps) & !("Population" %in% input$maps) & ("Population Density" %in% input$maps) ){ 
      leaflet()  %>%       
        addPolygons(data = census_dat2, fill = census_dat2@data$pdensity, popup = ~den_popup, stroke = TRUE, 
                  fillColor = ~den_pal(pdensity), color = "white", fillOpacity = 1) %>%
        addLegend(values = census_dat2@data$pdensity, position = "bottomleft", 
                title = "Population Density per SQ MI", pal = den_pal) %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)        

    # Dynamic Display Geographic
      }else if( ("Geographic" %in% input$maps) & !("Population" %in% input$maps) & !("Population Density" %in% input$maps) ){ 
      leaflet()  %>%       
        addTiles() %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)
        
    # Dynamic Display Geographic
      }else if( ("Geographic" %in% input$maps) & ("Population" %in% input$maps) & ("Population Density" %in% input$maps) ){ 
      leaflet()  %>%       
        addTiles() %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                       color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                       radius = 6, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)

    # Default Parks Only        
    }else{
      leaflet()  %>%
        addCircleMarkers(data = plf_displayList, lng =  ~Lon, lat = ~Lat,
                     color = ~plf_pal(Park_Type), stroke = 0, fillOpacity = .85, 
                     radius = 5, popup = ~plf_popup) %>%
        addLegend(values = plf_spatial@data$Park_Type, position = "bottomright", 
                title = "Park Type", pal = plf_pal)
     }
})

  
  ###############################
  ## END MIKE SERVER DATA SECTION
  ###############################
  
  
  
}

################################################################################
# UI
################################################################################
ui = navbarPage(
  title = "South Bend Dashboard",
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
    tabPanel("Nearest Safety Stations",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "gm_facility_type",
                   label = "Station Type",
                   choices = c("Fire Station"="FIRE STATION", "Police Station"="POLICE STATION"),
                   selected = "FIRE STATION"
                 ),
                 selectInput(
                   inputId = "gm_selected_school_park",
                   label = "Schools and Parks",
                   choices = sort(gm_schools_parks$Name)
                 ),
                 withSpinner(plotOutput("gm_bar"))
               ),
               mainPanel(
                 withSpinner(plotOutput("gm_map"))
               )
             )
            ),
 
    #########
    # Mike UI 
    #########

    tabPanel("Parks to Population Alignment",
           sidebarLayout(
             sidebarPanel(
               # first set of checkboxes for Park Type
               checkboxGroupInput("ptype", "Park Types:",
                                  c("Block Park" = "Block Park",
                                    "Cemetery" = "Cemetary",
                                    "Community Park" = "Community Park",
                                    "Golf Course" = "Golf Course",
                                    "Memorial" = "Memorial",
                                    "Neighborhood Park" = "Neighborhood Park",
                                    "Special" = "Special",
                                    "Zoo" = "Zoo"),
                                  selected = c("Block Park", "Cemetary", "Community Park", "Golf Course", "Memorial", "Neighborhood Park", "Special", "Zoo")),
             
               # Individual checkbox for inclusion of area map
               checkboxGroupInput("maps", "Maps:",
                                  c("Geographic" = "Geographic",
                                    "Population" = "Population",
                                    "Population Density" = "Population Density"),
                                  selected = c("Geographic")),
               uiOutput(outputId="maps.output")),
             
               mainPanel(leafletOutput("census_plot",height = 650, width=650))
             )
          ),  
  
    #############
    # End Mike UI 
    #############
  
    tabPanel("City Lights",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(inputId = "dates", 
                                label = "Inspection Date Range", 
                                startview = "year",
                                start="1970-01-01"),
                 
                 # Individual checkbox for inclusion of public facilities
                 checkboxInput("show_facilities", "Show Public Facilities?")
                 ),
               mainPanel(
                 "City Lights by Inspection Status", 
                 leafletOutput(outputId = "map")
                 )
               )
             )
)


shinyApp(ui = ui, server = server)
