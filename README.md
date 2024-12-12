# walkable

This is to explore walkability:

1.Healthy Stores and Farmer Markets
2.Subway stations (nyc) 
3.Walking distance: within 400 meters around subway stations.

change the rmarkdown file name from "walkable" to "shiny_walkability"

---
title: "Walkability to Food Sources from Subway Stations"
output: 
  flexdashboard::flex_dashboard:
    source: embed
runtime: shiny
---

```{r setup, include=FALSE}
library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(rsconnect)
```


```{r map, echo=FALSE}

stores <- read.csv("./data/stores_markets_retails.csv")
subways <- read.csv("./data/subway.csv")

# Clean data by removing rows with NA values in latitude/longitude
stores = stores |> filter(!is.na(latitude) & !is.na(longitude))
subways = subways |>  filter(!is.na(latitude) & !is.na(longitude))

# Convert to sf objects
stores_sf = st_as_sf(stores, coords = c("longitude", "latitude"), crs = 4326)
subways_sf = st_as_sf(subways, coords = c("longitude", "latitude"), crs = 4326)


# Define the UI

ui = fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      # Only keep the selectInput for Subway Line
      selectInput("subway_line", "Select Subway Line:",
                  choices = unique(subways$line), selected = unique(subways$line)[1])
    ),
    mainPanel(
      # Output leaflet map
      leafletOutput("map", height = "100vh", width = "100%")
    )
  )
)

server = function(input, output, session) {
  
  
  # Reactive data based on selected subway line
  filtered_data = reactive({
    # Filter subway stations by selected subway line
    subways_filtered = subways_sf |>  filter(line == input$subway_line)
    
    # Return the filtered subways only
    list(subways = subways_filtered)
  })
  
  # Render the map
  output$map = renderLeaflet({
    data = filtered_data()
    
    # Create 400m buffer around selected subway stations
    subway_buffers = st_buffer(data$subways, dist = 400)
    
    # Find stores within the 400m buffer
    stores_within_buffer = st_intersection(stores_sf, subway_buffers)
    
    # Create the leaflet map
    leaflet() |> 
      addProviderTiles("CartoDB.Positron") |> 
      
      # Add 400m buffer zones around subway stations (yellow)
      addPolygons(data = subway_buffers, fillColor = "yellow", fillOpacity = 0.2, color = "grey", weight = 0.4) |> 
      # Add legend
      addLegend(position = "bottomright", 
                colors = c("green", "red", "yellow"),
                labels = c("Food Sources","Subway Stations", "400-Meter Walking Radius"),
                title = NULL) |> 
      
      
      # Add subway stations (blue)
      addCircleMarkers(data = data$subways, color = "red", radius = 3, popup = ~paste(
                         "<strong>Subway Station:</strong> ", stop_name, "<br>",
                         "<strong>Borough:</strong> ", borough, "<br>"),
                       label = ~paste(
                         "Subway Station: ", stop_name)
) |> 
      # Add stores within 400m radius (green) with hover and click popups
      addCircleMarkers(data = stores_within_buffer, color = "green", radius = 0.5,
                       popup = ~paste(
                         "<strong>Store Name:</strong> ", store_name, "<br>",
                         "<strong>Borough:</strong> ", borough, "<br>",
                         "<strong>Address:</strong> ", store_address, "<br>",
                         "<strong>Type:</strong> ", type
                       ),
                       label = ~paste(
                         "Store Name: ", store_name))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
```