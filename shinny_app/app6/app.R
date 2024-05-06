library(shiny)
library(leaflet)
library(dplyr)
library(tidycensus)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
census_api_key("ce09a27df6fcf1fb2ed58ad56259ca761895811a", install = TRUE, overwrite = TRUE)

states_geo <- states(cb = TRUE, year = 2019, class = "sf") %>%
  st_transform(crs = 4326) %>%
  rename(state = NAME) %>%
  select(state, STATEFP) %>%
  distinct(STATEFP, .keep_all = TRUE) %>%
  arrange(state)  

places_geo <- places(cb = TRUE, year = 2019, class = "sf") %>%
  st_transform(crs = 4326) %>%
  st_centroid() %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2])

city_rent_data <- get_acs(geography = "place", 
                          variables = "B25064_001", 
                          survey = "acs5", 
                          year = 2019) %>%
  rename(city = NAME, median_rent = estimate)

city_rent_data_clean <- city_rent_data %>%
  left_join(places_geo, by = "GEOID") %>%
  left_join(states_geo, by = "STATEFP") %>%
  select(city, median_rent, longitude, latitude, state)

ui <- fluidPage(
  titlePanel("US City Median Rent Map"),
  p("This interactive map is designed to provide quick access to housing cost information to help you compare different areas within the selected state."),
  p("Explore median rent data across US cities. First, please select the state you live in or are interested in from the dropdown menu. Then, click on any city on the map to view the median rent."),
  selectInput("stateSelect", "Select a State:", choices = states_geo$state),
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.35, lat = 39.5, zoom = 4)
  })
  
  observe({
    state_data <- filter(city_rent_data_clean, state == input$stateSelect)
    
    if (nrow(state_data) > 0) {
      # Calculate the bounding box for the selected state's cities
      lng_range <- range(state_data$longitude, na.rm = TRUE)
      lat_range <- range(state_data$latitude, na.rm = TRUE)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(data = state_data, 
                         lng = ~longitude, 
                         lat = ~latitude, 
                         radius = 2,  
                         color = "#996699",  
                         fillOpacity = 0.1,  
                         popup = ~paste("City: ", city, "<br>", "Median Rent: $", median_rent)) %>%
        fitBounds(lng1 = lng_range[1], lat1 = lat_range[1], lng2 = lng_range[2], lat2 = lat_range[2])
    } else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = -98.35, lat = 39.5, zoom = 4)  # Default view if no data is available
    }
  })
}

shinyApp(ui, server)




