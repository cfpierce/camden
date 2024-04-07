#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)
library(sf)
library(leaflet)

camden_dashboard = readRDS("/Users/libbydoyle/Library/CloudStorage/Box-Box/camden/data/camden_clean.rds")

# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel("Police Traffic Stops"),
  sidebarLayout(
    sidebarPanel = checkboxGroupInput('outcome',label = h3("Pick Outcome"),
                                      choices = list( "warning" = "warning",
                                                      "citation" = "citation",
                                                      "summons" = "summons",
                                                      "arrest" = "arrest",
                                                      "no outcome" = "no outcome"),
                                      selected = "warning"), 

    mainPanel = mainPanel(leafletOutput(outputId = 'map'))
      ))







server = function(input, output){
  
  map_df = reactive({
    camden_dashboard |> 
       filter(camden_recode == input$outcome) |> 
      filter(!is.na(lng) & !is.na(lat))  |> 
     st_as_sf(coords = c('lng', 'lat')) |> 
     st_set_crs(4326)
  })


  
output$map = renderLeaflet({
  
  leaflet() |> 
    addTiles() |> 
    setView(lat = 39.9259, lng = -75.1196, zoom = 12)|> 
    addCircleMarkers(data = map_df(), 
                     radius = 0.5, 
                     fillOpacity = 0.5,
                     color = ifelse(map_df()$camden_recode == "warning", "yellow",
                                    ifelse(map_df()$camden_recode == "citation", "orange",
                                           ifelse(map_df()$camden_recode == "summons", "red",
                                                  ifelse(map_df()$camden_recode == "arrest", "blue", "gray")))))
  

})

}


# Run the application 
shinyApp(ui, server)

