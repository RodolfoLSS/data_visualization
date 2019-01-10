library(leaflet)
library(shiny)
library(htmltools)
library(highcharter)
library(ggplot2)

df <- read.csv("dataset.csv",
               header = TRUE, sep= ",")

ui <- fluidPage(
  titlePanel("Country explorer"),
  tabsetPanel(
    # Map
    tabPanel("Interactive map", leafletOutput("firstExample", height = 800)),
    # Data
    tabPanel("Data explorer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Entity", "Country: ", df$Entity)
               ),
               mainPanel(leafletOutput("secondExample", height = 600))
             )
    )
  )
)

server <- function(input, output){
  
  tmp.df <- df[sample(nrow(df), 1000), ]
  output$firstExample <- renderLeaflet({
 
    leaflet(tmp.df)%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Aerial imagery")%>%
      addTiles(group= 'OSM') %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo")%>%
      setView(
        lng = -9,
        lat = 38,
        zoom = 1.5
      ) %>%
      addLayersControl(
        baseGroups = c("Aerial Imagery", "OSM", "CartoDB", "NatGeo"),
        overlayGroups = c("Countries with Vectors"),
        options = layersControlOptions(collapsed = TRUE)
      )%>%
      addMarkers(~X, ~Y, popup = ~htmlEscape(Country), group = "Countries with Vectors")
    
  })
  
  
}

shinyApp(ui, server)