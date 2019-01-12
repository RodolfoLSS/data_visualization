library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(plotly)
library(htmltools)

# Install:
#   install.package("devtools")
#   devtools::install_github("jcheng5/googleCharts")
library(googleCharts)



#import data
df <- read.csv("merged_new.csv")

# Fixing some variables names
colnames(df)[12:13] <- c("UrbanPop", "RuralPop")

# Multiplying PopTotal by 1k, because its in thousands
df$PopTotal <- df$PopTotal * 1000

# df2 is the aggregated form of df
df2 <- aggregate(. ~ YEAR + COUNTRY, data = df, mean)

# Selecting only interesting variables from the aggregated dataframe
df2 <- df2[,-c(3:8, 24)]

# Reassigning the levels of Continents in region variable
df2$region = as.factor(df2$region)
levels(df2$region) <- c("Africa", "America", "Asia", "Europe", "Oceania")

# Getting list of variables names of df2
nms <- names(df2)






###### Axis limits for Google Charts
# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = 0.3,
  max = 1.1
)
ylim <- list(
  min = -200000000,
  max = 2000000000
)



##############################################################################################
ui <- fluidPage(
  
  
  titlePanel("Disease Vectors"),
  
  tabsetPanel(
    
    ## MAP TAB
    tabPanel("Map", leafletOutput("mymap", height = 450),
             fluidRow(
               shiny::column(4, offset = 4,
                             sliderInput("year", "Year",
                                         min = min(df$YEAR), max = max(df$YEAR),
                                         value = min(df$YEAR), step = 1, animate = TRUE,
                                         animationOptions(interval = 1000))
                             
               )
             ),
             absolutePanel(bottom = 30, left = 30, 
                           checkboxInput("mark", "Cumulative Cases", FALSE)
             )
    ),
    
    ## INTERACTIVE PLOTS TAB
    tabPanel("Interactive Plots",
             sidebarPanel(
               selectInput('x', 'X', choices = nms, selected = "HDI"),
               selectInput('y', 'Y', choices = nms, selected = "Occurrences"),
               selectInput('color', 'Color', choices = nms, selected = "region"),
               sliderInput("year2", "Year",
                           min = min(df2$YEAR), max = max(df2$YEAR),
                           value = min(df2$YEAR), step = 1, animate = TRUE,
                           animationOptions(interval = 1000)),
               checkboxInput("mark2", "Cumulative Cases", FALSE)
             ),
             mainPanel(
               plotlyOutput('trendPlot', height = "900px")
             )
    ),
    ## HISTOGRAM TAB
    tabPanel("Histogram",
             sidebarPanel(
               selectInput('x2', 'X', choices = nms, selected = "region"),
               selectInput('y2', 'Y', choices = nms, selected = "Occurrences"),
               sliderInput("year3", "Year",
                           min = min(df2$YEAR), max = max(df2$YEAR),
                           value = min(df2$YEAR), step = 1, animate = TRUE,
                           animationOptions(interval = 1000)),
               checkboxInput("mark3", "Cumulative Cases", FALSE)
             ),
             mainPanel(
               plotlyOutput('histPlot', height = "900px")
             )
    ),
    ## GOOGLE CHARTS TAB
    tabPanel("Google Charts",
             # This line loads the Google Charts JS library
             googleChartsInit(),
             
             # Use the Google webfont "Source Sans Pro"
             tags$link(
               href=paste0("http://fonts.googleapis.com/css?",
                           "family=Source+Sans+Pro:300,600,300italic"),
               rel="stylesheet", type="text/css"),
             tags$style(type="text/css",
                        "body {font-family: 'Source Sans Pro'}"
             ),
             
             
             googleBubbleChart("chart",
                               width="100%", height = "475px",
                               # Set the default options for this chart; they can be
                               # overridden in server.R on a per-update basis. See
                               # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                               # for option documentation.
                               options = list(
                                 fontName = "Source Sans Pro",
                                 fontSize = 13,
                                 # Set axis labels and ranges
                                 hAxis = list(
                                   title = "HDI",
                                   viewWindow = xlim
                                 ),
                                 vAxis = list(
                                   title = "Population Size",
                                   viewWindow = ylim
                                 ),
                                 # The default padding is a little too spaced out
                                 chartArea = list(
                                   top = 50, left = 75,
                                   height = "75%", width = "75%"
                                 ),
                                 # Allow pan/zoom
                                 explorer = list(),
                                 # Set bubble visual props
                                 bubble = list(
                                   opacity = 0.4, stroke = "none",
                                   # Hide bubble label
                                   textStyle = list(
                                     color = "none"
                                   )
                                 ),
                                 # Set fonts
                                 titleTextStyle = list(
                                   fontSize = 16
                                 ),
                                 tooltip = list(
                                   textStyle = list(
                                     fontSize = 12
                                   )
                                 )
                               )
             ),
             fluidRow(
               shiny::column(4, offset = 4,
                             sliderInput("year4", "Year",
                                         min = min(df2$YEAR), max = max(df2$YEAR),
                                         value = min(df2$YEAR),  step = 1, animate = TRUE,
                                         animationOptions(interval = 1000))
               )
             ),
             absolutePanel(bottom = 20, left = 30, 
                           checkboxInput("mark4", "Cumulative Cases", FALSE)
             )
    )
  )
)


##############################################################################################
server <- function(input, output, session){
  
  ####### MAP
  dataInput <- reactive({
    if(input$mark){return(df[df$YEAR<=input$year,])}
    else{return(df[df$YEAR==input$year,])}
    
  })
  
  
  pal <- colorFactor(
    palette = c('blue', 'red'),
    domain = df$VECTOR
  )
  
  
  #create the map
  output$mymap <- renderLeaflet({
    
    
    
    leaflet(dataInput()) %>% 
      setView(lng = -15, lat = 20, zoom = 2.5)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = dataInput(), lat = ~ Y, lng = ~ X, weight = 1, radius = 100,
                 label = ~(paste(COUNTRY, ", ", YEAR)),
                 popup = ~(paste("<b><big>Occurrence in", COUNTRY
                                 , "</b></big>"
                                 , "<br><b>Vector: </b>"
                                 , VECTOR
                                 , "<br><b>Year: </b>"
                                 , YEAR
                                 , "<br><br><b><big>", COUNTRY, " in ", YEAR, "</b></big>"
                                 , "<br><b>Country: </b>"
                                 , COUNTRY_ID
                                 , "<br><b>Population: </b>"
                                 , round(PopTotal/1000000, 2), "mil"
                                 , "<br><b>Population Density: </b>"
                                 , round(PopDens, 2), "per square km"
                                 , "<br><b>HDI: </b>"
                                 , HDI
                                 , "<br><b>Education Index: </b>"
                                 , Education
                                 , "<br><b>Percentage of Urban Pop: </b>"
                                 , round(UrbanPop, 2), "%"
                                 , "<br><b>Average Yearly Precipitation: </b>"
                                 , round(Precipitation, 2), "mm"
                                 , "<br><b>Average Yearly Temperature: </b>"
                                 , round(Temperature, 2), "\u00B0C"
                                 
                 )),
                 color = ~pal(VECTOR), fillOpacity = 0.5) %>%
      addLegend("bottomright", pal = pal, values = ~VECTOR,
                opacity = 0.7, group = "Vector Legend", title = "Vector:") %>%
      addLayersControl(overlayGroups = c("Vector Legend"))
    
    
  })
  
  
  ##### INTERACTIVE SCATTERPLOT
  dataset <- reactive({
    if(input$mark2){return(df2[df2$YEAR<=input$year2,])}
    else{return(df2[df2$YEAR==input$year2,])}
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()
    
    
    ggplotly(p, height = 550) %>% 
      layout(autosize=TRUE)
    
  })
  
  
  #### HISTOGRAM PLOT
  dataset2 <- reactive({
    if(input$mark3){return(df2[df2$YEAR<=input$year3,])}
    else{return(df2[df2$YEAR==input$year3,])}
  })
  
  output$histPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p2 <- ggplot(dataset2(), aes_string(x = input$x2, y = input$y2)) + 
      geom_bar(stat = "identity")
    
    
    ggplotly(p2, height = 550) %>% 
      layout(autosize=TRUE)
    
  })
  
  #### GOOGLE CHARTS
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(df2$region)
  )
  
  
  yearData <- reactive({
    if(input$mark4){return(df2[df2$YEAR<=input$year4, c(2, 8, 9, 18, 5)])}
    else{return(df2[df2$YEAR==input$year4, c(2, 8, 9, 18, 5)])}
  })
  
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "HDI vs. Population, %s\n Bubble Sizes by Occurrences and Color by Continent",
          input$year4),
        series = series
      )
    )
  })
  
}

##############################################################################################

# Run the application 
shinyApp(ui = ui, server = server)

