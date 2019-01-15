#load libraries
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

# Dividing the Percentage of Urban and Rural Pop so we get a proportion
df$UrbanPop <- df$UrbanPop/100
df$RuralPop <- df$RuralPop/100

# df2 is the aggregated form of df
df2 <- aggregate(. ~ YEAR + COUNTRY, data = df, mean)

# Selecting only interesting variables from the aggregated dataframe
df2 <- df2[,-c(3 ,5:8, 24)]

# Subtractin 1 from VECTOR aggregated so it is a proportion of type of vectors in the
# occurrences: 0 = Aedis aegypti || 0.5 = Half each || 1 = Aedes albopictus 
df2$VECTOR <- df2$VECTOR-1

# Reassigning the levels of Continents in region and sub.region variables
df2$region = as.factor(df2$region)
levels(df2$region) <- levels(df$region)

df2$sub.region = as.factor(df2$sub.region)
levels(df2$sub.region) <- levels(df$sub.region)

# Getting list of variables names of df2
nms <- names(df2)









##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
ui <- fluidPage(

  
  titlePanel("Aedes Aegypti and Aedes Albopictus Occurrences Around the World"),
  
  tabsetPanel(
    
    ## MAP TAB
    tabPanel("Vector Occurrence Location Map", 
          
             leafletOutput("mymap", height = 450),
             
             HTML("<b><big>Geographical Location of Registered Cases of
                        Occurrences of Vectors in 1991-2014</b></big>"),
             
             fluidRow(
               shiny::column(4, offset = 4,
                             sliderInput("year", "Year",
                                         min = min(df$YEAR), max = max(df$YEAR),
                                         value = min(df$YEAR), step = 1, animate = TRUE,
                                         animationOptions(interval = 1000))
                             
               )
             ),
             absolutePanel(bottom = 24, left = 30, 
                           checkboxInput("mark", "Cumulative Cases", FALSE)
             )
             ),
    
    ## INTERACTIVE SCATTERPLOT TAB
    tabPanel("Scatterplot of Attributes Dependencies",
             sidebarPanel(
               selectInput('x', 'X', choices = nms[-c(1, 2, 3, 19, 20)], selected = "HDI"),
               selectInput('y', 'Y', choices = nms[-c(1, 2, 3, 19, 20)], selected = "Occurrences"),
               selectInput('color', 'Color', choices = nms[c(1, 2, 19, 20)], selected = "region"),
               sliderInput("year2", "Year",
                           min = min(df2$YEAR), max = max(df2$YEAR),
                           value = c(min(df2$YEAR), min(df2$YEAR)+3), step = 1)
             ),
             mainPanel(
               plotlyOutput('trendPlot', height = "900px")
             )
    ),
    ## BARPLOT TAB
    tabPanel("Bar Plot of Occurrences Grouped",
             sidebarPanel(
               selectInput('x2', 'Group', choices = c("region", "VECTOR")
                           , selected = "region"),
               selectInput('y2', 'Average Metric',
                           choices = c("HDI", "Education", "Precipitation",
                                        "Temperature", "UrbanPop", "PopTotal")
                           , selected = "HDI"),
               sliderInput("year3", "Year",
                           min = min(df$YEAR), max = max(df$YEAR),
                           value = min(df$YEAR), step = 1, animate = TRUE,
                           animationOptions(interval = 1000))
             ),
             mainPanel(
               plotlyOutput('histPlot', height = "900px")
             )
             ),
    ## GOOGLE BUBBLE CHARTS TAB
    
    tabPanel("Google Bubble Chart",
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
             absolutePanel(right = 40, bottom = 10, width = 145,
                           selectInput('x4', 'X', 
                                       choices = c("HDI", "Education", "Precipitation",
                                                   "Temperature", "UrbanPop", "PopDens",
                                                   "PopTotal")
                                       , selected = "Temperature"),
                           selectInput('y4', 'Y', 
                                       choices = c("HDI", "Education", "Precipitation",
                                                   "Temperature", "UrbanPop", "PopDens",
                                                   "PopTotal")
                                       , selected = "Precipitation")
             )
      )
             
    )
  )



##############################################################################################
##############################################################################################
##############################################################################################
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
    
    
    
    leaflet(data = dataInput()) %>% 
      setView(lng = -15, lat = 20, zoom = 2.5)  %>%
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
                                 , "<br><b>Region: </b>"
                                 , region
                                 , "<br><b>Sub-Region: </b>"
                                 , sub.region
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
                                 , round(UrbanPop*100, 2), "%"
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
    return(df2[((df2$YEAR>=input$year2[1])&(df2$YEAR<=input$year2[2])),])
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()
    
    
    ggplotly(p, height = 550) %>% 
      layout(autosize=TRUE)
    
  })
 
  
  #### BARPLOT PLOT
  dataset2 <- reactive({
    return(df[df$YEAR<=input$year3,])
  })
  
  output$histPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    var1 = input$x2
    var2 = input$y2
    table <- data.frame(Group = aggregate(dataset2()[,var2] ~ dataset2()[,var1],
                                          data=dataset2(), mean)[,1],
                        Occurrences = as.numeric(ftable(dataset2()[,var1])[1,]),
                        Avg_Metric = aggregate(dataset2()[,var2] ~ dataset2()[,var1],
                                               data=dataset2(), mean)[,2])
    
    
    p2 <- ggplot(data=table, aes(x=Group, y=Occurrences, fill=Avg_Metric)) +
          geom_bar(stat="identity")
    
    ggplotly(p2, height = 550) %>% 
      layout(autosize=TRUE)
    
  })
  
  #### GOOGLE BUBBLE CHARTS
  variab <- reactive({
    xs <- input$x4
    ys <- input$y4
    if(input$x4=="UrbanPop"){xs <- "Proportion of Urban Population"}
    if(input$x4=="PopDens"){xs <- "Population Density"}
    if(input$x4=="PopTotal"){xs <- "Population Size"}
    if(input$x4=="Education"){xs <- "Education Index"}
    if(input$y4=="UrbanPop"){ys <- "Proportion of Urban Population"}
    if(input$y4=="PopDens"){ys <- "Population Density"}
    if(input$y4=="PopTotal"){ys <- "Population Size"}
    if(input$y4=="Education"){ys <- "Education Index"}
    return(c(xs, ys))
  })
  
  
  
  
  ser <- reactive({defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(yearData()[,"region"])
  )
  return(series)
  })
  
  
  yearData <- reactive({
    return(df2[df2$YEAR==input$year4, c("COUNTRY", input$x4, input$y4,
                                        "region", "Occurrences")])
  })
  
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "%s vs. %s, %s\n Bubble Sizes by Occurrences and Color by Region",
          variab()[1], variab()[2], input$year4),
        series = ser(),
        # Set axis labels and ranges
        hAxis = list(
          title = variab()[1],
          minValue = min(yearData()[,input$x4])-(0.75*sd(yearData()[,input$x4])),
          maxValue = max(yearData()[,input$x4])+(0.75*sd(yearData()[,input$x4]))
        ),
        vAxis = list(
          title = variab()[2],
          minValue = min(yearData()[,input$y4])-(0.75*sd(yearData()[,input$y4])),
          maxValue = max(yearData()[,input$y4])+(0.75*sd(yearData()[,input$y4]))
        )
      )
    )
  })
  
}

##############################################################################################
  
# Run the application 
shinyApp(ui = ui, server = server)

