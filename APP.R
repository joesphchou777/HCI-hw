library(shiny)
library(leaflet)
library(RColorBrewer)
airpoll <- read.csv("data/airpoll.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "AQI", min(airpoll$AQI), max(airpoll$AQI),
                            value = range(airpoll$AQI), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    airpoll[airpoll$AQI >= input$range[1] & airpoll$AQI <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, airpoll$AQI)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(airpoll) %>% addTiles() %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>% setView(121, 24, zoom = 7)%>%
      clearShapes() %>%
      addCircles(radius = ~AQI*100, weight = 1, color = "#777777",
                 fillColor = ~pal(AQI), fillOpacity = 0.7, popup = ~paste(AQI,airpoll$Station))
                   
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = airpoll)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~AQI
      )
    }
  })
}

shinyApp(ui, server)