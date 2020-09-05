library(leaflet)
library(shiny)
library(ggplot2)
library(htmltools)
library(plotly)
library(dplyr)

#data frame
dataset1 = read.csv("Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")
dataset2 = read.csv("Pedestrian_Counting_System_2019 (Exercise 2).csv")

merged_dataset= merge(x = dataset1, y = dataset2, by.x = "sensor_name", by.y="Sensor_Name")

merged_dataset
df <- data.frame(merged_dataset$sensor_name, merged_dataset$latitude, 
                 merged_dataset$longitude, merged_dataset$Hourly_Counts,
                 merged_dataset$Time,merged_dataset$Day)

ui <- pageWithSidebar(
                  headerPanel("Select Sensor"),
                  sidebarPanel(
                    selectInput("select", "Select a sensor", 
                                choices = setNames(as.character(unique(df$merged_dataset.sensor_name)),nm =(unique(df$merged_dataset.sensor_name))  )
                    )),
                 
                  mainPanel(
                    leafletOutput("map", width="900",height = "400px"),
                    plotlyOutput("plot",width="900",height = "400px")
                  )
                )

server <- function(input, output) {
  
  ## leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = df, ~unique(merged_dataset.longitude), 
                       ~unique(merged_dataset.latitude), layerId = ~unique(merged_dataset.sensor_name),
                       label = ~htmlEscape(unique(merged_dataset$sensor_name)),
                        radius = ~(log2(merged_dataset.Hourly_Counts)),
                       stroke = FALSE, fillOpacity = 0.5)
  })
  
  output$plot <- renderPlotly({
    x= df[df$merged_dataset.sensor_name == input$select,]
    group <- x %>% group_by(merged_dataset.Day,merged_dataset.Time) %>% summarise(avg_count = mean(merged_dataset.Hourly_Counts))
    plot_ly(group, x =group$merged_dataset.Time, y = group$avg_count, type = 'scatter', mode = 'scatter') %>%
      layout(
        xaxis = list(
          dtick = 1, 
          tick0 = 0, 
          tickmode = "linear"
        )) 
     
  })
}

shinyApp(ui, server)

