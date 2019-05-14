library(shiny)
library(tidyverse)

ui = fluidPage(
  titlePanel(title = "USA Census Visualization", 
             windowTitle = "US Census Vis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographics maps with information from the 2010 Census."),
      selectInput(inputId = "race", 
                  label = "Choose a race to display", 
                  choices = c("Percent White" = "white", 
                              "Percent Black" = "black", 
                              "Percent Latino" = "hispanic", 
                              "Percent Asian" = "asian"))
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server = function(input, output){
  
  output$plot = renderPlot({
    
  map = readRDS("counties.rds")
  
  counties_map = map_data("county")
  
  counties_map$name = paste(counties_map$region,
                            counties_map$subregion, 
                            sep = ",")
  
  
  ## join the two tables 
  
  all_data = left_join(counties_map, 
                       data, 
                       by = "name")
  
  ggplot(all_data, aes_string(x = "long", 
                       y = "lat",
                       group = "group", 
                       fill = input$race)) + 
    geom_polygon() + 
    scale_fill_gradient(low = "white", 
                        high = "darkred") + 
    theme_void()
  
  })
  
}

shinyApp(ui, server)