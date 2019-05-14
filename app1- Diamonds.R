library(shiny)
library(ggplot2)
library(dplyr)


ui = fluidPage(
  titlePanel("I am a title",
             windowTitle = "windo title"),
  sidebarLayout(
    sidebarPanel(
      helpText("This is to visualize diamonds data"),
      textInput(inputId = "title", label = "Enter a title:", 
                value = ""),
      selectInput(inputId = "pos", label = "Position", 
                  choices = c("Stack"="stack", 
                                 "Dodge"="dodge")),
      selectInput(inputId = "x_var", label = "X Variable", 
                  choices =  c(colnames(diamonds))),
      selectInput(inputId = "fill_var", label = "Fill Variable", 
                  choices =  c(colnames(diamonds)))
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)


server = function(input, output){
  
  output$plot = renderPlot({
    ggplot(diamonds, aes_string(x = input$x_var, 
                                fill = input$fill_var)) + 
      geom_bar(position = input$pos) + 
      ggtitle(input$title)
  })
}

shinyApp(ui, server)