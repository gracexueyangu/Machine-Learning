

ui = fluidPage(
  titlePanel("Diamonds Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("This App visualize the Diamonds data"),
      textInput(inputId = "title",
                label = "Enter your title:"),
      selectInput(inputId = "pos",
                  label = "Position",
                  choices = c("Stack" = "stack",
                              "Dodge" = "dodge")),    #small letter
      selectInput(inputId = "x_var", 
                  label = "Choose X Variable",
                  choices = colnames(diamonds),
                  selected = colnames(diamonds)[[2]]),
      selectInput(inputId = "fill_var", 
                  label = "Choose Fill Variable",
                  choices = colnames(diamonds))
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)






server  = function(input, output){
  
  output$plot = renderPlot({
    ggplot(diamonds,aes_string(x = input$x_var, 
                               fill = input$fill_var)) +
      geom_bar(position = input$pos) +
      ggtitle(input$title)
  })
  
  
  
}

#x = input$x_var: treated as text, not a variable
#x = get(input$x_var), y = get(input$fill_var): correct
#[[]]: return the nth of lists


shinyApp(ui, server)