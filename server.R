# load libraries
library(shiny)

# load code
source("prepare_input.R")
source("prediction_model.R")

# Define a server for the Shiny app
shinyServer(function(input, output) {

  output$text <- renderText({
  
    # prepare the pharse by parsing the input
    user_input <- PrepareInput(input$text)
    PredictionModel(user_input)

  })
  
})