# load libraries
library(shiny)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Data Science Capstone Project by Johns Hopkins University"),
    h4("Given a phrase (multiple words) this application will attempt to predict the next word"),
    br(),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with two inputs
      sidebarPanel(
        helpText("Type a phrase into the text box and click the submit button to see the next word prediction"),

        hr(),

        textInput("text", "Input phrase:", "Once upon a"),

        submitButton("Submit")
      ),

      # Create a spot for the histogram 
      mainPanel(
        h5("Prediction:"),
        verbatimTextOutput("text")
      )
      
    )
  )
)