library(shiny)
library(xgboost)
# install.packages('rsconnect')

# Load the trained model
load("PredictionModel.Rdata")
load("PredictionModelClass.Rdata")

# Define the user interface
ui <- fluidPage(
  titlePanel("Bike Sharing Demand Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for the model features
      numericInput("temp", "Temperature:", value = 0),
      numericInput("atemp", "ATemperature:", value = 0),
      numericInput("hum", "Humidity:", value = 0),
      numericInput("windspeed", "Wind Speed:", value = 0),
      numericInput("season", "Season:", value = 0),
      numericInput("yr", "Year:", value = 0),
      numericInput("mnth", "Month:", value = 0),
      numericInput("hr", "Hour:", value = 0),
      numericInput("holiday", "Holiday:", value = 0),
      numericInput("weekday", "Weekday:", value = 0),
      numericInput("workingday", "Working Day:", value = 0),
      numericInput("weathersit", "Weather Condition:",value = 0),
      actionButton("predictBtn", "Predict")
    ),
    mainPanel(
      h4("Predicted Demand Total Count:"),
      # Output to display the predicted bike demand
      verbatimTextOutput("prediction1"),
      verbatimTextOutput("prediction2")
    )
  )
)

# Define the server function
server <- function(input, output) {
  
  # Function to make predictions
  makePredictions <- function(input_data) {
    # print(input_data)
    data_predict <- xgb.DMatrix(data = as.matrix(input_data))
    # Load the random forest model
    load("PredictionModel.Rdata")
    # Use the random forest model for prediction
    prediction1 <- predict(xgb_model, newdata = data_predict)
    
    # Return the predicted bike demand
    return (round(prediction1))
  }
  
  # Function to make predictions
  makePredictionsClass <- function(input_data) {
    load("PredictionModelClass.Rdata")
    prediction2 <- predict(rf_class_model, newdata = input_data)
    
    # Return the predicted bike demand
    return (prediction2)
  }

  
  # Event handler for the predict button
  observeEvent(input$predictBtn, {
    # Create a data frame with user inputs
    input_data <- data.frame(
      season=input$season,
      yr=input$yr,
      mnth=input$mnth,
      hr=input$hr,
      holiday=input$holiday,
      weekday=input$weekday,
      workingday=input$workingday,
      weathersit=input$weathersit,
      temp=input$temp,
      atemp=input$atemp,
      hum=input$hum,
      windspeed = input$windspeed
    )
    output$prediction1 <- renderText({
      makePredictions(input_data)
    })
    output$prediction2 <- renderText({
      makePredictionsClass(input_data)
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
