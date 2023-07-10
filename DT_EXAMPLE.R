library(shiny)
library(DT) # For displaying large datasets efficiently

# Define the UI
ui <- fluidPage(
  titlePanel("Data Selection App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("dataset1", "Dataset 1"),
      checkboxInput("dataset2", "Dataset 2"),
      checkboxInput("dataset3", "Dataset 3")
    ),
    
    mainPanel(
      DTOutput("selectedData")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Prepare the large datasets outside the reactive environment
  data1 <- reactive({
    if (input$dataset1) {
      # Load and return dataset 1
      return(iris)
    }
    # Return an empty data structure when the checkbox is not selected
    data.frame()
  })
  
  data2 <- reactive({
    if (input$dataset2) {
      # Load and return dataset 2
      return(mtcars)
    }
    # Return an empty data structure when the checkbox is not selected
    data.frame()
  })
  
  data3 <- reactive({
    if (input$dataset3) {
      # Load and return dataset 3
      return(ChickWeight)
    }
    # Return an empty data structure when the checkbox is not selected
    data.frame()
  })
  
  # Combine the selected datasets into a single reactive expression
  combinedData <- reactive({
    data <- list(data1(), data2(), data3())
    data <- Filter(function(x) !is.data.frame(x) || nrow(x) > 0, data)  # Remove empty data frames from the list
    data
  })
  
  # Render the selected data using the DT::renderDataTable function
  output$selectedData <- renderDT({
    data <- combinedData()
    
    if (length(data) > 0) {
      # Combine and display the selected datasets in a single DataTable
      datatable(
        do.call(rbind, data),
        options = list(scrollX = TRUE)  # Enable horizontal scrolling if needed
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

