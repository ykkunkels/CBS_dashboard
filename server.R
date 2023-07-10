
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.1     ###
### YKK - 06-06-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

# Define server ----
server <- function(input, output, session) {
  
  ## Define SQL connection ----
  connection_iSR <- odbcDriverConnect("driver={SQL SERVER};server=SQL_HSR_ANA_PRD\\i01,50001;database=HSR_ANA_PRD;trusted_connection=true")
  
  ## Get CBS logo from external web-host ----
  output$logo <- renderUI({
    src <- "https://www.cbs.nl/Content/images/cbs-ld-logo.png"
    div(id = "myImage",
        tags$img(src = src, width = "65%", height = "auto"))
  })
  
  ## Settings: Role Selection ----
  output$role_txt <- renderText({
    paste(input$selected_role, collapse = ", ")
  })
  
  observe({
    toggle(id = "role_button", condition = input$selected_role)
  })
  
  
  ## Data Preparation: DT ----
  # Prepare the large datasets outside the reactive environment
  data1 <- reactive({
    if (input$selected_role == "Integrator") {
      # Load and return Integrator data
      return(iris)
    }
  })
  
  data2 <- reactive({
    if (input$selected_role == "DNB expert") {
      # Load and return DNB expert data
      return(mtcars)
    }
  })
  
  data3 <- reactive({
    if (input$selected_role == "R expert") {
      # Load and return R expert data
      return(ChickWeight)
    }
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
    
    if(length(data) > 0) {
      # Combine and display the selected datasets in a single DataTable
      datatable(
        do.call(rbind, data),
        options = list(scrollX = TRUE)  # Enable horizontal scrolling if needed
      )
    }
  })
  
  
  ## Settings: Load data button ----
  observeEvent(input$role_button, { # Jump to Data tab when role is selected
    updateTabItems(session, "sidebarmenu", "data_tab")
  })
  
  ## Observe: Excel button click---- 
  observeEvent(input$excel_button, {
    shell.exec("F:/Desktop/Dashboard_EXAMPLE/Example_Dashboard_6.6_2022V_230531.xlsb")
  })
  
} # closing server{}