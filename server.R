
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.2     ###
### YKK - 12-06-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

# Define server ----
server <- function(input, output, session) {
  
  ## Define & initialise reactiveValues objects ----
  SQL_input <- reactiveValues(connection_iSR = NA, jaar_transacties = NA, rekening_EB = NA, query = NA)
  SQL_output <- reactiveValues(data = NA)
  
  ## Define SQL connection ----
  SQL_input$connection_iSR <- odbcDriverConnect("driver={SQL SERVER};server=SQL_HSR_ANA_PRD\\i01,50001;database=HSR_ANA_PRD;trusted_connection=true")
  
  ## Get CBS logo from external web-host ----
  output$logo <- renderUI({
    src <- "https://www.cbs.nl/Content/images/cbs-ld-logo.png"
    div(id = "myImage", tags$img(src = src, width = "65%", height = "auto"))
  })
  
  ## Settings: Role Selection ----
  output$role_txt <- renderText({
    paste("You are logged in as:", input$selected_role, collapse = ", ")
  })
  
  # Only show load data button when role check box is ticked
  observe({
    toggle(id = "load_data_button", condition = input$selected_role)
  })
  
  # Jump to Data tab when load data button is clicked
  observeEvent(input$load_data_button, {
    updateTabItems(session, "sidebarmenu", "data_tab")
  })
  
  ## Observe: Excel button click---- 
  observeEvent(input$excel_button, {
    shell.exec("F:/Desktop/Dashboard_EXAMPLE/Example_Dashboard_6.6_2022V_230531.xlsb")
  })
  
  ## Data Module: Preparation & Loading ----
  # Load and return the large data sets outside the reactive environment, for each role
  data1 <- reactive({
    if (input$selected_role == "Integrator") {
      return(iris)
    }
  })
  
  data2 <- reactive({
    if (input$selected_role == "DNB expert") {
      return(mtcars)
    }
  })
  
  data3 <- reactive({
    if (input$selected_role == "R expert") {
      SQL_input$jaar_transacties <- "2021"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      
      SQL_output$data <- sqlQuery(SQL_input$connection_iSR, SQL_input$query)
      
      # return(SQL_output$data)
    }
  })
  
  # Combine into a single reactive expression
  combinedData <- reactive({
    output_data <- list(data1(), data2(), data3())
  })
  
  # Combine and render the selected data in a single datatable
  output$selectedData <- renderDT({
    if (!is.null(input$selected_role)) {
      output_data <- combinedData()
      datatable(do.call(rbind, output_data), options = list(scrollX = TRUE)) # horizontal scrolling is TRUE
    } 
  })
  
} # closing server{}