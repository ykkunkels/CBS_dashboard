
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.10    ###
### YKK - 03-07-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

server <- function(input, output, session) {
  
  ## 0. Basic Operations ----
  # Define & initialise reactiveValues objects
  SQL_input <- reactiveValues(connection_iSR = NA, query = NA)
  SQL_output <- reactiveValues(data = NA)
  
  # Define SQL connection
  SQL_input$connection_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
  
  # Get CBS logo from external web-host
  output$logo <- renderUI({
    src <- "https://www.cbs.nl/Content/images/cbs-ld-logo.png"
    div(id = "myImage", tags$img(src = src, width = "65%", height = "auto"))
  })
  
  ## 1. Initialise: Data ----
  
  # Query Sector_R data
  data_Sector_R <- reactive({
    
    SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type, sum(Waarde) AS Waarde 
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",input$select_jaar,"') AND Sector = ('",input$select_sector,"')
                                 AND Transactiesoort = ('",input$select_transactiesoort,"') AND Waarde_Type = ('",input$select_waarde_type,"')
                                 GROUP BY Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type")
    
    SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
  })
  
  # Render Sector R data
  output$data_Sector_R <- renderDT({
    
    data_Sector_R()
    datatable(SQL_output$data, 
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))) # horizontal scrolling is TRUE
  })
  
  # Render JPS data
  output$JPSData <- renderDT({
    
    # vw_SR_VolgordeJPS <- dbGetQuery(dbConnect(odbc::odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD"),
    #                                 "SELECT * FROM vw_SR_VolgordeJPS")
    
    tbl_SR_JPSReferentie <- dbGetQuery(dbConnect(odbc::odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD"),
                                       "SELECT * FROM tbl_SR_JPSReferentie")
    
    datatable(tbl_SR_JPSReferentie[2, 1:6])
    
    # datatable(vw_SR_VolgordeJPS,
    #   options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))
    # )
    
    # datatable(
    #   matrix(data = c(1, 1, 1, 1, 1, 1,
    #                   2, 2, 2, 2, 2, 2,
    #                   3, 3, 3, 3, 3, 3,
    #                   1, 1, 1, 1, 1, 1,
    #                   1, 1, 1, 1, 1, 1,
    #                   1, 1, 1, 1, 1, 1), 6, 6),
    #   options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))
    # )
  })
  
  ## 2. Plotting ----
  # Populate dropdown menu's with column names
  observe({
    updateSelectInput(session = session, inputId = "plot1_x", choices = colnames(SQL_output$data))
  })
  observe({
    updateSelectInput(session = session, inputId = "plot1_y", choices = colnames(SQL_output$data))
  })
  
  # Render plot
  output$plot1 <- renderPlot({
    
    plot(x = SQL_output$data[, input$plot1_x], y = SQL_output$data[, input$plot1_y])
  })
  
} # closing server{}
