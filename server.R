
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.11    ###
### YKK - 06-07-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

server <- function(input, output, session) {
  
  ## 0. Basic Operations ----
  # Define & initialise reactiveValues objects
  SQL_input <- reactiveValues(connection_iSR = NA, query = NA, query_JPS = NA)
  SQL_output <- reactiveValues(data = NA, data_JPS = NA)
  
  # Define SQL connection
  SQL_input$connection_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
  
  # Get CBS logo from external web-host
  output$logo <- renderUI({
    src_logo <- "https://www.cbs.nl/Content/images/cbs-ld-logo.png"
    div(id = "logo", tags$img(src = src_logo, width = "65%", height = "auto"))
  })
  
  # Get welcome image from external web-host
  output$img_welkom <- renderUI({
    src_welkom <- "https://cdn.cbs.nl/images/66453061544b43514969564556704a4f4d486a386e413d3d/720x480.jpg"
    div(id = "img_welkom", tags$img(src = src_welkom, width = "60%", height = "auto"))
  })
  
  
  ## 1. Initialise: Data ----
  
  # Query Sector_R data
  data_Sector_R <- reactive({
    
    SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type, sum(Waarde) AS Waarde 
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar BETWEEN ('",(as.integer(input$select_JPS_jaar) - 3),"') AND ('",input$select_JPS_jaar,"') 
                                 AND Sector = ('",input$select_sector,"')
                                 AND Transactiesoort = ('",input$select_transactiesoort,"') AND Waarde_Type = ('",input$select_waarde_type,"')
                                 GROUP BY Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type")
    
    SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
  })
  
  # Render Sector_R data
  output$data_Sector_R <- renderDT({
    
    data_Sector_R()
    datatable(SQL_output$data, 
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))) # horizontal scrolling is TRUE
  })
  
  # Query JPS data
  data_JPS <- reactive({
    
    SQL_input$query_JPS <- paste0("SELECT JPS_code, Smin1_code, Tmin1_code, Tmin1Ref_code, Qmin1_code
                                   FROM 	tbl_SR_JPSReferentie
                                   WHERE 	Jaar = ('",input$select_JPS_jaar,"') AND Status = ('",input$select_JPS_status,"')")
    
    SQL_output$data_JPS <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query_JPS)  
    
  })
  
  # Render JPS data
  output$data_JPS <- renderDT({
    
    data_JPS()
    # Hard-code JPS data matrix
    data_JPS_setup <- data.frame(
      matrix(data = c("JPS", "Referentie", "Tmin1", "Tmin1 Referentie", "Qmin1",
                      SQL_output$data_JPS$JPS_code[2], SQL_output$data_JPS$Smin1_code[2], SQL_output$data_JPS$Tmin1_code[2], SQL_output$data_JPS$Tmin1Ref_code[2], SQL_output$data_JPS$Qmin1_code[2],
                      SQL_output$data_JPS$JPS_code[3], SQL_output$data_JPS$Smin1_code[3], SQL_output$data_JPS$Tmin1_code[3], SQL_output$data_JPS$Tmin1Ref_code[3], SQL_output$data_JPS$Qmin1_code[3],
                      SQL_output$data_JPS$JPS_code[4], SQL_output$data_JPS$Smin1_code[4], SQL_output$data_JPS$Tmin1_code[4], SQL_output$data_JPS$Tmin1Ref_code[4], SQL_output$data_JPS$Qmin1_code[4],
                      SQL_output$data_JPS$JPS_code[5], SQL_output$data_JPS$Smin1_code[5], SQL_output$data_JPS$Tmin1_code[5], SQL_output$data_JPS$Tmin1Ref_code[5], SQL_output$data_JPS$Qmin1_code[5],
                      SQL_output$data_JPS$JPS_code[1], SQL_output$data_JPS$Smin1_code[1], SQL_output$data_JPS$Tmin1_code[1], SQL_output$data_JPS$Tmin1Ref_code[1], SQL_output$data_JPS$Qmin1_code[1]
      ), nrow = 5, ncol = 6
      )
    )
    
    datatable(data_JPS_setup, 
              colnames = c("Periode", "1", "2", "3", "4", "5"), rownames = F,
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))) 
    
  })
  
  ## 2. Plotting ----
  # Populate drop-down menu's with column names
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
