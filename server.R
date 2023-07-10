
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.6     ###
### YKK - 16-06-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

# Define server ----
server <- function(input, output, session) {
  
  ## Define & initialise reactiveValues objects ----
  SQL_input <- reactiveValues(connection_iSR = NA, jaar_transacties = NA, rekening_EB = NA, rekening_LT = NA, query = NA, colnames = NA)
  SQL_output <- reactiveValues(data = NA, colnames = NA)
  
  ## Define SQL connection ----
  SQL_input$connection_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
  
  ## Get CBS logo from external web-host ----
  output$logo <- renderUI({
    src <- "https://www.cbs.nl/Content/images/cbs-ld-logo.png"
    div(id = "myImage", tags$img(src = src, width = "65%", height = "auto"))
  })
  
  ## Observe: Excel button click---- 
  observeEvent(input$excel_button, {
    shell.exec("F:/Desktop/Dashboard_EXAMPLE/Example_Dashboard_6.6_2022V_230531.xlsb")
  })
  
  ## Settings: Role Selection ----
  output$role_txt <- renderText({
    paste("You are logged in as:", "<b>", input$selected_role, "</b>", collapse = ", ")
  })
  
  # Only show custom choices when custom role selected
  observeEvent(input$selected_role, {
    
    if (!(input$selected_role == "Custom")){
      hide(id = c("custom_years"))
      hide(id = c("custom_rekening_LT"))
      hide(id = c("custom_rekening_EB"))
    }
    
    if (input$selected_role == "Custom"){
      shinyjs::show(id = c("custom_years"))
      shinyjs::show(id = c("custom_rekening_LT"))
      shinyjs::show(id = c("custom_rekening_EB"))
    }
    
  })
  
  # Jump to Data tab when load data button is clicked
  observeEvent(input$load_data_button, {
    updateTabItems(session, "sidebarmenu", "data_tab")
  })
  
  ## Data Module: Preparation & Loading ----
  # Load and return the large data sets outside the reactive environment, for each role
  role_eindintegrator <- reactive({
    if (input$selected_role == "Eindintegrator") {
      return(mtcars)
    }
  })
  
  role_sectorspecialist <- reactive({
    if (input$selected_role == "Sectorspecialist") {
      SQL_input$jaar_transacties <- "2021"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  role_transactiespecialist <- reactive({
    if (input$selected_role == "Transactiespecialist") {
      SQL_input$jaar_transacties <- "2022"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  role_duale_classificatiespecialist <- reactive({
    if (input$selected_role == "Duale classificatiespecialist") {
      SQL_input$jaar_transacties <- "2023"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  role_sim_expert <- reactive({
    if (input$selected_role == "SIM-expert") {
      SQL_input$jaar_transacties <- "2021"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  role_cwc_lid_projectleider <- reactive({
    if (input$selected_role == "CWC-lid / Projectleider") {
      SQL_input$jaar_transacties <- "2022"
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 GROUP BY Jaar, Periode, Status")
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  role_r_expert <- reactive({
    if (input$selected_role == "R expert") {
      SQL_input$jaar_transacties <- "2021"
      SQL_input$rekening_LT <- "LT" 
      SQL_input$rekening_EB <- "EB"
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type, sum(Waarde) AS Waarde 
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 OR     Jaar >= ('",SQL_input$jaar_transacties,"') AND Rekening = ('",SQL_input$rekening_LT,"')
                                 GROUP BY Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type")
      # SQL_output$colnames <- colnames(SQL_output$data)
      # print(SQL_output$colnames)
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
      # browser()
    }
  })
  
  role_custom <- reactive({
    if (input$selected_role == "Custom") {
      SQL_input$jaar_transacties <- input$custom_years
      SQL_input$rekening_LT <- if(input$custom_rekening_LT){"LT"}
      SQL_input$rekening_EB <- if(input$custom_rekening_EB){"EB"}
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type, sum(Waarde) AS Waarde 
                                 FROM 	tbl_SR_Data_Transacties  
                                 WHERE 	Jaar >= ('",input$custom_years,"') AND Rekening = ('",SQL_input$rekening_EB,"')
                                 OR     Jaar >= ('",input$custom_years,"') AND Rekening = ('",SQL_input$rekening_LT ,"')
                                 GROUP BY Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type")     
      SQL_output$data <- dbGetQuery(SQL_input$connection_iSR, SQL_input$query)
    }
  })
  
  # Combine into a single reactive expression
  combinedData <- reactive({
    output_data <- list(role_eindintegrator(), role_sectorspecialist(), role_transactiespecialist(), role_duale_classificatiespecialist(),
                        role_sim_expert(), role_cwc_lid_projectleider(), role_r_expert(), role_custom())
  })
  
  # Combine and render the selected data in a single datatable
  # also get column names
  output$selectedData <- renderDT({
    
    SQL_output$colnames <- gsub(pattern = "_label", replacement = "", x = colnames(SQL_output$data))
    print(SQL_output$colnames)
    
    output_data <- combinedData()
    datatable(do.call(rbind, output_data), 
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100))) # horizontal scrolling is TRUE
  })
  
  ## Plotting ----
  # Populate dropdown menu's with column names
  observe({
    updateSelectInput(session = session, inputId = "plot1_x", choices = SQL_output$colnames)
  })
  observe({
    updateSelectInput(session = session, inputId = "plot1_y", choices = SQL_output$colnames)
  })
  
  
  output$plot1 <- renderPlot({
    
    plot(x = input$plot1_x, y = input$plot1_y)
    
  })
  
} # closing server{}