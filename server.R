
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.19    ###
### YKK - 28-08-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

server <- function(input, output, session) {
  
  ## 0. Basic Operations ----
  # Define & initialise reactiveValues objects
  SQL_input <- reactiveValues(connection = NA, query = NA, query_JPS = NA)
  SQL_output <- reactiveValues(data_JPS = NA, code_JPS = NA, data_Sector_R = NA, data_aggregated_Sector_R = NA)
  
  # Define SQL connection
  SQL_input$connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
  
  
  ## 1. Initialise: Data ----
  # Query JPS data
  data_JPS <- reactive({
    
    SQL_input$query_JPS <- paste0("SELECT JPS_code, Smin1_code, Tmin1_code, Tmin1Ref_code, Qmin1_code
                                   FROM 	tbl_SR_JPSReferentie
                                   WHERE 	Jaar = ('",input$select_JPS_jaar,"') AND Status = ('",input$select_JPS_status,"')")
    
    SQL_output$data_Sector_R_JPS <- dbGetQuery(SQL_input$connection, SQL_input$query_JPS)
    
    SQL_output$code_JPS <- SQL_output$data_Sector_R_JPS$JPS_code[1]
    
  })
  
  # Render JPS data
  output$data_JPS <- renderDT({
    
    data_JPS()
    # Hard-code JPS data matrix
    data_JPS_setup <- data.frame(
      matrix(data = c("JPS", "Referentie", "Tmin1", "Tmin1 Referentie", "Qmin1",
                      SQL_output$data_Sector_R_JPS$JPS_code[2], SQL_output$data_Sector_R_JPS$Smin1_code[2], SQL_output$data_Sector_R_JPS$Tmin1_code[2], SQL_output$data_Sector_R_JPS$Tmin1Ref_code[2], SQL_output$data_Sector_R_JPS$Qmin1_code[2],
                      SQL_output$data_Sector_R_JPS$JPS_code[3], SQL_output$data_Sector_R_JPS$Smin1_code[3], SQL_output$data_Sector_R_JPS$Tmin1_code[3], SQL_output$data_Sector_R_JPS$Tmin1Ref_code[3], SQL_output$data_Sector_R_JPS$Qmin1_code[3],
                      SQL_output$data_Sector_R_JPS$JPS_code[4], SQL_output$data_Sector_R_JPS$Smin1_code[4], SQL_output$data_Sector_R_JPS$Tmin1_code[4], SQL_output$data_Sector_R_JPS$Tmin1Ref_code[4], SQL_output$data_Sector_R_JPS$Qmin1_code[4],
                      SQL_output$data_Sector_R_JPS$JPS_code[5], SQL_output$data_Sector_R_JPS$Smin1_code[5], SQL_output$data_Sector_R_JPS$Tmin1_code[5], SQL_output$data_Sector_R_JPS$Tmin1Ref_code[5], SQL_output$data_Sector_R_JPS$Qmin1_code[5],
                      SQL_output$data_Sector_R_JPS$JPS_code[1], SQL_output$data_Sector_R_JPS$Smin1_code[1], SQL_output$data_Sector_R_JPS$Tmin1_code[1], SQL_output$data_Sector_R_JPS$Tmin1Ref_code[1], SQL_output$data_Sector_R_JPS$Qmin1_code[1]
      ), nrow = 5, ncol = 6
      )
    )
    
    datatable(data_JPS_setup, 
              colnames = c("Periode", "1", "2", "3", "4", "Y"), rownames = F,
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100)))
    
  })
  
  
  # Query Sector_R data
  data_Sector_R <- reactive({
    
    # Exception for showing data from all or specific TransactieSoorten
    if(input$select_transactiesoort == "all"){
      
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Sector, Transactie, TransactieSoort, Waarde_Type, Rekening, Waarde
                               FROM tbl_SR_Data_Transacties 
                               WHERE Jaar BETWEEN ('",(as.integer(substr(SQL_output$code_JPS, 1, 4)) - 1),"') AND ('",as.integer(substr(SQL_output$code_JPS, 1, 4)),"')
                               AND Waarde_Type='R' AND Sector=('",input$select_sector,"') AND Rekening=('",input$select_rekening,"')")
      
    }else{
      
      SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Sector, Transactie, TransactieSoort, Waarde_Type, Rekening, Waarde
                               FROM tbl_SR_Data_Transacties 
                               WHERE Jaar BETWEEN ('",(as.integer(substr(SQL_output$code_JPS, 1, 4)) - 1),"') AND ('",as.integer(substr(SQL_output$code_JPS, 1, 4)),"')
                               AND Waarde_Type='R' AND Sector=('",input$select_sector,"') AND Rekening=('",input$select_rekening,"') AND
                               TransactieSoort=('",input$select_transactiesoort,"')")
      
    }
    
    SQL_output$data_Sector_R <- dbGetQuery(SQL_input$connection, SQL_input$query)
    
  })
  
  
  # Render Sector_R data
  output$data_Sector_R <- renderDT({
    
    data_Sector_R()
    
    isolate({
      
      # Add JPS variable
      SQL_output$data_Sector_R$JPS <- paste0(SQL_output$data_Sector_R$Jaar, "-",
                                             SQL_output$data_Sector_R$Periode, "-",
                                             SQL_output$data_Sector_R$Status)
      
      # Aggregate data to sum over values
      SQL_output$data_aggregated_Sector_R <- aggregate(Waarde ~ JPS + Rekening + TransactieSoort + Transactie, SQL_output$data_Sector_R, sum)
      
      
      # Select only required JPSen
      required_JPS <- as.character(unlist(SQL_output$data_Sector_R_JPS[c(2:5, 1), c(2, 1, 3)]))
      SQL_output$data_aggregated_Sector_R <- SQL_output$data_aggregated_Sector_R[SQL_output$data_aggregated_Sector_R$JPS %in% required_JPS, ]
      
      
      # Reshape data from long to wide    
      SQL_output$data_reshaped_Sector_R <- reshape(SQL_output$data_aggregated_Sector_R, direction = 'wide', idvar = c("Transactie", "TransactieSoort"), timevar = "JPS")
      
      # Drop unnecessary columns
      drop <- grep(c("Rekening."), colnames(SQL_output$data_reshaped_Sector_R))
      SQL_output$data_reshaped_Sector_R <- SQL_output$data_reshaped_Sector_R[, -drop]
      
      # Rename colnames
      colnames(SQL_output$data_reshaped_Sector_R) <- gsub("Waarde.", "", colnames(SQL_output$data_reshaped_Sector_R))
      
      # Sort columns
      temp_nchar <- max(nchar(colnames(SQL_output$data_reshaped_Sector_R)[3:17]))
      SQL_output$data_reshaped_Sector_R <- SQL_output$data_reshaped_Sector_R[c(1, 2, order(substr(colnames(SQL_output$data_reshaped_Sector_R[3:17]), 1, 4),
                                                                                           substr(colnames(SQL_output$data_reshaped_Sector_R[3:17]), 8, temp_nchar)) + 2)]
      
      
      # Add decimal points (#! check if this doesn't introduce bugs)
      for(i in 3:(ncol(SQL_output$data_reshaped_Sector_R))){
        SQL_output$data_reshaped_Sector_R[, i] <- prettyNum(as.vector(SQL_output$data_reshaped_Sector_R[, i]), big.mark = ".", big.interval = 3L, decimal.mark = ",", scientific = FALSE)
      }
      
      SQL_output$data_final_Sector_R <- SQL_output$data_reshaped_Sector_R
      
    }) # closing isolate()
    
    # Take data as datatable
    datatable(SQL_output$data_final_Sector_R, rownames = NULL,
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 20, 30, 50, 100, 500), 
                             initComplete = JS( #change colnames fontsize
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '85%'});",
                               "$(this.api().table().header()).css({'color': 'steelblue'});",
                               "}")
              ))
  })
  
  
  ## 2. Initialise UI elements ----
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
  
  
  # Get data JPS info
  output$code_JPS <- renderText({paste0(SQL_output$code_JPS)})
  
  
  ## 3. Plotting ----
  # Populate drop-down menu's with transactions
  observe({
    updateSelectInput(session = session, inputId = "plot1_y", choices = SQL_output$data_reshaped_Sector_R[, "Transactie"])
  })
  
  # Render plot
  output$plot1 <- renderPlot({
    
    plot_y_data <- as.numeric(SQL_output$data_reshaped_Sector_R[which(SQL_output$data_reshaped_Sector_R[ , "Transactie"] == input$plot1_y), 3:17])
    
    if(any(plot_y_data >= 0)){temp_col <- "steelblue"} else{temp_col <- "red"}
    
    plot(x = 1:15, y = plot_y_data, type = "l", xlab = "Tijd", ylab = paste("Waarde (in miljoenen euro's)"), xaxt = "n", 
         main = paste("Waarde van Transactie", input$plot1_y), col = temp_col)
    axis(side = 1, at = 1:15, labels = colnames(SQL_output$data_reshaped_Sector_R)[3:17])
    
  })
  
  
  ## 4. Miscellaneous ----
  
  # Download handlers
  # Actionbutton: download A.Sector_R as .CSV
  output$download_A_Sector_R <- downloadHandler(
    filename = function() {
      paste0("A_Sector_R_", input$select_sector, "_", input$select_rekening, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SQL_output$data_final_Sector_R, file, row.names = FALSE)
    }
  )
  
  # Actionbutton: download plot as jpeg
  output$download_plot1 <- downloadHandler(
    filename = function() {
      paste0("plot1_", input$plot1_y, ".jpeg", sep = "")
    },
    content = function(file) {
      jpeg(file)
      plot_y_data <- as.numeric(SQL_output$data_reshaped_Sector_R[which(SQL_output$data_reshaped_Sector_R[ , "Transactie"] == input$plot1_y), 3:17])
      if(any(plot_y_data >= 0)){temp_col <- "steelblue"} else{temp_col <- "red"}
      plot(x = 1:15, y = plot_y_data, type = "l", xlab = "Tijd", ylab = paste("Waarde (in miljoenen euro's)"), xaxt = "n", 
           main = paste("Waarde van Transactie", input$plot1_y), col = temp_col)
      axis(side = 1, at = 1:15, labels = colnames(SQL_output$data_reshaped_Sector_R)[3:17])
      dev.off()
    }
  )
  
} # closing server{}
