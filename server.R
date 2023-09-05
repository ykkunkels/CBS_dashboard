
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.20    ###
### YKK - 05-09-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

server <- function(input, output, session) {
  
  ## 0. Basic Operations ----
  # Define & initialise reactiveValues objects
  SQL_input <- reactiveValues(connection = NA, query = NA, query_JPS = NA, select_transactiesoort = NA, select_onderdeel = NA, ncols = NA)
  SQL_output <- reactiveValues(data_JPS = NA, code_JPS = NA, data_Sector_R = NA, data_aggregated_Sector_R = NA)
  plot_parameters <- reactiveValues(temp_drop = 0, labels = NA)
  
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
    
    SQL_input$select_transactiesoort <- paste(input$select_transactiesoort, collapse = "', '" )
    SQL_input$select_onderdeel <- paste(input$select_onderdeel, collapse = "', '" )
    
    SQL_input$query <- paste0("SELECT Jaar, Periode, Status, Sector, Transactie, TransactieSoort, Onderdeel, Waarde_Type, Rekening, Waarde
                               FROM tbl_SR_Data_Transacties 
                               WHERE Jaar BETWEEN ('",(as.integer(substr(SQL_output$code_JPS, 1, 4)) - 1),"') AND ('",as.integer(substr(SQL_output$code_JPS, 1, 4)),"')
                               AND Waarde_Type='R' AND Sector=('",input$select_sector,"') AND Rekening=('",input$select_rekening,"') AND
                               TransactieSoort IN ('",SQL_input$select_transactiesoort,"') AND Onderdeel IN ('",SQL_input$select_onderdeel,"')")
    
    SQL_output$data_Sector_R <- dbGetQuery(SQL_input$connection, SQL_input$query)
    
  })
  
  
  # Render Sector_R standaard data
  output$data_Sector_R <- renderDT({
    
    data_Sector_R()
    
    isolate({
      
      # Add JPS variable
      SQL_output$data_Sector_R$JPS <- paste0(SQL_output$data_Sector_R$Jaar, "-",
                                             SQL_output$data_Sector_R$Periode, "-",
                                             SQL_output$data_Sector_R$Status)
      
      # Aggregate data to sum over values
      SQL_output$data_aggregated_Sector_R <- aggregate(Waarde ~ JPS + Rekening + TransactieSoort + Transactie + Onderdeel, SQL_output$data_Sector_R, sum)
      
      # Select only required JPSen
      required_JPS <- as.character(unlist(SQL_output$data_Sector_R_JPS[c(2:5, 1), c(2, 1, 3)]))
      SQL_output$data_aggregated_Sector_R <- SQL_output$data_aggregated_Sector_R[SQL_output$data_aggregated_Sector_R$JPS %in% required_JPS, ]
      
      # Reshape data from long to wide    
      SQL_output$data_reshaped_Sector_R <- reshape(SQL_output$data_aggregated_Sector_R, direction = 'wide', idvar = c("Transactie", "TransactieSoort", "Onderdeel"), timevar = "JPS")
      
      # Drop unnecessary columns
      drop <- grep(c("Rekening."), colnames(SQL_output$data_reshaped_Sector_R))
      SQL_output$data_reshaped_Sector_R <- SQL_output$data_reshaped_Sector_R[, -drop]
      
      # Rename colnames
      colnames(SQL_output$data_reshaped_Sector_R) <- gsub("Waarde.", "", colnames(SQL_output$data_reshaped_Sector_R))
      
      # Initialise variables to sort columns
      SQL_input$ncols <- ncol(SQL_output$data_reshaped_Sector_R)
      temp_nchar <- max(nchar(colnames(SQL_output$data_reshaped_Sector_R)[4:SQL_input$ncols]))
      
      # Sort columns
      SQL_output$data_reshaped_Sector_R <- SQL_output$data_reshaped_Sector_R[c(1:3, order(substr(colnames(SQL_output$data_reshaped_Sector_R[4:SQL_input$ncols]), 1, 4), 
                                                                                          substr(colnames(SQL_output$data_reshaped_Sector_R[4:SQL_input$ncols]), 8, temp_nchar)) + 3)]
      
      # Exception for when "Onderdeel" is both "05" and "10"; combine to fill rows
      if(all(SQL_input$select_onderdeel == "05', '10" & (c(5, 10) %in% as.numeric(SQL_output$data_reshaped_Sector_R[, "Onderdeel"]))) | 
         all(SQL_input$select_onderdeel == "10', '05" & (c(5, 10) %in% as.numeric(SQL_output$data_reshaped_Sector_R[, "Onderdeel"])))){
        
        temp_select_data_05 <- (as.numeric(SQL_output$data_reshaped_Sector_R[, "Onderdeel"]) == 5)
        temp_select_data_10 <- (as.numeric(SQL_output$data_reshaped_Sector_R[, "Onderdeel"]) == 10)
        
        temp_data_05 <- SQL_output$data_reshaped_Sector_R[temp_select_data_05, ]
        temp_data_10 <- SQL_output$data_reshaped_Sector_R[temp_select_data_10, ]
        
        temp_drop <- c(setdiff(temp_data_05$Transactie, temp_data_10$Transactie), setdiff(temp_data_10$Transactie, temp_data_05$Transactie))
        
        temp_data_05 <- temp_data_05[(temp_data_05[, "Transactie"] != temp_drop), ]
        temp_data_10 <- temp_data_10[(temp_data_10[, "Transactie"] != temp_drop), ]
        
        SQL_output$data_reshaped_Sector_R <- rows_patch(temp_data_05, temp_data_10, by = "Transactie")
        
        SQL_output$data_reshaped_Sector_R[, "Onderdeel"] <- "05 + 10"
        
      }
      
      # # Create "Bijstelling" table !!!WIP!!!!
      # SQL_output$data_reshaped_Sector_R[, 3:7] - SQL_output$data_reshaped_Sector_R[, 8:12]
      
      
      # Add decimal points (#! check if this doesn't introduce bugs)
      for(i in 3:(ncol(SQL_output$data_reshaped_Sector_R))){
        SQL_output$data_reshaped_Sector_R[, i] <- prettyNum(as.vector(SQL_output$data_reshaped_Sector_R[, i]), big.mark = ".", big.interval = 3L, decimal.mark = ",", scientific = FALSE)
      }
      
      # Set "NA" and zero's to whitespace
      SQL_output$data_reshaped_Sector_R[SQL_output$data_reshaped_Sector_R == "NA" | SQL_output$data_reshaped_Sector_R == 0] <- ""
      
      # Finalise dataset
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
  }) # closing renderDT({})
  
  
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
  
  # Initialise plot
  plot1 <- reactive({
    
    # Get y-axis data, get y-axis n, set color, and set labels
    plot_y_data <- as.numeric(SQL_output$data_reshaped_Sector_R[which(SQL_output$data_reshaped_Sector_R[ , "Transactie"] == input$plot1_y), 4:SQL_input$ncols])
    n_yaxis <- length(plot_y_data)
    if(any(plot_y_data >= 0)){temp_col <- "steelblue"} else{temp_col <- "red"}
    plot_parameters$labels <- colnames(SQL_output$data_reshaped_Sector_R)[4:(n_yaxis + 3)]
    
    # Exception for dropping year means
    if(input$drop_year_means == TRUE){
      plot_parameters$temp_drop <- which(substr(colnames(SQL_output$data_reshaped_Sector_R)[4:SQL_input$ncols], 5, 7) == "-Y-")
      plot_y_data <- plot_y_data[-plot_parameters$temp_drop]
      plot_parameters$labels <- colnames(SQL_output$data_reshaped_Sector_R)[4:(n_yaxis + 3)][-plot_parameters$temp_drop]
      n_yaxis <- length(plot_y_data)
    }
    
    # Plotting and axis customisation 
    plot(x = 1:n_yaxis, y = plot_y_data, type = "l", xlab = "Tijd", ylab = paste("Waarde (in miljoenen euro's)"), xaxt = "n", 
         main = paste("Waarde van Transactie", input$plot1_y), col = temp_col)
    axis(side = 1, at = 1:n_yaxis, labels = plot_parameters$labels)
    
    # Record plot for downloadHandler()
    recordPlot()
    
  })
  
  # Render plot
  output$plot1 <- renderPlot({replayPlot(req(plot1()))})
  
  
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
      jpeg(file, width = 1920, height = 540)
      replayPlot(plot1())
      dev.off()
    }
  )
  
} # closing server{}