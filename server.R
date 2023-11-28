
################################
### TEST Shiny CBS Dashboard ###
### Server version 0.0.31    ###
### YKK - 28-11-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

server <- function(input, output, session) {
  
  ## 0. Basic Operations -----------------------------------------------------------------------------------------------------------
  # Define & initialise reactiveValues objects: parameters
  SQL_parameters <- reactiveValues(connection = NA, query = NA, query_JPS = NA)
  selection_parameters <- reactiveValues(select_transactiesoort = NA, select_onderdeel = NA)
  misc_parameters <- reactiveValues(ncols = NA)
  plot_parameters <- reactiveValues(temp_drop = 0, labels = NA)
  user_parameters <- reactiveValues(feedback_save = TRUE)
  
  # Define & initialise reactiveValues objects: data
  JPS_data <- reactiveValues(code_JPS = NA, data_JPS = NA, data_JPS_df = NA, drop_JPS = NA, data_JPS_df_final = matrix(NA, 1, 1),
                             data_JPS_df_colnames = c("Periode", "1", "2", "3", "4", "Y"), data_JPS_df_ncol = 6, required_JPS = NA)
  main_data <- reactiveValues(data_Sector_R_early = NA, data_Sector_R = NA, data_Sector_R_aggregated = NA,
                              data_Sector_R_reshaped = NA, data_Sector_R_adjusted1 = NA, data_Sector_R_adjusted2 = NA, 
                              data_Sector_R_bijstelling = NA, data_Sector_R_final = NA, data_Sector_R_bijstelling_absoluut = NA, 
                              data_Sector_R_bijstelling_procentueel = NA, data_shown_now = NA)
  misc_data <- reactiveValues("totals_A" = NA, "totals_B" = NA, "totals_M" = NA, "totals_P" = NA)
  
  # Define SQL connection
  SQL_parameters$connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
  
  
  ## 1. Initialise: JPS Data --------------------------------------------------------------------------------------------------------
  # Query JPS data
  data_JPS <- reactive({
    
    SQL_parameters$query_JPS <- paste0("SELECT JPS_code, Smin1_code, Tmin1_code, Tmin1Ref_code, Qmin1_code
                                  FROM tbl_SR_JPSReferentie
                                  WHERE	Jaar = ('",input$select_JPS_jaar,"') AND Status = ('",input$select_JPS_status,"')")
    
    JPS_data$data_JPS <- dbGetQuery(SQL_parameters$connection, SQL_parameters$query_JPS)
    
    JPS_data$code_JPS <- JPS_data$data_JPS$JPS_code[1]
    
  })
  
  # Hard-code JPS data dataframe
  data_JPS_hardcoded <- reactive({
    
    data_JPS()
    
    JPS_data$data_JPS_df <- data.frame(
      matrix(data = c("JPS", "Referentie", "Tmin1", "Tmin1 Referentie", "Qmin1",
                      JPS_data$data_JPS$JPS_code[2], JPS_data$data_JPS$Smin1_code[2], JPS_data$data_JPS$Tmin1_code[2], JPS_data$data_JPS$Tmin1Ref_code[2], JPS_data$data_JPS$Qmin1_code[2],
                      JPS_data$data_JPS$JPS_code[3], JPS_data$data_JPS$Smin1_code[3], JPS_data$data_JPS$Tmin1_code[3], JPS_data$data_JPS$Tmin1Ref_code[3], JPS_data$data_JPS$Qmin1_code[3],
                      JPS_data$data_JPS$JPS_code[4], JPS_data$data_JPS$Smin1_code[4], JPS_data$data_JPS$Tmin1_code[4], JPS_data$data_JPS$Tmin1Ref_code[4], JPS_data$data_JPS$Qmin1_code[4],
                      JPS_data$data_JPS$JPS_code[5], JPS_data$data_JPS$Smin1_code[5], JPS_data$data_JPS$Tmin1_code[5], JPS_data$data_JPS$Tmin1Ref_code[5], JPS_data$data_JPS$Qmin1_code[5],
                      JPS_data$data_JPS$JPS_code[1], JPS_data$data_JPS$Smin1_code[1], JPS_data$data_JPS$Tmin1_code[1], JPS_data$data_JPS$Tmin1Ref_code[1], JPS_data$data_JPS$Qmin1_code[1]
      ), nrow = 5, ncol = 6
      )
    )
    
    observeEvent(input$select_JPS_periode, {
      
      # Drop columns depending on dropdown "Periode" input
      if(input$select_JPS_periode == "Y + Q"){JPS_data$data_JPS_df_final <- JPS_data$data_JPS_df; JPS_data$drop_JPS <- NA; JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))}
      if(input$select_JPS_periode == "Y"){JPS_data$drop_JPS <- c(2, 3, 4, 5); JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "Y"]}
      if(input$select_JPS_periode == "Q1"){JPS_data$drop_JPS <- c(3, 4, 5, 6); JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "1"]}
      if(input$select_JPS_periode == "Q2"){JPS_data$drop_JPS <- c(2, 4, 5, 6); JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "2"]}
      if(input$select_JPS_periode == "Q3"){JPS_data$drop_JPS <- c(2, 3, 5, 6); JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "3"]}
      if(input$select_JPS_periode == "Q4"){JPS_data$drop_JPS <- c(2, 3, 4, 6); JPS_data$required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "4"]}
      
      if(all(is.na(JPS_data$drop_JPS)) ==  FALSE){JPS_data$data_JPS_df_final <- JPS_data$data_JPS_df[, -JPS_data$drop_JPS]}
      
    })
    
  })
  
  
  # Render JPS data
  output$data_JPS <- renderDT({
    
    data_JPS_hardcoded()
    
    # Render JPS DT depending on JPS dropdown input
    if(input$select_JPS_periode != "Y + Q"){
      
      datatable(JPS_data$data_JPS_df_final,
                colnames = JPS_data$data_JPS_df_colnames[-JPS_data$drop_JPS], rownames = F,
                caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
                options = list(dom = "t",
                               initComplete = JS( #change colnames fontsize
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'color': 'steelblue'});",
                                 "}"))
      )
      
    }else{
      
      datatable(JPS_data$data_JPS_df_final,
                colnames = JPS_data$data_JPS_df_colnames, rownames = F,
                caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
                options = list(dom = "t",
                               initComplete = JS( #change colnames fontsize
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'color': 'steelblue'});",
                                 "}"))
      )
    }
  })
  
  
  ## 2. Initialise: Sector_R Data ----------------------------------------------------------------------------------------------------
  
  # Query Sector_R_early data
  data_Sector_R_early <- reactive({
    
    SQL_parameters$query <- paste0("SELECT Jaar, Periode, Status, Sector, Transactie, TransactieSoort, Onderdeel, Waarde_Type, Rekening, Waarde
                               FROM tbl_SR_Data_Transacties
                               WHERE Jaar BETWEEN ('",(as.integer(substr(JPS_data$code_JPS, 1, 4)) - 1),"') AND ('",as.integer(substr(JPS_data$code_JPS, 1, 4)),"')
                               ")
    
    main_data$data_Sector_R_early <- dbGetQuery(SQL_parameters$connection, SQL_parameters$query)
    
  })
  
  
  # Query Sector_R data
  data_Sector_R <- reactive({
    
    data_Sector_R_early()
    
    # Select query based on variable that can have "all" input ("select_transactiesoort" and "select_onderdeel")
    if(all(input$select_transactiesoort == "all" & input$select_onderdeel == "all")){
      
      temp_data <- main_data$data_Sector_R_early[which(main_data$data_Sector_R_early$Sector == input$select_sector), ]
      temp_data <- temp_data[which(temp_data$Rekening == input$select_rekening), ]
      
    }else if(all(input$select_transactiesoort == "all" & any(input$select_onderdeel != "all"))){
      
      temp_data <- main_data$data_Sector_R_early[which(main_data$data_Sector_R_early$Sector == input$select_sector), ]
      temp_data <- temp_data[temp_data$Onderdeel %in% input$select_onderdeel, ]
      temp_data <- temp_data[which(temp_data$Rekening == input$select_rekening), ]
      
    }else if(any(input$select_transactiesoort != "all" & input$select_onderdeel == "all")){
      
      temp_data <- main_data$data_Sector_R_early[which(main_data$data_Sector_R_early$Sector == input$select_sector), ]
      temp_data <- temp_data[temp_data$TransactieSoort %in% input$select_transactiesoort, ]
      temp_data <- temp_data[which(temp_data$Rekening == input$select_rekening), ]
      
    }else if(any(input$select_transactiesoort != "all" & input$select_onderdeel != "all")){
      
      temp_data <- main_data$data_Sector_R_early[which(main_data$data_Sector_R_early$Sector == input$select_sector), ]
      temp_data <- temp_data[temp_data$TransactieSoort %in% input$select_transactiesoort, ]
      temp_data <- temp_data[temp_data$Onderdeel %in% input$select_onderdeel, ]
      temp_data <- temp_data[which(temp_data$Rekening == input$select_rekening), ]
      
    }
    
    main_data$data_Sector_R <- temp_data
  })
  
  
  # Aggregate Sector_R data
  data_Sector_R_aggregated <- reactive({
    
    data_Sector_R()
    
    isolate({
      
      # Add JPS variable
      main_data$data_Sector_R$JPS <- paste0(main_data$data_Sector_R$Jaar, "-",
                                            main_data$data_Sector_R$Periode, "-",
                                            main_data$data_Sector_R$Status)
      
      # Aggregate data to sum over values
      main_data$data_Sector_R_aggregated <- aggregate(Waarde ~ JPS + Rekening + TransactieSoort + Transactie + Onderdeel, main_data$data_Sector_R, sum)
      
      # Select only required JPSen
      # required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))
      main_data$data_Sector_R_aggregated <- main_data$data_Sector_R_aggregated[main_data$data_Sector_R_aggregated$JPS %in% JPS_data$required_JPS, ]
      
      # # Select only required JPSen depending on JPS dropdown input
      # if(input$select_JPS_periode == "Y + Q"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))
      #   
      # }else if(input$select_JPS_periode == "Y"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "Y"]
      #   
      # }else if(input$select_JPS_periode == "1"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "1"]
      #   
      # }else if(input$select_JPS_periode == "2"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "2"]
      #   
      # }else if(input$select_JPS_periode == "3"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "3"]
      #   
      # }else if(input$select_JPS_periode == "4"){
      #   
      #   required_JPS <- as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)]))[substr(as.character(unlist(JPS_data$data_JPS[c(2:5, 1), c(2, 1, 3)])), 6, 6) == "4"]
      #   
      # }
      
      # main_data$data_Sector_R_aggregated <- main_data$data_Sector_R_aggregated[main_data$data_Sector_R_aggregated$JPS %in% JPS_data$required_JPS, ]
      
      
      
    }) # closing isolate()
  })
  
  # Reshape Sector_R data from long to wide
  data_Sector_R_reshaped <- reactive({
    
    data_Sector_R_aggregated()
    
    # Reshape data from long to wide    
    main_data$data_Sector_R_reshaped <- reshape(main_data$data_Sector_R_aggregated, direction = 'wide', idvar = c("Transactie", "TransactieSoort", "Onderdeel"), timevar = "JPS")
    
  })
  
  
  # Adjust Sector_R data; renaming, sorting, etc.
  data_Sector_R_adjusted1 <- reactive({
    
    data_Sector_R_reshaped() 
    
    isolate({
      
      if(any(grep("Rekening.", colnames(main_data$data_Sector_R_reshaped)) !=0)){
        
        # Drop unnecessary columns
        drop <- grep(c("Rekening."), colnames(main_data$data_Sector_R_reshaped))
        main_data$data_Sector_R_reshaped <- main_data$data_Sector_R_reshaped[, -drop]
        
      }
      
      if(any(grep("Waarde.", colnames(main_data$data_Sector_R_reshaped)) !=0)){
        
        # Rename colnames
        colnames(main_data$data_Sector_R_reshaped) <- gsub("Waarde.", "", colnames(main_data$data_Sector_R_reshaped))
        
      }
      
      # Initialise variables to sort columns
      misc_parameters$ncols <- ncol(main_data$data_Sector_R_reshaped)
      temp_nchar <- max(nchar(colnames(main_data$data_Sector_R_reshaped)[4:misc_parameters$ncols]))
      
      # Sort columns
      main_data$data_Sector_R_reshaped <- main_data$data_Sector_R_reshaped[c(1:3, order(substr(colnames(main_data$data_Sector_R_reshaped[4:misc_parameters$ncols]), 1, 4), 
                                                                                        substr(colnames(main_data$data_Sector_R_reshaped[4:misc_parameters$ncols]), 8, temp_nchar)) + 3)]
      
      
      # Summing data over "Onderdeel": initialise if-loop exception (summing only required when N "Onderdeel" > 1)
      if(length(input$select_onderdeel) > 1){
        
        # Summing data over "Onderdeel": merge data by "Transactie" & "TransactieSoort"
        temp_onderdeel_split <- split(main_data$data_Sector_R_reshaped, main_data$data_Sector_R_reshaped[, "Onderdeel"])
        temp_data_sum_onderdeel <- merge(temp_onderdeel_split[[1]], temp_onderdeel_split[[2]], by = c("Transactie", "TransactieSoort"))
        
        # Summing data over "Onderdeel": fix cols and colnames, and assign to main_data
        temp_data_sum_onderdeel[, grep("Onderdeel", colnames(temp_data_sum_onderdeel))] <- NULL
        temp_data_sum_onderdeel[is.na(temp_data_sum_onderdeel)] <- 0 # set NA's to zero
        temp_data_sum_onderdeel <- cbind(temp_data_sum_onderdeel[, c(1, 2)], temp_data_sum_onderdeel[, grep(".x", colnames(temp_data_sum_onderdeel))] + temp_data_sum_onderdeel[, grep(".y", colnames(temp_data_sum_onderdeel))])
        colnames(temp_data_sum_onderdeel) <- gsub(pattern = ".x", replacement = "", x = colnames(temp_data_sum_onderdeel))
        main_data$data_Sector_R_reshaped <- temp_data_sum_onderdeel
      }
      
      # Remove onderdeel
      main_data$data_Sector_R_reshaped$Onderdeel <- NULL
      
      
      # Sum "all" Onderdeel
      if(all(input$select_onderdeel == "all")){
        temp_onderdeel_loc <- which(colnames(main_data$data_Sector_R_reshaped) == "Onderdeel")
        main_data$data_Sector_R_reshaped[is.na(main_data$data_Sector_R_reshaped)] <- 0
        main_data$data_Sector_R_reshaped <- aggregate(. ~ Transactie + TransactieSoort, main_data$data_Sector_R_reshaped[, -temp_onderdeel_loc], sum)
      }
      
      # Get TransactieSoort totals
      misc_data$totals_A <- colSums(main_data$data_Sector_R_reshaped[main_data$data_Sector_R_reshaped$TransactieSoort == "A", ] %>% select(-any_of(c("TransactieSoort", "Transactie", "Onderdeel"))), na.rm = TRUE)
      misc_data$totals_B <- colSums(main_data$data_Sector_R_reshaped[main_data$data_Sector_R_reshaped$TransactieSoort == "B", ] %>% select(-any_of(c("TransactieSoort", "Transactie", "Onderdeel"))), na.rm = TRUE)
      misc_data$totals_M <- colSums(main_data$data_Sector_R_reshaped[main_data$data_Sector_R_reshaped$TransactieSoort == "M", ] %>% select(-any_of(c("TransactieSoort", "Transactie", "Onderdeel"))), na.rm = TRUE)
      misc_data$totals_P <- colSums(main_data$data_Sector_R_reshaped[main_data$data_Sector_R_reshaped$TransactieSoort == "P", ] %>% select(-any_of(c("TransactieSoort", "Transactie", "Onderdeel"))), na.rm = TRUE)
      
      for(i in 1:length(na.omit(unique(main_data$data_Sector_R_reshaped$TransactieSoort)))){
        
        # Add an empty row for every TransactieSoort totals
        main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped) + 1, ] <- NA
        
        # Fill empty row
        main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), "TransactieSoort"] <- unique(main_data$data_Sector_R_reshaped$TransactieSoort)[i]
        main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), "Transactie"] <- "Totaal"
        
        # Fill empty row (exception for "Onderdeel" value which is NULL when "Onderdeel" is set to "all")
        if(!is.null(main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), "Onderdeel"])){
          main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), "Onderdeel"] <- main_data$data_Sector_R_reshaped[1, "Onderdeel"]
        }
        
        temp_target_cols <- colnames(main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), ] %>% select(-any_of(c("TransactieSoort", "Transactie", "Onderdeel"))))
        main_data$data_Sector_R_reshaped[nrow(main_data$data_Sector_R_reshaped), temp_target_cols] <- eval(parse(text = paste0("misc_data$totals_", unique(main_data$data_Sector_R_reshaped$TransactieSoort)[i])))
        
      }
      
      # Create dataset "bijstelling_absoluut"
      if(JPS_data$code_JPS %in% colnames(main_data$data_Sector_R_reshaped)){
        
        temp_JPS_location_last_1 <- which(colnames(main_data$data_Sector_R_reshaped) == JPS_data$code_JPS)
        temp_JPS_location_first_1 <- (temp_JPS_location_last_1 - 4)
        
        temp_bijstelling_1 <- main_data$data_Sector_R_reshaped[, (temp_JPS_location_first_1:temp_JPS_location_last_1)]
        
        temp_JPS_location_last_2 <- which(colnames(main_data$data_Sector_R_reshaped) == JPS_data$data_JPS[1, 2])
        temp_JPS_location_first_2 <- (temp_JPS_location_last_2 - 4)
        
        # Exception for any single "Onderdeel" of which no "bijstelling-tabel" can be made
        if(length(input$select_onderdeel) == 1 & all(input$select_onderdeel %in% c("10"))){
          
          temp_message <- matrix(paste("Bijstelling not available for onderdeel ", 
                                       input$select_onderdeel), 1, 1)
          
          main_data$data_Sector_R_bijstelling_procentueel <- main_data$data_Sector_R_bijstelling_absoluut <- temp_message
          
        }else{
          
          temp_bijstelling_2 <- main_data$data_Sector_R_reshaped[, (temp_JPS_location_first_2:temp_JPS_location_last_2)]
          
          main_data$data_Sector_R_bijstelling_absoluut <- (temp_bijstelling_1 - temp_bijstelling_2)
          main_data$data_Sector_R_bijstelling_absoluut <- cbind(main_data$data_Sector_R_reshaped[, 1:2], main_data$data_Sector_R_bijstelling_absoluut)
          
          # Create dataset "bijstelling_procentueel"
          main_data$data_Sector_R_bijstelling_procentueel <- round(((temp_bijstelling_1 / (temp_bijstelling_1 + temp_bijstelling_2)) * 100), 2)
          main_data$data_Sector_R_bijstelling_procentueel <- cbind(main_data$data_Sector_R_reshaped[, 1:3], main_data$data_Sector_R_bijstelling_procentueel)
        }
        
      }
      
      # Add decimal points (#! This is only a cosmetic change, but can introduce errors down the line (i.e., data is now character instead of numeric(!)))
      for(i in 3:(ncol(main_data$data_Sector_R_reshaped))){
        main_data$data_Sector_R_reshaped[, i] <- prettyNum(as.vector(main_data$data_Sector_R_reshaped[, i]), big.mark = ".", big.interval = 3L, decimal.mark = ",", scientific = FALSE)
      }
      
      # Set "NA" and zero's to whitespace (#! This is only a cosmetic change, but can introduce errors down the line (i.e., can't do calculations with whitespaces, must have zero's for that instead(!)))
      main_data$data_Sector_R_reshaped[main_data$data_Sector_R_reshaped == "NA" | main_data$data_Sector_R_reshaped == 0] <- ""
      
      # Save intermediary data
      main_data$data_Sector_R_adjusted1 <- main_data$data_Sector_R_reshaped
      
    }) # closing isolate()
    
  })
  
  # Adjust Sector_R data; n column selection
  data_Sector_R_adjusted2 <- reactive({
    # browser()
    data_Sector_R_adjusted1()
    
    # Set number of years after JPS according to dropdown
    temp <- tail(grep("-", colnames(main_data$data_Sector_R_adjusted1)), (5 * as.integer(input$settings_ncol)))
    temp <- unique(c((1:(grep("-", colnames(main_data$data_Sector_R_adjusted1))[1] - 1)), temp))
    main_data$data_Sector_R_adjusted2 <- main_data$data_Sector_R_adjusted1[, temp]
    
    # Update ncols
    misc_parameters$ncols <- ncol(main_data$data_Sector_R_adjusted2)
    
    # Finalise dataset 
    main_data$data_Sector_R_final <- main_data$data_Sector_R_adjusted2
    
  })
  
  
  # Render Sector_R data
  output$data_Sector_R <- renderDT({
    
    data_Sector_R_adjusted2()
    
    # Check to see if user selected JPS before trying to view data Sector_R
    if(any(is.na(JPS_data$data_JPS)) & is.na(JPS_data$code_JPS)){
      showModal(modalDialog(title = "Er is nog géén JPS geselecteerd!", 
                            HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/7/74/Feedbin-Icon-error.svg">'),
                            "Selecteer eerst een JPS", footer = tagList(actionButton(inputId = "goto_JPS", label = "Ga naar JPS'en"))))
    } 
    
    # Select data to shown dependent on dropdown menu
    if(input$select_A_tabel == "Standaard"){
      main_data$data_shown_now <- main_data$data_Sector_R_final
    }else if(input$select_A_tabel == "Bijstelling" && input$select_absoluut == "Absoluut"){
      main_data$data_shown_now <- main_data$data_Sector_R_bijstelling_absoluut
    }else if(input$select_A_tabel == "Bijstelling" && input$select_absoluut == "Procentueel"){
      main_data$data_shown_now <- main_data$data_Sector_R_bijstelling_procentueel
    }else if(input$select_A_tabel == "Q-1/Y-1"){
      datatable(matrix(c(2,2)))
    }else if(input$select_A_tabel == "Q-4/Y-1"){
      datatable(matrix(c(3,3)))
    }
    
    datatable(main_data$data_shown_now, rownames = NULL,
              caption = htmltools::tags$caption(style = 'caption-side: bottom;','Data retrieved on ', htmltools::em(Sys.time())),
              options = list(scrollX = TRUE, pageLength = input$settings_nrow, lengthMenu = c(12, 20, 30, 50, 100, input$settings_nrow), 
                             columnDefs = list(list(className = 'dt-head-right', targets = "_all"), list(className = 'dt-right', targets = "_all")),
                             initComplete = JS( #change colnames fontsize
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '85%'});",
                               "$(this.api().table().header()).css({'color': 'steelblue'});",
                               "}")
              ))
    
  }) # closing renderDT({})
  
  
  ## 3. Initialise UI elements -------------------------------------------------------------------------------------------------------
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
  output$code_JPS <- renderText({paste0(JPS_data$code_JPS)})
  
  
  ## 4. Plotting ----
  # Populate drop-down menu's with transactions
  observe({
    updateSelectInput(session = session, inputId = "plot1_y", choices = main_data$data_shown_now["Transactie"])
  })
  
  # Initialise plot
  plot1 <- reactive({
    # Check to see if user selected JPS before trying to plot
    if(is.na(misc_parameters$ncols)){
      showModal(modalDialog(title = "Er is nog géén JPS en/of data geselecteerd!", 
                            HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/7/74/Feedbin-Icon-error.svg">'),
                            "Selecteer eerst een JPS en/of data", 
                            footer = tagList(actionButton(inputId = "goto_JPS", label = "Ga naar JPS'en"),
                                             actionButton(inputId = "goto_Sectoranalyse", label = "Ga naar Sector R data"))))
    } 
    
    # Get x- and y-axis data, set labels
    plot_data <- list("x" = NA, "y" = NA)
    temp_data <- main_data$data_shown_now[main_data$data_shown_now$TransactieSoort == input$plot1_B_or_M, ]
    plot_data$y <- as.numeric(temp_data[which(temp_data["Transactie"] == input$plot1_y), 4:misc_parameters$ncols])
    n_yaxis <- length(plot_data$y)
    plot_data$x <- 1:n_yaxis
    plot_parameters$labels <- colnames(main_data$data_shown_now)[4:(n_yaxis + 3)]
    
    # Exception for dropping year means
    if(input$drop_year_means == TRUE){
      plot_parameters$temp_drop <- which(substr(colnames(main_data$data_shown_now)[4:misc_parameters$ncols], 5, 7) == "-Y-")
      plot_data$y <- plot_data$y[-plot_parameters$temp_drop]
      plot_parameters$labels <- colnames(main_data$data_shown_now)[4:(n_yaxis + 3)][-plot_parameters$temp_drop]
      n_yaxis <- length(plot_data$y)
      plot_data$x <- 1:n_yaxis
    }
    
    # Plotting and axis customisation; clip() for colour change below zero
    plot(x = plot_data$x, y = plot_data$y, type = "l", xlab = "Tijd", ylab = paste("Waarde (in miljoenen euro's)"), xaxt = "n", 
         main = paste("Waarde van Transactie", input$plot1_y), col = "steelblue")
    if(!any(is.na(plot_data$y))){
      clip(x1 = min(plot_data$x),
           x2 = max(plot_data$x),
           y1 = min(plot_data$y),
           y2 = 0)
    }
    lines(plot_data, col = "firebrick1")
    axis(side = 1, at = 1:n_yaxis, labels = plot_parameters$labels)
    abline(h = 0)
    abline(v = plot_data$x, col = "lightgrey")
    
    # Record plot for downloadHandler()
    recordPlot()
    
  })
  
  # Render plot
  output$plot1 <- renderPlot({replayPlot(req(plot1()))})
  
  
  ## 5. Miscellaneous ----------------------------------------------------------------------------------------------------------------
  # Dynamic welcome text
  if(as.numeric(substr(Sys.time(), 12, 13)) >= 06 & as.numeric(substr(Sys.time(), 12, 13)) < 12){
    output$welcome_text <- renderText(paste("Goedemorgen, welkom bij het"))
  }else if(as.numeric(substr(Sys.time(), 12, 13)) >= 12 & as.numeric(substr(Sys.time(), 12, 13)) < 17){
    output$welcome_text <- renderText(paste("Goedemiddag, welkom bij het"))
  }else if(as.numeric(substr(Sys.time(), 12, 13)) >= 17 & as.numeric(substr(Sys.time(), 12, 13)) < 23){
    output$welcome_text <- renderText(paste("Goedenavond, welkom bij het"))
  }else if(as.numeric(substr(Sys.time(), 12, 13)) >= 23 & as.numeric(substr(Sys.time(), 12, 13)) < 06){
    output$welcome_text <- renderText(paste("Goedenacht, welkom bij het"))
  }
  
  # Download handlers
  # Actionbutton: download A.Sector_R as .CSV
  output$download_A_Sector_R <- downloadHandler(
    filename = function() {
      paste0("A_Sector_R_", input$select_sector, "_", input$select_rekening, ".csv", sep = "")
    },
    content = function(file) {
      write.table(main_data$data_Sector_R_final, file, row.names = FALSE, sep = ";")
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
  
  # Jump to JPS tab when goto_JPS button is clicked
  observeEvent(input$goto_JPS, {
    updateTabItems(session, "sidebarmenu", "JPS_tab")
    removeModal()
  })
  
  # Jump to JPS tab when goto_Sectoranalyse button is clicked
  observeEvent(input$goto_Sectoranalyse, {
    updateTabItems(session, "sidebarmenu", "A_tab")
    removeModal()
  })
  
  # Initialise Help image gallery
  output$help_gallery <- renderSlickR({
    help_images <- list.files("//cbsp.nl/productie/secundair/IT_NR/Werk/OntwikkelOmgeving/Dashboard ve-R-nieuwen/Software Documentatie/help_gallery", pattern=".png", full.names = TRUE)
    slickR(help_images)
  })
  
  # Change background colour
  observeEvent(input$bg_color, {
    session$sendCustomMessage("change_skin", paste0("skin-", input$bg_color))
  })
  
  # Do the following on closing app:
  # - disconnect from SQl server
  session$onSessionEnded(function() {
    reactive({dbDisconnect(SQL_parameters$connection)})
  })
  
} # closing server{}