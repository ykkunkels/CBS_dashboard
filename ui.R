
##################################
### TEST Shiny CBS Dashboard   ###
### UI version 0.0.18          ###
### YKK - 15-08-2023           ###
### Change log:                ###
###  > Added Sector_R data     ###
###  > Expanded Sector_R data  ###
###  > Added Download .csv     ###
###  > Added decimal points    ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*###

## Load and / or Install required packages ----
if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
if(!require('shinydashboard')){install.packages('shinydashboard', dep = TRUE)};library('shinydashboard')
if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
if(!require('odbc')){install.packages('odbc', dep = TRUE)};library('odbc')
if(!require('DT')){install.packages('DT', dep = TRUE)};library('DT')
if(!require('shinycssloaders')){install.packages('shinycssloaders', dep = TRUE)};library('shinycssloaders')

## UI ----
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    ## Header ----
                    dashboardHeader(title = "CBS Dashboard"),
                    
                    ## Sidebar ----
                    dashboardSidebar(width = 230,
                                     sidebarMenu(menuItem("Menu"), id = "sidebarmenu",
                                                 menuItem("Welkom", tabName = "welkom_tab", icon = icon("door-open")),
                                                 menuItem("JPS", tabName = "JPS_tab", icon = icon("book")),
                                                 
                                                 menuItem("Sectoranalyse", tabName = "A_tab", icon = icon("a")),
                                                 menuItem("Sectoranalyse", tabName = "B_tab", icon = icon("b")),
                                                 menuItem("Grote verschillen-analyse", tabName = "G_tab", icon = icon("g")),
                                                 menuItem("Eindintegratie", tabName = "E_tab", icon = icon("e")),
                                                 menuItem("Details", tabName = "X_tab", icon = icon("x")),
                                                 menuItem("Overige tabbladen", tabName = "overige_tab", icon = icon("clipboard-question")),
                                                 
                                                 menuItem("Visualisations", tabName = "visualisations_tab", icon = icon("chart-line")),
                                                 menuItem("Settings", tabName = "settings_tab", icon = icon("cog")),
                                                 
                                                 uiOutput("logo", style = "background-color: white;"),
                                                 h5("version 0.0.18", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt; 
                                                    position: relative; left: 30px;")
                                     ) # closing sidebarMenu()
                    ), # closing dashboardSidebar()
                    
                    ## Body ----
                    dashboardBody(useShinyjs(),
                                  
                                  # Setup tabItems            
                                  tabItems(
                                    
                                    tabItem(tabName = "welkom_tab", # welkom tab ----
                                            
                                            div(uiOutput("img_welkom", align = "center"),
                                                h1("CBS Dashboard in R Shiny", align = "center")
                                            )
                                            
                                    ), # closing B tabItem()
                                    
                                    tabItem(tabName = "JPS_tab", # JPS tab ----
                                            
                                            fluidPage(
                                              
                                              fluidRow(
                                                
                                                column(width = 3, style = "margin-top: 0; margin-bottom: 0;", h5(strong("Jaar"))),
                                                column(width = 3, style = "margin-top: 0; margin-bottom: 0;", h5(strong("Status")))
                                                
                                              ),
                                              
                                              # data JPS: Populate drop-down menu's directly from SQL
                                              fluidRow(
                                                column(3, selectInput(inputId = "select_JPS_jaar", label = "", width = "100px", 
                                                                      selected = (as.integer(substr(Sys.time(), 1, 4)) - 2),
                                                                      choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001",
                                                                                                       Database = "HSR_ANA_PRD"),
                                                                                             paste0("SELECT DISTINCT Jaar FROM tbl_SR_JPSReferentie ORDER BY Jaar DESC")))
                                                )
                                                ),
                                                column(3, selectInput(inputId = "select_JPS_status", label = "", width = "100px",
                                                                      selected = "R", #! make dynamic
                                                                      choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001",
                                                                                                       Database = "HSR_ANA_PRD"),
                                                                                             paste0("SELECT DISTINCT Status FROM tbl_SR_JPSReferentie ORDER BY Status")))
                                                )
                                                )
                                              ) # closing fluidRow()
                                              
                                            ),
                                            
                                            
                                            hr(style = "border-top: 1px solid #000000"),
                                            
                                            withSpinner(DTOutput("data_JPS"), type = 6)
                                            
                                    ), # closing JPS tabItem()
                                    
                                    tabItem(tabName = "A_tab", # A tab ----
                                            
                                            ## Navbarpage: A. tabbladen
                                            navbarPage(
                                              "A. Sectoranalyse",
                                              tabPanel("Plausibiliteit_R"),
                                              tabPanel("Sector_R",
                                                       
                                                       fluidPage(
                                                         
                                                         # Sector_R: Populate dropdown menu's directly from SQL 
                                                         fluidRow(
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Sector"))),
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Rekening"))),
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("TransactieSoort"))),
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("JPS"))),
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Download selection as .CSV")))
                                                           
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_sector", label = "", width = "125px",
                                                                              selected = "S.11",
                                                                              choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                               Database = "HSR_ANA_PRD"), 
                                                                                                     paste0("SELECT DISTINCT Sector FROM tbl_SR_Data_Transacties ORDER BY Sector")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_rekening", label = "", width = "125px",
                                                                              selected = "LT",
                                                                              choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                               Database = "HSR_ANA_PRD"), 
                                                                                                     paste0("SELECT DISTINCT Rekening FROM tbl_SR_Data_Transacties")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 2, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_transactiesoort", label = "", width = "125px",
                                                                              selected = "all",
                                                                              choices = c("all", dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                                      Database = "HSR_ANA_PRD"), 
                                                                                                            paste0("SELECT DISTINCT Transactiesoort FROM tbl_SR_Data_Transacties ORDER BY Transactiesoort")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 2, style = "margin-top: -5px; margin-bottom: -25px;",
                                                                  verbatimTextOutput("code_JPS")
                                                           ),
                                                           
                                                           column(width = 2, style = "margin-top: -5px; margin-bottom: -25px;",
                                                                  downloadButton(outputId = "download_A_Sector_R", label = "Download")
                                                           )
                                                         ), # closing fluidRow()
                                                         
                                                         hr(style = "border-top: 1px solid #000000")
                                                         
                                                       ), # closing fluidPage()
                                                       
                                                       
                                                       # Sector_R: Data
                                                       withSpinner(DTOutput("data_Sector_R"), type = 6)),
                                              
                                              tabPanel("Sector_B"),
                                              tabPanel("Saldi_Sector"),
                                              tabPanel("SaldiOverzicht"),
                                              tabPanel("Sectorverslag"),
                                              tabPanel("Kerncijfers")
                                              
                                            ) # closing navbarPage()
                                            
                                    ), # closing A tabItem()
                                    
                                    tabItem(tabName = "B_tab", # B tab ----
                                            
                                            ## Navbarpage: B. tabbladen
                                            navbarPage("B. Sectoranalyse",
                                                       tabPanel("Extern_saldo_D4"),
                                                       tabPanel("Extern vermogen_excl_AF11"),
                                                       tabPanel("Kerncijfers_S.1"),
                                                       tabPanel("Kerncijfers_S.11"),
                                                       tabPanel("Kerncijfers_S.12"),
                                                       tabPanel("Kerncijfers_S.13"),
                                                       tabPanel("Kerncijfers_S.1A"),
                                                       tabPanel("Kerncijfers_S.2"))
                                            
                                    ), # closing B tabItem()
                                    
                                    tabItem(tabName = "G_tab", # G tab ----
                                            
                                            ## Navbarpage: G. tabbladen
                                            navbarPage("G. Grote verschillen-analyse",
                                                       tabPanel("SIM Correcties Y"),
                                                       tabPanel("SIM Correcties Q"),
                                                       tabPanel("Transactie_B"),
                                                       tabPanel("GVA – SX-en"),
                                                       tabPanel("GVA – TX-en"),
                                                       tabPanel("GVA – YX-en"),
                                                       tabPanel("GVA – Existentie JK"),
                                                       tabPanel("Negatieve EB"))
                                            
                                    ), # closing G tabItem()
                                    
                                    tabItem(tabName = "E_tab", # G tab ----
                                            
                                            ## Navbarpage: E. tabbladen
                                            navbarPage("E. Eindintegratie",
                                                       tabPanel("SVs reeks"),
                                                       tabPanel("SV_onderdeel"),
                                                       tabPanel("SV correcties"))
                                            
                                    ), # closing E tabItem()
                                    
                                    tabItem(tabName = "X_tab", # G tab ----
                                            
                                            ## Navbarpage: X. tabbladen
                                            navbarPage("X. Details",
                                                       tabPanel("Maak_eigen_tabel"),
                                                       tabPanel("Draaitabel_tellen2"))
                                            
                                    ), # closing X tabItem()
                                    
                                    tabItem(tabName = "overige_tab", # G tab ----
                                            
                                            ## Navbarpage: overige tabbladen
                                            navbarPage("overige Tabbladen",
                                                       tabPanel("Commentaar"),
                                                       tabPanel("Sectoren"),
                                                       tabPanel("Transactie"),
                                                       tabPanel("Blad1"),
                                                       tabPanel("Onderdelen"))
                                            
                                    ), # closing overige tabItem()
                                    
                                    tabItem(tabName = "visualisations_tab", # Visualisations tab ----
                                            
                                            h5("Welcome! Soon visualisation functionality will be placed here"),
                                            
                                            selectInput(inputId = "plot1_x", label = "x-axis", choices = NULL),
                                            selectInput(inputId = "plot1_y", label = "y-axis", choices = NULL),
                                            
                                            plotOutput(outputId = "plot1", height = "600px")
                                            
                                    ), # closing Visualisations tabItem()
                                    
                                    tabItem(tabName = "settings_tab", # Settings tab ----
                                            
                                            ## Role selection ----
                                            hr(style = "border-top: 1px solid #000000"),
                                            h4("Work in progress"),
                                            radioButtons("selected_role", "Select your role to customise data selection:", 
                                                         inline = TRUE, selected = "Eindintegrator",
                                                         c("Eindintegrator" = "Eindintegrator",
                                                           "Sectorspecialist" = "Sectorspecialist",
                                                           "Transactiespecialist" = "Transactiespecialist",
                                                           "Duale classificatiespecialist" = "Duale classificatiespecialist",
                                                           "SIM-expert" = "SIM-expert",
                                                           "CWC-lid / Projectleider" = "CWC-lid / Projectleider",
                                                           "R expert" = "R expert",
                                                           "Custom (Advanced)" = "Custom")
                                            )
                                    ) # closing Settings tabItem()
                                    
                                  ) # closing tabItems()
                                  
                    ) # closing dashboardBody()        
                    
) # closing dashboardPage()