
################################
### TEST Shiny CBS Dashboard ###
### UI version 0.0.9         ###
### YKK - 03-07-2023         ###
### Change log:              ###
### > Added tabs             ###
### > Changed role design    ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

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
                                                 menuItem("Settings", tabName = "settings_tab", icon = icon("cog")),
                                                 menuItem("Data", tabName = "data_tab", icon = icon("table")),
                                                 menuItem("JPS", tabName = "JPS_tab", icon = icon("book")),
                                                 menuItem("Visualisations", tabName = "visualisations_tab", icon = icon("chart-line")),
                                                 menuItem("Intranet", icon = icon("atlas"), href = "https://cbsintranet/default.aspx/"),
                                                 menuItem("Checks", tabName = "checks_tab", icon = icon("book-reader")),
                                                 
                                                 menuItem("A. tabbladen", tabName = "A_tab", icon = icon("clipboard-question")),
                                                 menuItem("B. tabbladen", tabName = "B_tab", icon = icon("clipboard-question")),
                                                 menuItem("G. tabbladen", tabName = "G_tab", icon = icon("clipboard-question")),
                                                 menuItem("E. tabbladen", tabName = "E_tab", icon = icon("clipboard-question")),
                                                 menuItem("X. tabbladen", tabName = "X_tab", icon = icon("clipboard-question")),
                                                 menuItem("Overige tabbladen", tabName = "overige_tab", icon = icon("clipboard-question")),
                                                 
                                                 uiOutput("logo", style = "background-color: white;"),
                                                 h5("version 0.0.9", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;
                                                    position: relative; left: 30px;")
                                     ) # closing sidebarMenu()
                    ), # closing dashboardSidebar()
                    
                    ## Body ----
                    dashboardBody(
                      
                      ## Set up Shinyjs ----
                      useShinyjs(),
                      
                      tabItems(
                        
                        tabItem(tabName = "settings_tab", # Settings tab ----
                                
                                ## Role selection ----
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
                                ),
                                
                                ## Custom inputs
                                div(style = "display: inline-block; vertical-align:top; width: 150px;",
                                    selectInput(inputId = "custom_years", label = "", #multiple = TRUE,
                                                choices = c("Choose year" = "", 
                                                            (as.integer(substr(Sys.time(), 1, 4)) - 5):as.integer(substr(Sys.time(), 1, 4)))
                                    )),
                                
                                ## Input: Load data button
                                div(actionButton(inputId= "load_data_button", label = paste("Load your data")))
                                
                        ), # closing Settings tabItem()
                        
                        tabItem(tabName = "data_tab", # Data tab ----
                                
                                
                        ), # closing Data tabItem()
                        
                        tabItem(tabName = "JPS_tab", # JPS tab ----
                                
                                ## Loaded JPS: DT ----
                                DTOutput("JPSData")
                                
                        ), # closing JPS tabItem()
                        
                        tabItem(tabName = "visualisations_tab", # Visualisations tab ----
                                
                                h5("Welcome! Soon visualisation functionality will be placed here"),
                                
                                selectInput(inputId = "plot1_x", label = "x-axis", choices = NULL),
                                selectInput(inputId = "plot1_y", label = "y-axis", choices = NULL),
                                
                                plotOutput(outputId = "plot1")
                                
                        ), # closing Visualisations tabItem()
                        
                        tabItem(tabName = "checks_tab", # Checks tab ----
                                
                                ## Input: Excel button
                                h5("To open the dashboard in Excel, click the button below."),
                                actionButton(inputId= "excel_button", label = "Open Excel Dashboard")
                                
                        ), # closing Checks tabItem()
                        
                        tabItem(tabName = "A_tab", # A tab ----
                                
                                ## Navbarpage: A. tabbladen
                                navbarPage("A. Tabbladen",
                                           tabPanel("Plausibiliteit_R"),
                                           tabPanel("Sector_R",
                                                    
                                                    ## Loaded Data: DT ----
                                                    htmlOutput(outputId = "role_txt"), br(),
                                                    
                                                    fluidRow(
                                                      column(4, 
                                                             # Populate "Sector" dropdown directly from SQL 
                                                             selectInput(inputId = "select_sector", label = "Sector", width = "100px",
                                                                         choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                          Database = "HSR_ANA_PRD"), 
                                                                                                paste0("SELECT DISTINCT Sector FROM tbl_SR_Data_Transacties")))
                                                             )
                                                      ),
                                                      
                                                      column(4, 
                                                             # Populate "Transactiesoort" dropdown directly from SQL 
                                                             selectInput(inputId = "select_transactiesoort", label = "Transactiesoort", width = "100px",
                                                                         choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                          Database = "HSR_ANA_PRD"), 
                                                                                                paste0("SELECT DISTINCT Transactiesoort FROM tbl_SR_Data_Transacties")))
                                                             )
                                                      ),
                                                      
                                                      column(4, 
                                                             # Populate "Waarde_type" dropdown directly from SQL 
                                                             selectInput(inputId = "select_waarde_type", label = "Waarde_Type", width = "100px",
                                                                         choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                          Database = "HSR_ANA_PRD"), 
                                                                                                paste0("SELECT DISTINCT Waarde_Type FROM tbl_SR_Data_Transacties")))
                                                             )
                                                      )
                                                    ),
                                                    
                                                    withSpinner(DTOutput("selectedData"), type = 6),
                                                    htmlOutput(outputId = "retrieval_txt")),
                                           
                                           tabPanel("Sector_B"),
                                           tabPanel("Saldi_Sector"),
                                           tabPanel("SaldiOverzicht"),
                                           tabPanel("Sectorverslag"),
                                           tabPanel("Kerncijfers"))
                                
                        ), # closing A tabItem()
                        
                        tabItem(tabName = "B_tab", # B tab ----
                                
                                ## Navbarpage: B. tabbladen
                                navbarPage("B. Tabbladen",
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
                                navbarPage("G. Tabbladen",
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
                                navbarPage("E. Tabbladen",
                                           tabPanel("SVs reeks"),
                                           tabPanel("SV_onderdeel"),
                                           tabPanel("SV correcties"))
                                
                        ), # closing E tabItem()
                        
                        tabItem(tabName = "X_tab", # G tab ----
                                
                                ## Navbarpage: X. tabbladen
                                navbarPage("X. Tabbladen",
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
                                
                        ) # closing overige tabItem()
                        
                      ) # closing tabItems()
                      
                    ) # closing dashboardBody()        
                    
) # closing dashboardPage()