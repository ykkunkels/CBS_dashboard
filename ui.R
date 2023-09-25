
########################################
### TEST Shiny CBS Dashboard         ###
### UI version 0.0.24                ###
### YKK - 25-09-2023                 ###
### Change log:                      ###
###  >  Fixed data selection "05+10" ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*###

## Load and / or Install required packages ----
if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
if(!require('shinydashboard')){install.packages('shinydashboard', dep = TRUE)};library('shinydashboard')
if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
if(!require('shinycssloaders')){install.packages('shinycssloaders', dep = TRUE)};library('shinycssloaders')
if(!require('odbc')){install.packages('odbc', dep = TRUE)};library('odbc')
if(!require('DT')){install.packages('DT', dep = TRUE)};library('DT')
if(!require('dplyr')){install.packages('dplyr', dep = TRUE)};library('dplyr')
if(!require('svDialogs')){install.packages('svDialogs', dep = TRUE)};library('svDialogs')
if(!require('slickR')){install.packages('slickR', dep = TRUE)};library('slickR')

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
                                                 
                                                 menuItem("Visualisaties", tabName = "visualisaties_tab", icon = icon("chart-line")),
                                                 menuItem("Help", tabName = "help_tab", icon = icon("info")),
                                                 menuItem("Geef feedback", tabName = "feedback_tab", icon = icon("comment")),
                                                 menuItem("Instellingen", tabName = "instellingen_tab", icon = icon("cog")),
                                                 
                                                 uiOutput("logo", style = "background-color: white;"),
                                                 h5("version 0.0.24", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt; 
                                                    position: relative; left: 30px;")
                                     ) # closing sidebarMenu()
                    ), # closing dashboardSidebar()
                    
                    ## Body ----
                    dashboardBody(useShinyjs(),
                                  
                                  # Setup tabItems            
                                  tabItems(
                                    
                                    tabItem(tabName = "welkom_tab", # welkom tab ----
                                            
                                            div(uiOutput("img_welkom", align = "center"),
                                                
                                                h1(textOutput("welcome_text"), align = "center"),
                                                h1(em("CBS Dashboard in R Shiny"), align = "center", style="color:steelblue")
                                            )
                                            
                                    ), # closing welkom tabItem()
                                    
                                    tabItem(tabName = "JPS_tab", # JPS tab ----
                                            
                                            fluidPage(
                                              
                                              fluidRow(
                                                
                                                column(width = 3, style = "margin-top: 0; margin-bottom: -25px;", h5(strong("Jaar"))),
                                                column(width = 3, style = "margin-top: 0; margin-bottom: -25px;", h5(strong("Status")))
                                                
                                              ),
                                              
                                              # data JPS: Populate drop-down menu's directly from SQL
                                              fluidRow(
                                                column(3, style = "margin-top: 0; margin-bottom: -25px;", 
                                                       selectInput(inputId = "select_JPS_jaar", label = "", width = "100px", 
                                                                   selected = (as.integer(substr(Sys.time(), 1, 4)) - 2),
                                                                   choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001",
                                                                                                    Database = "HSR_ANA_PRD"), 
                                                                                          paste0("SELECT DISTINCT Jaar FROM tbl_SR_JPSReferentie ORDER BY Jaar DESC")))
                                                       )
                                                ),
                                                column(3, style = "margin-top: 0; margin-bottom: -25px;", 
                                                       selectInput(inputId = "select_JPS_status", label = "", width = "100px",
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
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Sector"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Rekening"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("TransactieSoort"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Onderdeel"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Weergave Tabel"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Absoluut / procentueel"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("JPS"))),
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: 0;", h5(strong("Download .CSV")))
                                                           
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_sector", label = "", width = "125px",
                                                                              selected = "S.11",
                                                                              choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                               Database = "HSR_ANA_PRD"), 
                                                                                                     paste0("SELECT DISTINCT Sector FROM tbl_SR_Data_Transacties ORDER BY Sector")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_rekening", label = "", width = "125px",
                                                                              selected = "LT",
                                                                              choices = c(dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                               Database = "HSR_ANA_PRD"), 
                                                                                                     paste0("SELECT DISTINCT Rekening FROM tbl_SR_Data_Transacties")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_transactiesoort", label = "", width = "125px",
                                                                              selected = "all", multiple = TRUE,
                                                                              choices = c("all", dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                                      Database = "HSR_ANA_PRD"), 
                                                                                                            paste0("SELECT DISTINCT Transactiesoort FROM tbl_SR_Data_Transacties ORDER BY Transactiesoort")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_onderdeel", label = "", width = "125px",
                                                                              selected = c("05", "10"), multiple = TRUE,
                                                                              choices = c("all", dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                                                                                                                      Database = "HSR_ANA_PRD"), 
                                                                                                            paste0("SELECT DISTINCT Onderdeel FROM tbl_SR_Data_Transacties ORDER BY Onderdeel")))
                                                                  )
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_A_tabel", label = "", width = "125px",
                                                                              selected = "Standaard",
                                                                              choices = c("Standaard", "Bijstelling", "Q-1/Y-1", "Q-4/Y-1"))
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -25px; margin-bottom: -25px;",
                                                                  selectInput(inputId = "select_absoluut", label = "", width = "125px",
                                                                              selected = "Absoluut",
                                                                              choices = c("Absoluut", "Procentueel"))
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -5px; margin-bottom: -25px;",
                                                                  verbatimTextOutput("code_JPS")
                                                           ),
                                                           
                                                           column(width = 1, style = "margin-top: -5px; margin-bottom: -25px;",
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
                                    
                                    tabItem(tabName = "visualisaties_tab", # visualisaties tab ----
                                            
                                            fluidRow(
                                              column(width = 4, h5(strong("Welcome! Here data can be plotted using the dropdown menu"))),
                                              column(width = 4, h5(strong("Drop year means"))),
                                              column(width = 4, h5(strong("Download plot via download button below")))
                                            ),
                                            
                                            fluidRow(
                                              column(width = 4, selectInput(inputId = "plot1_y", label = "y-axis", choices = NULL)),
                                              column(width = 4, checkboxInput(inputId = "drop_year_means", label = "drop")),
                                              column(width = 4, br(), downloadButton(outputId = "download_plot1", label = "Download"))
                                            ),
                                            
                                            plotOutput(outputId = "plot1", height = "600px")
                                            
                                    ), # closing visualisaties tabItem()
                                    
                                    tabItem(tabName = "help_tab", # feedback tab ----
                                            
                                            h2("Scroll door onderstaande afbeeldingen om de korte handleiding te lezen", align = "center"),
                                            slickROutput("help_gallery", width = "85%"),
                                            div(h3("Bij eventuele problemen, email Yoram K. Kunkels (yk.kunkels@cbs.nl)", 
                                                   style = "position: absolute; bottom: 0; left: 700px;"))
                                            
                                    ), # closing help tabItem()
                                    
                                    tabItem(tabName = "feedback_tab", # feedback tab ----
                                            
                                            h4("Hieronder kunt u uw feedback over dit CBS dashboard achterlaten"),
                                            br(),
                                            textInput("feedback_naam", "Naam (niet verplicht)", width = "800px"),
                                            textAreaInput("feedback_main", "Feedback", width = "800px"),
                                            textAreaInput("feedback_missing", "Dit mis ik nog (niet verplicht)", width = "800px"),
                                            textAreaInput("feedback_errors", "Dit werkt nog niet (niet verplicht)", width = "800px"),
                                            textAreaInput("feedback_good", "Dit vind ik goed (niet verplicht)", width = "800px"),
                                            textAreaInput("feedback_value", "Ik geef het huidige dashboard een (cijfer tussen 1 en 10) (niet verplicht)", width = "800px"),
                                            actionButton("feedback_reset", "Reset"),
                                            actionButton('feedback_send', 'Verzenden')
                                            
                                    ), # closing feedback tabItem()
                                    
                                    tabItem(tabName = "instellingen_tab", # Instellingen tab ----
                                            
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
                                    ) # closing Instellingen tabItem()
                                    
                                  ) # closing tabItems()
                                  
                    ) # closing dashboardBody()        
                    
) # closing dashboardPage()