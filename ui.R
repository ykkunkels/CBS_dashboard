
################################
### TEST Shiny CBS Dashboard ###
### UI version 0.0.3         ###
### YKK - 12-06-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

## Load and / or Install required packages ----
if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
if(!require('shinydashboard')){install.packages('shinydashboard', dep = TRUE)};library('shinydashboard')
if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
if(!require('RODBC')){install.packages('RODBC', dep = TRUE)};library('RODBC') # Misschien ODBC 
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
                                                 menuItem("Data", tabName = "data_tab", icon = icon("file-alt")),
                                                 menuItem("Visualisations", tabName = "visualisations_tab", icon = icon("chart-line")),
                                                 menuItem("Intranet", icon = icon("atlas"), href = "https://cbsintranet/default.aspx/"),
                                                 menuItem("Checks", tabName = "checks_tab", icon = icon("book-reader")),
                                                 uiOutput("logo", style = "background-color: white;"),
                                                 h5("version 0.0.3", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;
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
                                selectInput(inputId = "custom_years", label = "Years:", multiple = TRUE,
                                            choices = c("Choose year(s)" = "", 
                                                        (as.integer(substr(Sys.time(), 1, 4)) - 5):as.integer(substr(Sys.time(), 1, 4)))
                                ),
                                
                                ## Input: Load data button
                                actionButton(inputId= "load_data_button", label = paste("Load your data"))
                                
                        ), # closing Settings tabItem()
                        
                        tabItem(tabName = "data_tab", # Data tab ----
                                
                                ## Loaded Data: DT ----
                                htmlOutput(outputId = "role_txt"), br(),
                                withSpinner(DTOutput("selectedData"), type = 6)
                                
                        ), # closing Data tabItem()
                        
                        tabItem(tabName = "visualisations_tab", # Visualisations tab ----
                                
                                h5("Welcome! Soon visualisation functionality will be placed here")
                                
                        ), # closing Visualisations tabItem()
                        
                        tabItem(tabName = "checks_tab", # Checks tab ----
                                
                                ## Input: Excel button
                                h5("To open the dashboard in Excel, click the button below."),
                                actionButton(inputId= "excel_button", label = "Open Excel Dashboard")
                                
                        ) # closing Checks tabItem()
                        
                      ) # closing tabItems()
                      
                    ) # closing dashboardBody()        
                    
) # closing dashboardPage()