
################################
### TEST Shiny CBS Dashboard ###
### UI version 0.0.1         ###
### YKK - 06-06-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

## Load and / or Install required packages ----
if(!require('shiny')){install.packages('shiny', dep = TRUE)};library('shiny')
if(!require('shinydashboard')){install.packages('shinydashboard', dep = TRUE)};library('shinydashboard')
if(!require('shinyjs')){install.packages('shinyjs', dep = TRUE)};library('shinyjs')
if(!require('RODBC')){install.packages('RODBC', dep = TRUE)};library('RODBC') # Misschien ODBC 
if(!require('DT')){install.packages('DT', dep = TRUE)};library('DT')

## UI ----
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    ## Header ----
                    dashboardHeader(title = "CBS Dashboard"),
                    
                    ## Sidebar ----
                    dashboardSidebar(width = 250,
                                     sidebarMenu(menuItem("Menu"), id = "sidebarmenu",
                                                 menuItem("Settings", tabName = "settings_tab", icon = icon("cog")),
                                                 menuItem("Data", tabName = "data_tab", icon = icon("file-alt")),
                                                 menuItem("Visualisations", tabName = "visualisations_tab", icon = icon("chart-line")),
                                                 menuItem("Intranet", icon = icon("atlas"), href = "https://cbsintranet/default.aspx/"),
                                                 menuItem("Checks", tabName = "checks_tab", icon = icon("book-reader")),
                                                 uiOutput("logo", style = "background-color: white;"),
                                                 h5("version 0.0.1", style = "font-style: normal; letter-spacing: 1px; line-height: 26pt;
                position: absolute; bottom: 0; left: 40px;")
                                                 
                                     ) # closing sidebarMenu()
                    ), # closing dashboardSidebar()
                    
                    ## Body ----
                    dashboardBody(
                      
                      ## Set up Shinyjs ----
                      useShinyjs(),
                      
                      tabItems(
                        
                        tabItem(tabName = "settings_tab", # Settings tab ----
                                
                                ## Role selection ----
                                checkboxGroupInput("selected_role", "Select your role to customise data selection:", inline = TRUE,
                                                   c("Integrator" = "Integrator",
                                                     "DNB expert" = "DNB expert",
                                                     "R expert" = "R expert")
                                ),
                                h5("Your current role is:"),
                                textOutput(outputId = "role_txt"),
                                
                                ## Input: Role button
                                actionButton(inputId= "role_button", label = "Load your data"),
                                
                        ), # closing Settings tabItem()
                        
                        tabItem(tabName = "data_tab", # Data tab ----
                                
                                h5("Here be dragons"),
                                
                                # verbatimTextOutput("selectedData")
                                DTOutput("selectedData")
                                
                        ), # closing Data tabItem()
                        
                        tabItem(tabName = "visualisations_tab", # Visualisations tab ----
                                
                                h5("Welcome! Soon visualisation functionality will be placed here")
                                
                        ), # closing Visualisations tabItem()
                        
                        tabItem(tabName = "checks_tab", # Checks tab ----
                                
                                ## Text Explain: Excel button
                                h5("To open the dashboard in Excel, click the button below."),
                                
                                ## Input: Excel button
                                actionButton(inputId= "excel_button", label = "Open Excel Dashboard")
                                
                        ) # closing Checks tabItem()
                        
                      ) # closing tabItems()
                      
                    ) # closing dashboardBody()        
                    
) # closing dashboardPage()