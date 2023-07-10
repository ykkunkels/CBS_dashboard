library(shiny)
library(shinydashboard)


ui <- fluidPage(
  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
  selectInput("customername", "Customer", choices = NULL),
  selectInput("ordernumber", "Order number", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  
  sales <- vroom::vroom("sales-dashboard/sales_data_sample.csv", col_types = list(), na = "")
  sales %>% 
    select(TERRITORY, CUSTOMERNAME, ORDERNUMBER, everything()) %>%
    arrange(ORDERNUMBER)
  
  
  territory <- reactive({
    filter(sales, TERRITORY == input$territory)
  })
  observeEvent(territory(), {
    choices <- unique(territory()$CUSTOMERNAME)
    updateSelectInput(inputId = "customername", choices = choices) 
  })
  
  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
  })
  observeEvent(customer(), {
    choices <- unique(customer()$ORDERNUMBER)
    updateSelectInput(inputId = "ordernumber", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$ordernumber)
    customer() %>% 
      filter(ORDERNUMBER == input$ordernumber) %>% 
      select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
  })
}
shinyApp(ui, server)