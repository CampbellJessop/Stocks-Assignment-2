library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)

#shinyWidgetsGallery()

# Read zipped data
stocks <- read_csv("nyse_stocks.csv.zip")

# Convert to `tsibble()`
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

# # 1 stock
# selected_stock <- "AAPL"
# 
# stocks %>%
#   filter(symbol == selected_stock) %>%
#   autoplot(open) +
#   labs(title = selected_stock)
# 
# # Multiple stocks
# selected_stocks <- c("GOOG", "AAPL")
# 
# stocks %>%
#   filter(symbol %in% selected_stocks) %>%
#   autoplot(open)



body <- dashboardBody(
  fluidRow(
    box(
      title = "Stock Time Plot", 
      status = "primary", 
      solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("timePlot", 
                 height = 250)
    ),
    
    box(
      title = "Stock Text Output",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      verbatimTextOutput("summary")
    )
  ),
  
  
  fluidRow(
    box(
      title = "Input #1",
      status = "info",
      solidHeader = TRUE,
      width = 4,
      selectizeInput(inputId="stockselect",
                     label="Select some stocks:",
                     choices=unique(stocks$symbol),
                     multiple=TRUE)
    ),
    box(
      title = "Input #2",
      status = "success",
      solidHeader = TRUE,
      width = 4,
      awesomeRadio(
        inputId = "yvar",
        label = "Select a variable to study:", 
        choices = c("open","volume"),
        selected = "open"
      )
    ),
    box(
      title = "Input #3", 
      status = "danger",
      solidHeader = TRUE,
      width = 4,
      dateInput("Start", label = "Select a starting date:", value = "2010-01-04"),
      dateInput("End", label = "Select an ending date:", value = "2016-12-30")
    )
  ),
  
  fluidRow(
    box(
      width = 4, 
      status = "info",
      solidHeader = TRUE,
      "Select as many stocks as you would like by searching its ticker symbol."
    ),
    box(
      width = 4, 
      status = "success",
      solidHeader = TRUE,
      "'Open' = Opening price point", br(), "'Volume' = Number of shares traded"
    ),
    box(
      width = 4, 
      status = "danger",
      solidHeader = TRUE,
      "Stock information ranges from 1/4/2010 - 12/30/2016"
    )
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Stocks Assignment"),
  dashboardSidebar(disable=TRUE),
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$timePlot <- renderPlot({
    # draw the time plot with selected symbols and y var
    selected_stocks <- input$stockselect
    stocks %>%  
      filter(symbol %in% selected_stocks) %>%
      select(symbol,input$yvar) %>% 
      autoplot() + scale_x_date(limits = as.Date(c(input$Start,input$End)))
  })
  # Summary output of selected stocks per date range 
  output$summary<-renderPrint({
    selected_stocks <- input$stockselect
    stockss<-as.data.frame(stocks)
    x<-subset(stockss,subset=symbol%in%selected_stocks& date>as.Date(input$Start)&date<as.Date(input$End),select = c(symbol,open,volume))
    tapply(x[,input$yvar],x$symbol,summary)
  })}


# Preview the UI in the console
shinyApp(ui = ui, server = server)
