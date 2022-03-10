library(shinydashboard)
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

# 1 stock
selected_stock <- "AAPL"

stocks %>%
  filter(symbol == selected_stock) %>%
  autoplot(open) +
  labs(title = selected_stock)

# Multiple stocks
selected_stocks <- c("GOOG", "AAPL")

stocks %>%
  filter(symbol %in% selected_stocks) %>%
  autoplot(open)

?awesomeRadio()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Group Assignment 2 - Stocks!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     selectizeInput(inputId="stockselect",
                     label="Select some stocks!",
                     choices=unique(stocks$symbol),
                     multiple=TRUE),
     awesomeRadio(
       inputId = "yvar",
       label = "Select a variable to study:", 
       choices = c("open","volume"),
       selected = "open"
     ),
     dateInput("Start", label = h3("Starting Date"), value = "2010-01-01"),
     dateInput("End", label = h3("Ending Date"), value = "2017-01-01")
    
     ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("timePlot"),
      #Show summary of plot
      verbatimTextOutput("summary")
    )
  )
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

# Run the application 
shinyApp(ui = ui, server = server)
